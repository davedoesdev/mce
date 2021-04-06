#include <sys/types.h>
#include <unistd.h>
#include <iostream>
#include <utility>
#include "mce.hpp"
#include "json.hpp"
#include "cxxopts.hpp"

using nlohmann::json;

namespace mce {

const char null_code       = 'a';
const char true_code       = 'b';
const char false_code      = 'c';
const char number_code     = 'd';
const char char_code       = 'e';
const char string_code     = 'f';
const char symbol_code     = 'g';
const char vector_code     = 'h';

const char unmemoized_code = '0';
const char result_code     = '1';
const char step_contn_code = '2';
const char transfer_code   = '3';

symbol::symbol(const std::string& s) : std::string(s) {}

inline boxed box(std::shared_ptr<Runtime> runtime) {
    return std::make_shared<Box>(runtime);
}

template<typename T>
struct box_arg { typedef T type; };

template<typename T>
inline boxed box(const typename box_arg<T>::type& a,
                 std::shared_ptr<Runtime> runtime) {
    return std::make_shared<Box>(a, runtime);
}

template<>
inline boxed box<std::string>(const std::string& s,
                              std::shared_ptr<Runtime> runtime) {
    return box<std::shared_ptr<std::string>>(std::make_shared<std::string>(s), runtime);
}

template<>
struct box_arg<symbol>{ typedef std::string type; };
template<>
inline boxed box<symbol>(const std::string& s,
                         std::shared_ptr<Runtime> runtime) {
    return box<std::shared_ptr<symbol>>(std::make_shared<symbol>(s), runtime);
}

template<>
struct box_type<std::string> { typedef std::shared_ptr<std::string> type; };

template<>
struct box_type<symbol> { typedef std::shared_ptr<symbol> type; };

template<>
struct box_type<vector> { typedef std::shared_ptr<vector> type; };

struct CMapHash {
    std::size_t operator()(const boxed& a) const noexcept {
        if (a->contains<vector>()) {
            return std::hash<std::shared_ptr<vector>>{}(a->cast<vector>());
        }

        if (a->contains<lambda>()) {
            return std::hash<lambda>{}(a->cast<lambda>());
        }

        if (a->contains<double>()) {
            return std::hash<double>{}(a->cast<double>());
        }

        return std::hash<boxed>{}(a);
    }
};

struct CMapEqual {
    bool operator()(const boxed& x, const boxed& y) const noexcept {
        if (x->empty()) {
            return y->empty();
        }

        if (y->empty()) {
            return false;
        }

        if (!x->contains_type_of(*y)) {
            return false;
        }

        if (x->contains<vector>()) {
            return x->cast<vector>() == y->cast<vector>();
        }

        if (x->contains<lambda>()) {
            return x->cast<lambda>() == y->cast<lambda>();
        }

        if (x->contains<double>()) {
            return x->cast<double>() == y->cast<double>();
        }

        return x == y;
    }
};

typedef std::unordered_map<boxed, boxed, CMapHash, CMapEqual> cmap_table;
typedef std::function<boxed(boxed)> map_fn;
typedef std::function<boxed(cmap_table&, boxed, boxed)> set_entry_fn;
typedef boxed extend_rtenv_fn(boxed env, size_t len, boxed values);

boxed make_vector(std::shared_ptr<Runtime> runtime) {
    auto v = std::shared_ptr<vector>(new vector(new std::vector<boxed>()), [runtime](auto vptr) {
        runtime->allocated.vectors.erase(vptr);
        delete vptr;
    });
    runtime->allocated.vectors[v.get()] = v;
    return box<std::shared_ptr<vector>>(v, runtime);
}

boxed vc(boxed first, boxed second) {
    auto a = make_vector(first->get_runtime());
    auto v = a->cast<vector>();
    (*v)->push_back(first);
    (*v)->push_back(second);
    return a;
}

template<>
lambda make_lambda<lambda>(std::function<function> fn,
                           std::shared_ptr<Runtime> runtime,
                           bool has_defn) {
    auto f = std::shared_ptr<func>(new func(new std::function<function>(fn)), [runtime](auto fptr) {
        runtime->allocated.functions.erase(fptr);
        delete fptr;
    });
    runtime->allocated.functions[f.get()] = std::make_pair(has_defn, f);
    return f;
}

template<>
boxed make_lambda<boxed>(std::function<function> fn,
                         std::shared_ptr<Runtime> runtime,
                         bool has_defn) {
    return box<lambda>(make_lambda<lambda>(fn, runtime, has_defn), runtime);
}

void Runtime::break_cycles() {
    // Get all the vectors we have now.
    std::unordered_set<std::shared_ptr<vector>> vs;
    for (auto const& entry : allocated.vectors) {
        auto const& v = entry.second.lock();
        assert(v); // deleter in make_vector should remove unreferenced vectors
        vs.insert(v);
    }

    // Get all the lambdas we have now.
    std::unordered_set<lambda> fs;
    for (auto const& entry : allocated.functions) {
        auto const& f = entry.second.second.lock();
        assert(f); // deleter in make_lambda should remove unreferenced lambdas
        fs.insert(f);
    }

    // Break cycles. This modifies the objects but we've already saved them into a
    // string above which we'll restore later. The other end of the cycle will be
    // pointing to the outer shared pointer so when we reset the inner shared pointer
    // the chain will be broken since the other end won't be keeping the inner
    // object alive any more.
    //
    // e.g. for pairs:
    //
    //            +---------------------------------------+
    //            |                                       |
    //            v                                       |
    //          outer1        +------------>outer2        |
    //   CUT HERE |           |               | CUT HERE  |
    //          inner1        |             inner2        |
    //          /    \        |             /    \        |
    //     inner1a inner1b----+         outer2a outer2b---+
    //
    for (auto const& v : vs) {
        v->reset();
    }
    for (auto const& f : fs) {
        f->reset();
    }
}

void Runtime::add_stats(std::vector<std::vector<size_t>>& stats) {
    if (gc_callback) {
        stats.emplace_back(std::vector<size_t> {
            allocated.vectors.size(),
            allocated.functions.size()
        });
    }
}

std::string mce_save(boxed exp);
boxed mce_restore(const std::string& s, std::shared_ptr<Runtime> runtime);

void Runtime::maybe_gc() {
    if (((allocated.vectors.size() > gc_threshold) ||
         (allocated.functions.size() > gc_threshold)) &&
        !calling_gc_callback) {
        std::vector<std::vector<size_t>> stats;
        add_stats(stats);
        // First we need to find which objects are still pointed to.
        // We only consider vectors and lambdas because they're the only
        // object types that can form loops. The other types can be left to
        // shared pointers only.

        // Make a big vector which will contain every object boxed.
        auto saved_allocations = std::shared_ptr<vector>(new vector(new std::vector<boxed>()));
        // Make a map which will remember where each object is in the boxed vector.
        std::unordered_map<void*, size_t> saved_indices;

        // For each vector, add it to the boxed vector and remember its position.
        for (auto const& entry : allocated.vectors) {
            auto const& v = entry.second.lock();
            assert(v); // deleter in make_vector should remove unreferenced vectors
            saved_indices[v.get()] = (*saved_allocations)->size();
            (*saved_allocations)->push_back(box<std::shared_ptr<vector>>(v, shared_from_this()));
        }

        // For each lambda, add it to the boxed vector and remember its position.
        for (auto const& entry : allocated.functions) {
            auto const& f = entry.second.second.lock();
            assert(f); // deleter in make_lambda should remove unreferenced lambdas
            if (entry.second.first) {
                saved_indices[f.get()] = (*saved_allocations)->size();
                (*saved_allocations)->push_back(box<lambda>(f, shared_from_this()));
            }
        }

        // Save the boxed vector into a string.
        // Note: This process itself will have created more objects with cycles but
        // they will all be destroyed when we break cycles since we know there will
        // be nowhere else referencing them.
        auto saved = mce_save(box<std::shared_ptr<vector>>(saved_allocations, shared_from_this()));
        // We don't need the boxed vector any more.
        saved_allocations.reset();

        // Break cycles in objects.
        break_cycles();

        add_stats(stats);

        // Remember which objects are still live.
        std::unordered_set<std::shared_ptr<vector>> rvs;
        for (auto const& entry : allocated.vectors) {
            auto const& v = entry.second.lock();
            assert(v);
            rvs.insert(v);
        }
        std::unordered_set<lambda> rfs;
        for (auto const& entry : allocated.functions) {
            auto const& f = entry.second.second.lock();
            assert(f);
            rfs.insert(f);
        }

        // Now we restore the boxed vector. Note this will also have all the cycles restored.
        auto restored = mce_restore(saved, shared_from_this())->cast<vector>();

        // Make another boxed vector.
        saved_allocations = std::shared_ptr<vector>(new vector(new std::vector<boxed>()));
        // And another map of indices.
        std::unordered_map<void*, size_t> saved_indices2;

        // Go through the objects we remembered were live and add their corresponding object
        // from the restored vector to the new boxed vector.
        for (auto& v : rvs) {
            saved_indices2[v.get()] = (*saved_allocations)->size();
            (*saved_allocations)->push_back(
                (*restored)->at(saved_indices.at(v.get())));
        }
        for (auto& f : rfs) {
            saved_indices2[f.get()] = (*saved_allocations)->size();
            (*saved_allocations)->push_back(
                (*restored)->at(saved_indices.at(f.get())));
        }
        restored.reset();
        saved_indices.clear();

        // Save the new boxed vector into a string.
        saved = mce_save(box<std::shared_ptr<vector>>(saved_allocations, shared_from_this()));
        saved_allocations.reset();

        // Break the cycles again.
        break_cycles();

        // Restore again. This will only have what we remembered was live.
        restored = mce_restore(saved, shared_from_this())->cast<vector>();

        // Now we can fix up the live objects.
        for (auto& v : rvs) {
            *v = *(*restored)->at(saved_indices2.at(v.get()))->cast<vector>();
        }
        for (auto& f : rfs) {
            *f = *(*restored)->at(saved_indices2.at(f.get()))->cast<lambda>();
        }
        rvs.clear();
        rfs.clear();
        restored.reset();
        saved_indices2.clear();

        add_stats(stats);

        if (gc_callback && gc_callback->contains<lambda>()) {
            auto a = make_vector(shared_from_this());
            auto v = a->cast<vector>();
            for (auto s : stats) {
                auto b = make_vector(shared_from_this());
                auto w = b->cast<vector>();
                for (auto t : s) {
                    (*w)->push_back(box<double>(t, shared_from_this()));
                }
                (*v)->push_back(b);
            }
            calling_gc_callback = true;
            std::unique_ptr<bool, void(*)(bool*)> cleanup(&calling_gc_callback, [](bool *p) {
                *p = false;
            });
            auto r = (**gc_callback->cast<lambda>())(vc(a, box(shared_from_this())));
            assert(!r->contains<bool>() || r->cast<bool>());
        }
    }
}

boxed vlist_ref(boxed vl, size_t i) {
    while (i > 0) {
        vl = (*vl->cast<vector>())->at(1);
        --i;
    }

    return (*vl->cast<vector>())->at(0);
}

boxed vlist_rest(boxed vl, size_t i) {
    while (i > 0) {
        vl = (*vl->cast<vector>())->at(1);
        --i;
    }

    return (*vl->cast<vector>())->at(1);
}

boxed vlist_to_vector(boxed vl) {
    auto a = make_vector(vl->get_runtime());
    auto v = a->cast<vector>();

    while (vl->contains<vector>()) {
        auto v2 = vl->cast<vector>();
        (*v)->push_back((*v2)->at(0));
        vl = (*v2)->at(1);
    }

    return a;
}

boxed vector_cmap(map_fn f, boxed v, cmap_table& tab, set_entry_fn set_entry) {
    auto ref = tab.find(v);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto entry = set_entry(tab, v, make_vector(v->get_runtime()));
    auto ev = entry->cast<vector>();
    auto vv = v->cast<vector>();

    for (auto const& el : **vv) {
        (*ev)->push_back(f(el));
    }

    return entry;
}

boxed table_set(cmap_table& tab, boxed v, boxed entry) {
    tab[v] = entry;
    return entry;
}

bool is_yield_defn(boxed args) {
    if (!args->contains<vector>() || (*args->cast<vector>())->empty()) {
        return false;
    }
    auto first = vlist_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-YIELD-DEFINITION");
}

boxed get_procedure_defn(boxed proc) {
    auto runtime = proc->get_runtime();
    return (**proc->cast<lambda>())(
        vc(box<symbol>("MCE-YIELD-DEFINITION", runtime), box(runtime)));
}

boxed xdisplay(boxed exp, std::ostream& out, bool is_write) {
    if (exp->empty()) {
        out << "()";
    } else {
        if (exp->contains<bool>()) {
            out << (exp->cast<bool>() ? "#t" : "#f");
        } else if (exp->contains<double>()) {
            out << exp->cast<double>();
        } else if (exp->contains<char>()) {
            auto c = exp->cast<char>();
            if (is_write) {
                out << "#\\x" << std::hex << static_cast<int>(c);
            } else {
                out << c;
            }
        } else if (exp->contains<std::string>()) {
            auto s = exp->cast<std::string>();
            if (is_write) {
                json j = *s;
                out << j.dump();
            } else {
                out << *s;
            }
        } else if (exp->contains<symbol>()) {
            out << *exp->cast<symbol>();
        } else if (exp->contains<vector>()) {
            auto bnil = box(exp->get_runtime());
            bool first = true;
            out << "#(";
            auto vec = exp->cast<vector>();
            for (auto const& v : **vec) {
                if (!first) {
                    out << " ";
                }
                first = false;
                xdisplay(v, out, is_write);
            }
            out << ")";
        } else if (exp->contains<lambda>()) {
            out << "#<procedure>";
        } else {
            throw std::range_error("unknown display expression");
        }
    }
    return exp;
}

boxed xdisplay_args(boxed args, std::ostream& out, bool is_write) {
    auto bnil = box(args->get_runtime());
    auto r = bnil;
    while (args->contains<vector>()) {
        auto v = args->cast<vector>();
        r = xdisplay((*v)->at(0), out, is_write);
        args = (*v)->at(1);
    }
    if (!is_write) {
        out << std::endl;
    }
    return r;
}

boxed print(boxed args) {
    return xdisplay_args(args, std::cout, false);
}

boxed eprint(boxed args) {
    return xdisplay_args(args, std::cerr, false);
}

boxed write(boxed args) {
    return xdisplay_args(args, std::cout, true);
}

boxed ewrite(boxed args) {
    return xdisplay_args(args, std::cerr, true);
}

boxed unmemoize(boxed exp);
boxed serialize(boxed exp);

boxed memoize_lambda(lambda proc, boxed defn) {
    return make_lambda<boxed>([proc, defn](boxed args) -> boxed {
        //print(cons(serialize(unmemoize(args)), box(args->get_runtime())));
        if (is_yield_defn(args)) {
            if (defn->contains<lambda>()) {
                return (**defn->cast<lambda>())(box(args->get_runtime()));
            }
            return defn;
        }
        //print(cons(serialize(unmemoize(defn)), box(defn->get_runtime())));
        return (**proc)(args);
    }, defn->get_runtime(), true);
}

boxed memoize_lambda(boxed proc, boxed defn) {
    return memoize_lambda(proc->cast<lambda>(), defn);
}

boxed send(boxed k, boxed args) {
    return vc(k, args);
}

boxed sendv(boxed k, boxed v) {
    return send(k, vc(v, box(v->get_runtime())));
}

boxed rtenv_lookup(boxed i, boxed env) {
    auto iv = i->cast<vector>();
    auto first = (*iv)->at(0)->cast<double>();
    auto second = (*iv)->at(1)->cast<double>();
    auto v = vlist_ref(env, first)->cast<vector>();
    if (second < (*v)->size()) {
        return (**v)[second];
    }
    return box(i->get_runtime());
}

boxed rtenv_setvar(boxed i, boxed val, boxed env) {
    auto iv = i->cast<vector>();
    auto first = (*iv)->at(0)->cast<double>();
    auto second = (*iv)->at(1)->cast<double>();
    auto v = vlist_ref(env, first)->cast<vector>();
    if (second >= (*v)->size()) {
        (*v)->resize(second + 1, box(i->get_runtime()));
    }
    (**v)[second] = val;
    return val;
}

boxed symbol_lookup(boxed args) {
    auto i = vlist_ref(args, 1);
    return make_lambda<boxed>([i](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        return sendv(k, rtenv_lookup(i, env));
    }, args->get_runtime());
}

boxed send_value(boxed args) {
    auto exp = vlist_ref(args, 1);
    return make_lambda<boxed>([exp](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        return sendv(k, exp);
    }, args->get_runtime());
}

boxed make_step_contn(boxed k, boxed env, boxed args) {
    return vc(box<symbol>("MCE-STEP-CONTN", k->get_runtime()), vc(k, vc(env, args)));
}

bool is_step_contn(boxed args) {
    if (!args->contains<vector>() || (*args->cast<vector>())->empty()) {
        return false;
    }
    auto first = vlist_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-STEP-CONTN");
}

boxed step_contn_k(boxed args) {
    return vlist_ref(args, 1);
}

boxed step_contn_env(boxed args) {
    return vlist_ref(args, 2);
}

boxed step_contn_args(boxed args) {
    return vlist_rest(args, 2);
}

bool is_transfer(boxed args) {
    if (!args->contains<vector>() || (*args->cast<vector>())->empty()) {
        return false;
    }
    auto first = vlist_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-TRANSFER");
}

boxed transfer_args(boxed args) {
    return vlist_rest(args, 0);
}

boxed transfer(boxed args) {
    return send(vlist_ref(args, 2),
        vc(box<symbol>("MCE-TRANSFER", args->get_runtime()), vlist_rest(args, 2)));
}

boxed make_global_rtenv(std::shared_ptr<Runtime> runtime) {
    return vc(make_vector(runtime), box(runtime));
}

boxed applyx(boxed k, boxed env, boxed fn, boxed args) {
    return send(fn, make_step_contn(k, env, args));
}

boxed result(boxed args) {
    auto runtime = args->get_runtime();
    auto a = make_vector(runtime);
    auto v = a->cast<vector>();
    (*v)->push_back(box<symbol>("MCE-RESULT", runtime));
    (*v)->push_back(vlist_ref(args, 0));
    return a;
}

bool is_result(boxed exp) {
    if (!exp->contains<vector>()) {
        return false;
    }
    auto v = exp->cast<vector>();
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<symbol>() &&
           (*(**v)[0]->cast<symbol>() == "MCE-RESULT");
}

boxed result_val(boxed exp) {
    return (**exp->cast<vector>())[1];
}

boxed less_than(boxed args) {
    return box<bool>(vlist_ref(args, 0)->cast<double>() <
                     vlist_ref(args, 1)->cast<double>(),
                     args->get_runtime());
}

boxed greater_than(boxed args) {
    return box<bool>(vlist_ref(args, 0)->cast<double>() >
                     vlist_ref(args, 1)->cast<double>(),
                     args->get_runtime());
}

boxed plus(boxed args) {
    double r = 0;
    while (args->contains<vector>()) {
        auto v = args->cast<vector>();
        r += (*v)->at(0)->cast<double>();
        args = (*v)->at(1);
    }
    return box<double>(r, args->get_runtime());
}

boxed minus(boxed args) {
    auto v = args->cast<vector>();
    double n = (*v)->at(0)->cast<double>();
    if (!(*v)->at(1)->contains<vector>()) {
        return box<double>(-n, args->get_runtime());
    }
    while ((args = (*v)->at(1))->contains<vector>()) {
        v = args->cast<vector>();
        n -= (*v)->at(0)->cast<double>();
    }
    return box<double>(n, args->get_runtime());
}

boxed multiply(boxed args) {
    double r = 1;
    while (args->contains<vector>()) {
        auto v = args->cast<vector>();
        r *= (*v)->at(0)->cast<double>();
        args = (*v)->at(1);
    }
    return box<double>(r, args->get_runtime());
}

boxed divide(boxed args) {
    auto v = args->cast<vector>();
    double n = (*v)->at(0)->cast<double>();
    if (!(*v)->at(1)->contains<vector>()) {
        return box<double>(1 / n, args->get_runtime());
    }
    while ((args = (*v)->at(1))->contains<vector>()) {
        v = args->cast<vector>();
        n /= (*v)->at(0)->cast<double>();
    }
    return box<double>(n, args->get_runtime());
}

boxed is_null(boxed args) {
    return box<bool>(vlist_ref(args, 0)->empty(), args->get_runtime());
}

boxed is_string(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<std::string>(), args->get_runtime());
}

boxed is_procedure(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<lambda>(), args->get_runtime());
}

boxed make_vector(boxed args) {
    auto n = vlist_ref(args, 0)->cast<double>();
    auto runtime = args->get_runtime();
    auto a = make_vector(runtime);
    auto v = a->cast<vector>();
    (*v)->resize(n, box(runtime));
    return a;
}

boxed is_vector(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<vector>(), args->get_runtime());
}

boxed vector_length(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<double>(static_cast<double>((*a->cast<vector>())->size()),
                       args->get_runtime());
}

boxed vector_ref(boxed args) {
    auto a = vlist_ref(args, 0);
    auto i = vlist_ref(args, 1);
    return (*a->cast<vector>())->at(i->cast<double>());
}

boxed vector_set(boxed args) {
    auto a = vlist_ref(args, 0);
    auto i = vlist_ref(args, 1);
    auto exp = vlist_ref(args, 2);
    (*a->cast<vector>())->at(i->cast<double>()) = exp;
    return box(args->get_runtime());
}

boxed is_string_equal(boxed args) {
    auto x = vlist_ref(args, 0);
    auto y = vlist_ref(args, 1);
    return box<bool>(*x->cast<std::string>() == *y->cast<std::string>(),
                     args->get_runtime());
}

boxed is_number_equal(boxed args) {
    auto x = vlist_ref(args, 0);
    auto y = vlist_ref(args, 1);
    return box<bool>(x->cast<double>() == y->cast<double>(),
                     args->get_runtime());
}

boxed is_eq(boxed args) {
    auto x = vlist_ref(args, 0);
    auto y = vlist_ref(args, 1);

    auto runtime = args->get_runtime();

    if (x->empty()) {
        return box<bool>(y->empty(), runtime);
    }

    if (y->empty()) {
        return box<bool>(false, runtime);
    }

    if (!x->contains_type_of(*y)) {
        return box<bool>(false, runtime);
    }

    if (x->contains<bool>()) {
        return box<bool>(x->cast<bool>() == y->cast<bool>(), runtime);
    }

    if (x->contains<char>()) {
        return box<bool>(x->cast<char>() == y->cast<char>(), runtime);
    }

    if (x->contains<double>()) {
        return box<bool>(x->cast<double>() == y->cast<double>(), runtime);
    }

    if (x->contains<std::string>()) {
        return box<bool>(
            x->cast<std::string>() == y->cast<std::string>(), runtime);
    }

    if (x->contains<symbol>()) {
        return box<bool>(*x->cast<symbol>() == *y->cast<symbol>(), runtime);
    }

    if (x->contains<vector>()) {
        return box<bool>(x->cast<vector>() == y->cast<vector>(), runtime);
    }

    if (x->contains<lambda>()) {
        return box<bool>(x->cast<lambda>() == y->cast<lambda>(), runtime);
    }

    return box<bool>(false, runtime);
}

boxed gapplyx(boxed args) {
    return applyx(vlist_ref(args, 0),
                  vlist_ref(args, 1),
                  vlist_ref(args, 2),
                  vlist_ref(args, 3));
}

boxed save(boxed args) {
    return box<std::string>(mce_save(vlist_ref(args, 0)), args->get_runtime());
}

boxed restore(boxed args) {
    return sendv(vlist_ref(args, 0),
                 mce_restore(*vlist_ref(args, 2)->cast<std::string>(),
                             args->get_runtime()));
}

boxed getpid(boxed args) {
    return box<double>(static_cast<double>(::getpid()), args->get_runtime());
}

boxed Runtime::get_config(const std::string& k) {
    auto it = config_table.find(k);
    if (it == config_table.end()) {
        return box<bool>(false, shared_from_this());
    }
    return it->second;
}

boxed get_config(boxed args) {
    return args->get_runtime()->get_config(*vlist_ref(args, 0)->cast<std::string>());
}

void Runtime::set_config(const std::string& k, boxed v) {
    config_table[k] = v;
}

void Runtime::set_gc_callback(boxed v) {
    gc_callback = v;
}

boxed set_gc_callback(boxed args) {
    auto runtime = args->get_runtime();
    runtime->set_gc_callback(vlist_ref(args, 0));
    return box(runtime);
}

function* Runtime::get_global_function(const std::string& name) {
    auto it = global_table.find(name);
    if (it == global_table.end()) {
        return nullptr;
    }
    return it->second;
}

void Runtime::register_global_function(const std::string& name, function f) {
    global_table[name] = f;
}

void Runtime::unregister_global_function(const std::string& name) {
    global_table.erase(name);
}

void Runtime::register_kenv_function(function f) {
    kenvfn_set.insert(f);
}

boxed find_global(const symbol& sym, std::shared_ptr<Runtime> runtime) {
    auto it = runtime->global_table.find(sym);
    if (it == runtime->global_table.end()) {
        // out_of_range doesn't show sym when serialized
        throw std::range_error(sym);
    }
    return make_lambda<boxed>(it->second, runtime);
}

boxed find_global(const double i, std::shared_ptr<Runtime> runtime) {
    const auto f = runtime->core_globals.at(i);
    if (!f) {
        throw std::range_error(std::to_string(i));
    }
    return make_lambda<boxed>(f, runtime);
}

boxed find_global(boxed b) {
    if (b->contains<double>()) {
        return find_global(b->cast<double>(), b->get_runtime());
    }
    return find_global(*b->cast<symbol>(), b->get_runtime());
}

boxed step(boxed state) {
    return (**vlist_ref(state, 0)->cast<lambda>())(vlist_rest(state, 0));
}

boxed run(boxed state) {
    auto runtime = state->get_runtime();

    while (!is_result(state)) {
        state = step(state);
        runtime->maybe_gc();
    }

    return result_val(state);
}

boxed globalize(boxed x, boxed args, boxed cf);

boxed handle_global_lambda(boxed args, boxed fn, boxed cf) {
    auto f = fn->cast<lambda>();

    if (is_transfer(args)) {
        return (**f)(transfer_args(args));
    }

    if (is_step_contn(args)) {
        auto sck = step_contn_k(args);
        auto sca = step_contn_args(args);
        return sendv(sck, globalize((**f)(sca), sca, cf));
    }

    return globalize((**f)(args), args, cf);
}

boxed lookup_global(const double i, std::shared_ptr<Runtime> runtime);

boxed handle_global_lambda_kenv(boxed args, boxed fn) {
    if (is_step_contn(args)) {
        return send(fn, vc(step_contn_k(args),
                           vc(step_contn_env(args),
                              step_contn_args(args))));
    }

    auto runtime = args->get_runtime();
    return run(send(fn, vc(lookup_global(runtime->g_result, runtime),
                           vc(make_global_rtenv(runtime),
                              args))));
}

boxed wrap_global_lambda(boxed fn, boxed cf) {
    const lambda l = fn->cast<lambda>();
    auto p = (*l)->target<function*>();
    auto runtime = fn->get_runtime();

    if (p && (runtime->kenvfn_set.find(*p) != runtime->kenvfn_set.end())) {
        return make_lambda<boxed>([fn](boxed args) -> boxed {
            return handle_global_lambda_kenv(args, fn);
        }, runtime);
    }

    return make_lambda<boxed>([fn, cf](boxed args) -> boxed {
        return handle_global_lambda(args, fn, cf);
    }, runtime);
}

boxed extend_rtenv(boxed env, size_t, boxed values) {
    return vc(vlist_to_vector(values), env);
}

boxed improper_extend_rtenv(boxed env, size_t len, boxed values) {
    auto runtime = env->get_runtime();
    auto bnil = box(runtime);
    auto av = make_vector(runtime);
    auto v = av->cast<vector>();
    for (size_t i = 0; (i < len) && !values->empty(); ++i) {
        if (i == (len - 1)) {
            (*v)->push_back(values);
        } else {
            (*v)->push_back(vlist_ref(values, 0));
            values = vlist_rest(values, 0);
        }
    }
    return vc(av, env);
}

boxed handle_lambda(boxed args,
                    size_t len,
                    boxed fn,
                    boxed env,
                    extend_rtenv_fn extend_rtenv) {
    auto runtime = args->get_runtime();
    auto bnil = box(runtime);

    if (is_step_contn(args)) {
        return send(fn, vc(step_contn_k(args),
                           vc(extend_rtenv(env, len, step_contn_args(args)),
                              bnil)));
    }

    return run(send(fn, vc(lookup_global(runtime->g_result, runtime),
                           vc(extend_rtenv(env, len, args),
                              bnil))));
}

boxed handle_contn_lambda(boxed args, boxed k) {
    if (is_transfer(args)) {
        return send(k, transfer_args(args));
    }

    if (is_step_contn(args)) {
        return send(k, step_contn_args(args));
    }

    return run(send(k, args));
}

boxed global_lambda(boxed args) {
    auto self = vlist_ref(args, 0);
    auto defn = vlist_ref(args, 1);
    return wrap_global_lambda(find_global(defn), self);
}

boxed evalx_initial(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto scanned = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, scanned](boxed) -> boxed {
        return send(scanned, vc(k, vc(env, box(k->get_runtime()))));
    }, args->get_runtime());
}

boxed constructed_function0(boxed args);
boxed constructed_function1(boxed args);
boxed if0(boxed args);
boxed if1(boxed args);
boxed sclis0(boxed args);
boxed sclis1(boxed args);
boxed sclis2(boxed args);
boxed scseq0(boxed args);
boxed scseq1(boxed args);
boxed lambda0(boxed args);
boxed lambda1(boxed args);
boxed improper_lambda0(boxed args);
boxed improper_lambda1(boxed args);
boxed letcc0(boxed args);
boxed letcc1(boxed args);
boxed define0(boxed args);
boxed define1(boxed args);
boxed application0(boxed args);
boxed application1(boxed args);

#define define_forms(...) \
std::vector<function*> forms { \
    __VA_ARGS__ \
}; \
enum class forms { \
    __VA_ARGS__ \
};

define_forms(
    symbol_lookup,
    send_value,
    constructed_function0,
    constructed_function1,
    global_lambda,
    if0,
    if1,
    sclis0,
    sclis1,
    sclis2,
    scseq0,
    scseq1,
    lambda0,
    lambda1,
    improper_lambda0,
    improper_lambda1,
    letcc0,
    letcc1,
    define0,
    define1,
    application0,
    application1,
    evalx_initial
)

boxed make_form(boxed n, boxed args) {
    auto defn = vc(n, args);
    auto runtime = args->get_runtime();

    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(forms[n->cast<double>()](vc(f2, args)), defn);

    return f;
}

boxed make_form(enum forms n, boxed args) {
    return make_form(box<double>(static_cast<double>(n), args->get_runtime()), args);
}

boxed make_form(boxed args) {
    auto vl = args->cast<vector>();
    return make_form((*vl)->at(0), (*vl)->at(1));
}

boxed aform(enum forms n, std::shared_ptr<Runtime> runtime) {
    return box<double>(static_cast<double>(n), runtime);
}

boxed lookup_global(const double i, std::shared_ptr<Runtime> runtime) {
    auto r = find_global(i, runtime);
    auto defn = vc(aform(forms::global_lambda, runtime),
                   vc(box<double>(i, runtime), box(runtime)));
    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

boxed globalize(boxed x, boxed args, boxed cf) {
    if (!x->contains<lambda>()) {
        return x;
    }

    auto runtime = x->get_runtime();
    auto defn = vc(aform(forms::constructed_function0, runtime),
                   vc(args, vc(cf, box(runtime))));
    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

boxed constructed_function0(boxed args) {
    auto args2 = vlist_ref(args, 1);
    auto cf = vlist_ref(args, 2);
    return make_lambda<boxed>([args2, cf](boxed args3) -> boxed {
        return applyx(make_form(forms::constructed_function1, vc(args3, box(args3->get_runtime()))),
                      make_global_rtenv(args3->get_runtime()), cf, args2);
    }, args->get_runtime());
}

boxed constructed_function1(boxed args) {
    auto args2 = vlist_ref(args, 1);
    return make_lambda<boxed>([args2](boxed args3) -> boxed {
        auto f = vlist_ref(args3, 0);
        return send(f, args2);
    }, args->get_runtime());
}

boxed if1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto scan1 = vlist_ref(args, 3);
    auto scan2 = vlist_ref(args, 4);
    return make_lambda<boxed>([k, env, scan1, scan2](boxed args) -> boxed {
        auto v = vlist_ref(args, 0)->cast<bool>();
        auto f = v ? scan1 : scan2;
        return send(f, vc(k, vc(env, box(args->get_runtime()))));
    }, args->get_runtime());
}

boxed if0(boxed args) {
    auto scan0 = vlist_ref(args, 1);
    auto scan1 = vlist_ref(args, 2);
    auto scan2 = vlist_ref(args, 3);
    return make_lambda<boxed>([scan0, scan1, scan2](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scan0,
            vc(make_form(forms::if1,
                         vc(k, vc(env, vc(scan1, vc(scan2, bnil))))),
               vc(env, bnil)));
    }, args->get_runtime());
}

boxed sclis2(boxed args) {
    auto k = vlist_ref(args, 1);
    auto v = vlist_ref(args, 2);
    return make_lambda<boxed>([k, v](boxed args) -> boxed {
        auto w = vlist_ref(args, 0);
        return sendv(k, vc(v, w));
    }, args->get_runtime());
}

boxed sclis1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto rest = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        auto bnil = box(args->get_runtime());
        return send(rest,
            vc(make_form(forms::sclis2, vc(k, vc(v, bnil))),
               vc(env, bnil)));
    }, args->get_runtime());
}

boxed sclis0(boxed args) {
    auto first = vlist_ref(args, 1);
    auto rest = vlist_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(first,
            vc(make_form(forms::sclis1,
                         vc(k, vc(env, vc(rest, bnil)))),
               vc(env, bnil)));
    }, args->get_runtime());
}

boxed scseq1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto rest = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed) -> boxed {
        return send(rest, vc(k, vc(env, box(k->get_runtime()))));
    }, args->get_runtime());
}

boxed scseq0(boxed args) {
    auto first = vlist_ref(args, 1);
    auto rest = vlist_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(first,
            vc(make_form(forms::scseq1,
                         vc(k, vc(env, vc(rest, bnil)))),
               vc(env, bnil)));
    }, args->get_runtime());
}

boxed lambda1(boxed args) {
    auto len = vlist_ref(args, 1)->cast<double>();
    auto scanned = vlist_ref(args, 2);
    auto env = vlist_ref(args, 3);
    return make_lambda<boxed>([len, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, len, scanned, env, extend_rtenv);
    }, args->get_runtime());
}

boxed lambda0(boxed args) {
    auto len = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([len, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return sendv(k,
                     make_form(forms::lambda1,
                               vc(len, vc(scanned, vc(env, bnil)))));
    }, args->get_runtime());
}

boxed improper_lambda1(boxed args) {
    auto len = vlist_ref(args, 1)->cast<double>();
    auto scanned = vlist_ref(args, 2);
    auto env = vlist_ref(args, 3);
    return make_lambda<boxed>([len, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, len, scanned, env, improper_extend_rtenv);
    }, args->get_runtime());
}

boxed improper_lambda0(boxed args) {
    auto len = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([len, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return sendv(k,
                     make_form(forms::improper_lambda1,
                               vc(len, vc(scanned, vc(env, bnil)))));
    }, args->get_runtime());
}

boxed letcc1(boxed args) {
    auto k = vlist_ref(args, 1);
    return make_lambda<boxed>([k](boxed args) -> boxed {
        return handle_contn_lambda(args, k); 
    }, args->get_runtime());
}

boxed letcc0(boxed args) {
    auto scanned = vlist_ref(args, 1);
    return make_lambda<boxed>([scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            vc(k,
               vc(extend_rtenv(env,
                               1,
                               vc(make_form(forms::letcc1, vc(k, bnil)),
                                  bnil)),
                  bnil)));
    }, args->get_runtime());
}

boxed define1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto i = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, i](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        return sendv(k, rtenv_setvar(i, v, env));
    }, args->get_runtime());
}

boxed define0(boxed args) {
    auto i = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([i, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            vc(make_form(forms::define1, vc(k, vc(env, vc(i, bnil)))),
               vc(env, bnil)));
    }, args->get_runtime());
}

boxed application1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    return make_lambda<boxed>([k, env](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        return applyx(k, env, vlist_ref(v, 0), vlist_rest(v, 0));
    }, args->get_runtime());
}

boxed application0(boxed args) {
    auto scanned = vlist_ref(args, 1);
    return make_lambda<boxed>([scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            vc(make_form(forms::application1, vc(k, vc(env, bnil))),
               vc(env, bnil)));
    }, args->get_runtime());
}

bool is_unmemoized(std::shared_ptr<vector> v) {
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<symbol>() &&
           (*(**v)[0]->cast<symbol>() == "MCE-UNMEMOIZED");
}

boxed unmemoized_repexp(std::shared_ptr<vector> v) {
    return (**v)[1];
}

boxed memoize_aux(boxed exp, cmap_table& tab, map_fn fn) {
    if (exp->contains<vector>()) {
        auto v = exp->cast<vector>();
        if (is_unmemoized(v)) {
            auto repexp = unmemoized_repexp(v);
            auto runtime = exp->get_runtime();
            auto r = box(runtime);
            auto f = box(runtime);
            auto entry = table_set(tab, exp, memoize_lambda(
                make_lambda<lambda>([f](boxed args) -> boxed {
                    return (**f->cast<lambda>())(args);
                }, runtime),
                make_lambda<boxed>([r](boxed) -> boxed {
                    return r;
                }, runtime)));
            *r = *fn(repexp);
            *f = Box(make_lambda<lambda>([r, f](boxed args) -> boxed {
                    *f = *make_form(r);
                    return (**f->cast<lambda>())(args);
                }, runtime), runtime);
            return entry;
        }
        return vector_cmap(fn, exp, tab, table_set);
    }

    return exp;
}

boxed memoize(boxed exp) {
    cmap_table tab;
    map_fn fn = [&tab, &fn](boxed x) -> boxed {
        return memoize_aux(x, tab, fn);
    };
    return fn(exp);
}

boxed unmemoize_aux(boxed exp, cmap_table& tab, map_fn fn) {
    if (exp->contains<vector>()) {
        return vector_cmap(fn, exp, tab, table_set);
    }

    if (exp->contains<lambda>()) {
        auto ref = tab.find(exp);
        if (ref != tab.end()) {
            return ref->second;
        }

        auto runtime = exp->get_runtime();
        auto entry = table_set(tab, exp, make_vector(runtime));
        auto v = entry->cast<vector>();

        (*v)->push_back(box<symbol>("MCE-UNMEMOIZED", runtime));
        (*v)->push_back(fn(get_procedure_defn(exp)));

        return entry;
    }

    return exp;
}

boxed unmemoize(boxed exp) {
    cmap_table tab;
    map_fn fn = [&tab, &fn](boxed x) -> boxed {
        return unmemoize_aux(x, tab, fn);
    };
    return fn(exp);
}

boxed make_serialized(size_t n, std::shared_ptr<Runtime> runtime) {
    auto a = make_vector(runtime);
    auto v = a->cast<vector>();

    (*v)->push_back(box<symbol>("MCE-SERIALIZED", runtime));
    (*v)->push_back(box<double>(static_cast<double>(n), runtime));

    return a;
}

bool is_serialized(std::shared_ptr<vector> v) {
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<symbol>() &&
           (*(**v)[0]->cast<symbol>() == "MCE-SERIALIZED");
}

bool is_serialized(boxed exp) {
    return exp->contains<vector>() && is_serialized(exp->cast<vector>());
}

boxed serialized_n(std::shared_ptr<vector> v) {
    return (**v)[1];
}

double serialized_n(boxed exp) {
    return serialized_n(exp->cast<vector>())->cast<double>();
}

boxed serialize_aux(boxed exp,
                    cmap_table& tab,
                    map_fn fn,
                    set_entry_fn set_entry) {
    if (exp->contains<vector>()) {
        return vector_cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

boxed serialize(boxed exp) {
    size_t counter = 0;
    cmap_table tab;
    auto runtime = exp->get_runtime();
    set_entry_fn set_entry = [&counter, runtime](cmap_table& tab, boxed v, boxed entry) {
        table_set(tab, v, make_serialized(counter++, runtime));
        return entry;
    };
    map_fn fn = [&tab, &fn, &set_entry](boxed x) -> boxed {
        return serialize_aux(x, tab, fn, set_entry);
    };
    auto serialized = fn(exp);
    return vc(serialized, box<double>(counter, runtime));
}

boxed deserialize_aux(boxed exp,
                      cmap_table& tab,
                      map_fn fn,
                      set_entry_fn set_entry) {
    if (exp->contains<vector>()) {
        auto v = exp->cast<vector>();
        if (is_serialized(v)) {
            return tab.at(serialized_n(v));
        }
        return vector_cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

boxed deserialize(boxed exp) {
    size_t counter = 0;
    cmap_table tab;
    set_entry_fn set_entry = [&counter](cmap_table& tab, boxed, boxed entry) {
        return table_set(tab,
                         box<double>(static_cast<double>(counter++), entry->get_runtime()),
                         entry);
    };
    map_fn fn = [&tab, &fn, &set_entry](boxed x) -> boxed {
        return deserialize_aux(x, tab, fn, set_entry);
    };
    return fn((*exp->cast<vector>())->at(0));
}

json pickle_aux(boxed exp) {
    json j;
    if (exp->empty()) {
        j.push_back(std::string(1, null_code));
    } else if (exp->contains<bool>()) {
        j.push_back(std::string(1, exp->cast<bool>() ? true_code : false_code));
    } else if (exp->contains<double>()) {
        j.push_back(std::string(1, number_code));
        j.push_back(exp->cast<double>());
    } else if (exp->contains<char>()) {
        j.push_back(std::string(1, char_code));
        j.push_back(std::string(1, exp->cast<char>()));
    } else if (exp->contains<std::string>()) {
        j.push_back(std::string(1, string_code));
        j.push_back(*exp->cast<std::string>());
    } else if (exp->contains<symbol>()) {
        j.push_back(std::string(1, symbol_code));
        j.push_back(*exp->cast<symbol>());
    } else if (exp->contains<vector>()) {
        j.push_back(std::string(1, vector_code));
        auto vec = exp->cast<vector>();
        j.push_back((*vec)->size());
        for (auto const& v : **vec) {
            j.push_back(pickle_aux(v));
        }
    } else {
        throw std::range_error("unknown pickle expression");
    }
    return j;
}

std::string pickle(boxed exp) {
    return pickle_aux(exp).dump();
}

boxed unpickle_aux(const json& j, std::shared_ptr<Runtime> runtime) {
    switch (j[0].get<std::string>()[0]) {
    case true_code:
        return box<bool>(true, runtime);
    case false_code:
        return box<bool>(false, runtime);
    case number_code:
        return box<double>(j[1].get<double>(), runtime);
    case char_code:
        return box<char>(j[1].get<std::string>()[0], runtime);
    case string_code:
        return box<std::string>(j[1].get<std::string>(), runtime);
    case symbol_code:
        return box<symbol>(j[1].get<std::string>(), runtime);
    case vector_code: {
        auto a = make_vector(runtime);
        auto v = a->cast<vector>();
        auto size = j.size();
        (*v)->resize(size - 2);
        for (json::size_type i = 2; i < size; ++i) {
            (*v)->at(i - 2) = unpickle_aux(j[i], runtime);
        }
        return a;
    }
    default:
        return box(runtime);
    }
}

boxed unpickle(const std::string& s, std::shared_ptr<Runtime> runtime) {
    return unpickle_aux(json::parse(s), runtime);
}

std::string mce_save(boxed exp) {
    return pickle(serialize(unmemoize(exp)));
}

boxed mce_restore(const std::string& s, std::shared_ptr<Runtime> runtime) {
    return memoize(deserialize(unpickle(s, runtime)));
}

boxed start(const std::string& s, boxed args) {
    auto runtime = args->get_runtime();
    auto r = mce_restore(s, runtime);
    if (r->contains<lambda>()) {
        return (**r->cast<lambda>())(args);
    }
    return run(r);
}

boxed start(std::istream &stream, boxed args) {
    json j;
    stream >> j;
    return start(j.get<std::string>(), args);
}

// Copy binary representation to vector
template<typename T>
void bpickle_aux(const T& data, std::vector<unsigned char>& v) {
    for (size_t i = 0; i < sizeof(data); ++i) {
        v.push_back(reinterpret_cast<const unsigned char*>(&data)[i]);
    }
}

// Copy index to another given position in a vector
void bpickle_aux(std::vector<unsigned char>& v, size_t pos, uint64_t ref) {
    for (size_t i = 0; i < sizeof(ref); ++i) {
        v[pos + i] = reinterpret_cast<const unsigned char*>(&ref)[i];
    }
}

// Copy current size of vector to another given position in the vector
void bpickle_aux(std::vector<unsigned char>& v, size_t pos) {
    bpickle_aux(v, pos, v.size());
}

void bpickle(boxed exp,
             std::vector<uint64_t>& refs,
             std::vector<unsigned char>& v,
             size_t vec_offset) {
    if (exp->empty()) {
        v.push_back(null_code);
    } else if (exp->contains<bool>()) {
        v.push_back(exp->cast<bool>() ? true_code : false_code);
    } else if (exp->contains<double>()) {
        v.push_back(number_code);
        bpickle_aux(exp->cast<double>(), v);
    } else if (exp->contains<char>()) {
        v.push_back(char_code);
        bpickle_aux(exp->cast<char>(), v);
    } else if (exp->contains<std::string>()) {
        v.push_back(string_code);
        auto s = exp->cast<std::string>();
        bpickle_aux(static_cast<uint64_t>(s->size()), v);
        for (auto const& c : *s) {
            bpickle_aux(c, v);
        }
    } else if (exp->contains<symbol>()) {
        v.push_back(symbol_code);
        auto s = exp->cast<symbol>();
        bpickle_aux(static_cast<uint64_t>(s->size()), v);
        for (auto const& c : *s) {
            bpickle_aux(c, v);
        }
    } else if (exp->contains<vector>()) {
        auto vec = exp->cast<vector>();
        if (vec_offset == 0) {
            refs.push_back(static_cast<uint64_t>(v.size()));
            if (is_unmemoized(vec)) {
                v.push_back(unmemoized_code);
                bpickle_aux(static_cast<uint64_t>(v.size() + 8), v);
                return bpickle(exp, refs, v, 1);
            }
            if (is_result(exp)) {
                v.push_back(result_code);
                bpickle_aux(static_cast<uint64_t>(v.size() + 8), v);
                return bpickle(exp, refs, v, 1);
            }
            if (is_step_contn(exp)) {
                v.push_back(step_contn_code);
                bpickle_aux(static_cast<uint64_t>(v.size() + 8), v);
                return bpickle(exp, refs, v, 1);
            }
            if (is_transfer(exp)) {
                v.push_back(transfer_code);
                bpickle_aux(static_cast<uint64_t>(v.size() + 8), v);
                return bpickle(exp, refs, v, 1);
            }
        }
        v.push_back(vector_code);
        // Add size of vector
        auto size = (*vec)->size();
        assert(vec_offset <= size);
        bpickle_aux(static_cast<uint64_t>(size - vec_offset), v);
        auto pos = v.size();
        for (size_t i = vec_offset; i < size; ++i) {
            // Leave space for 8-byte index of each element.
            // This means they can be found and changed easily.
            bpickle_aux(static_cast<uint64_t>(0), v);
        }
        for (size_t i = vec_offset; i < size; ++i) {
            if (is_serialized((*vec)->at(i))) {
                bpickle_aux(v, pos + (i - vec_offset) * 8, refs[serialized_n((*vec)->at(i))]);
            } else {
                bpickle_aux(v, pos + (i - vec_offset) * 8);
                bpickle((*vec)->at(i), refs, v, 0);
            }
        }
    } else {
        throw std::range_error("unknown bpickle expression");
    }
}

void bconvert_out(const std::string& s, std::shared_ptr<Runtime> runtime) {
    auto exp = serialize(unmemoize(mce_restore(s, runtime)))->cast<vector>();

    std::vector<uint64_t> refs;
    std::vector<unsigned char> v;
    bpickle((*exp)->at(0), refs, v, 0);

    if (refs.size() != (*exp)->at(1)->cast<double>()) {
    std::cerr << refs.size() << " " << (*exp)->at(1)->cast<double>() << std::endl;
        throw std::length_error("unexpected number of references");
    }

    auto size = static_cast<uint64_t>(v.size());
    std::cout.write(reinterpret_cast<const char*>(&size), sizeof(size));
    std::cout.write(reinterpret_cast<const char*>(v.data()), v.size());
}

void bconvert_out(std::istream &stream, std::shared_ptr<Runtime> runtime) {
    json j;
    stream >> j;
    return bconvert_out(j.get<std::string>(), runtime);
}

boxed bunpickle(const unsigned char *s,
                const size_t len,
                const uint64_t i,
                std::unordered_map<uint64_t, boxed>& refs,
                std::shared_ptr<Runtime> runtime);

boxed bunpickle(const unsigned char *s,
                const size_t len,
                const uint64_t i,
                const std::string& marker,
                std::unordered_map<uint64_t, boxed>& refs,
                std::shared_ptr<Runtime> runtime) {
    auto r = make_vector(runtime);
    refs[i] = r;
    auto v = r->cast<vector>();
    (*v)->push_back(box<symbol>(marker, runtime));
    assert(i + 8 < len);
    for (auto el : **bunpickle(s, len, *(uint64_t*)&s[i + 1], refs, runtime)->cast<vector>()) {
        (*v)->push_back(el);
    }
    return r;
}

boxed bunpickle(const unsigned char *s,
                const size_t len,
                const uint64_t i,
                std::unordered_map<uint64_t, boxed>& refs,
                std::shared_ptr<Runtime> runtime) {
    auto ref = refs.find(i);
    if (ref != refs.end()) {
        return ref->second;
    }

    assert(i < len);
    switch (s[i]) {
        case null_code:
            return box(runtime);

        case true_code:
            return box<bool>(true, runtime);

        case false_code:
            return box<bool>(false, runtime);

        case number_code:
            assert(i + sizeof(double) < len);
            return box<double>(*(double*)&s[i + 1], runtime);

        case char_code:
            assert(i + 1 < len);
            return box<char>(s[i + 1], runtime);

        case string_code: {
            assert(i + 8 < len);
            const auto size = *(uint64_t*)&s[i + 1];
            assert(i + 8 + size < len);
            return box<std::string>(std::string(reinterpret_cast<const char*>(&s[i + 9]), size),
                                    runtime);
        }

        case symbol_code: {
            assert(i + 8 < len);
            const auto size = *(uint64_t*)&s[i + 1];
            assert(i + 8 + size < len);
            return box<symbol>(std::string(reinterpret_cast<const char*>(&s[i + 9]), size),
                               runtime);
        }

        case vector_code: {
            auto r = make_vector(runtime);
            refs[i] = r;
            auto v = r->cast<vector>();
            assert(i + 8 < len);
            for (uint64_t j = 0; j < *(uint64_t*)&s[i + 1]; ++j) {
                const auto pos = i + 9 + j * 8;
                assert(pos + 7 < len);
                (*v)->push_back(bunpickle(s, len, *(uint64_t*)&s[pos], refs, runtime));
            }
            return r;    
        }

        case unmemoized_code:
            return bunpickle(s, len, i, "MCE-UNMEMOIZED", refs, runtime);

        case result_code:
            return bunpickle(s, len, i, "MCE-RESULT", refs, runtime);

        case step_contn_code:
            return bunpickle(s, len, i, "MCE-STEP-CONTN", refs, runtime);

        case transfer_code:
            return bunpickle(s, len, i, "MCE-TRANSFER", refs, runtime);

        default:
            throw std::range_error("unknown bunpickle expression");
    }
}

void bconvert_in(const std::string& s, std::shared_ptr<Runtime> runtime) {
    std::unordered_map<uint64_t, boxed> refs;
    json j = mce_save(bunpickle(
        &reinterpret_cast<const unsigned char*>(s.c_str())[8], s.size(), 0, refs, runtime));
    std::cout << j.dump();
}

void bconvert_in(std::istream &stream, std::shared_ptr<Runtime> runtime) {
    uint64_t size;
    stream.read(reinterpret_cast<char*>(&size), sizeof(size));

    std::vector<unsigned char> v(size);
    stream.read(reinterpret_cast<char*>(v.data()), size);

    std::unordered_map<uint64_t, boxed> refs;
    json j = mce_save(bunpickle(v.data(), v.size(), 0, refs, runtime));
    std::cout << j.dump();
}

boxed start(int argc, char *argv[]) {
    cxxopts::Options options("mce", "Metacircular Evaluator");
    options.add_options()
        ("h,help", "help")
        ("gc-threshold",
         "gc when object table exceeds this number",
         cxxopts::value<size_t>()->default_value("100000"))
        ("run",
         "CPS form or state to run",
         cxxopts::value<std::string>())
        ("config",
         "Set configuration",
         cxxopts::value<std::string>())
        ("bconvert-out",
         "Convert CPS form or state to binary format",
         cxxopts::value<std::string>()->implicit_value(""))
        ("bconvert-in",
         "Convert binary format form or state to CPS",
         cxxopts::value<std::string>()->implicit_value(""));
    auto opts = options.parse(argc, argv);
    auto runtime = std::make_shared<Runtime>();
    if (opts.count("help")) {
        std::cout << options.help() << std::endl;
        return box(runtime);
    }
    runtime->set_gc_threshold(opts["gc-threshold"].as<size_t>());
    if (opts.count("config")) {
        auto kv = opts["config"].as<std::string>();
        auto pos = kv.find('=');
        runtime->set_config(kv.substr(0, pos), box<std::string>(kv.substr(pos + 1), runtime));
    }
    if (opts.count("run")) {
        return start(json::parse(opts["run"].as<std::string>()), box(runtime));
    }
    if (opts.count("bconvert-out")) {
        auto state = opts["bconvert-out"].as<std::string>();
        if (state.empty()) {
            bconvert_out(std::cin, runtime);
        } else {
            bconvert_out(state, runtime);
        }
        return box(runtime);
    }
    if (opts.count("bconvert-in")) {
        auto state = opts["bconvert-in"].as<std::string>();
        if (state.empty()) {
            bconvert_in(std::cin, runtime);
        } else {
            bconvert_in(state, runtime);
        }
        return box(runtime);
    }
    return start(std::cin, box(runtime));
}

boxed cf_test(boxed args) {
    auto n = vlist_ref(args, 0)->cast<double>();
    auto x = vlist_ref(args, 1);
    if (n == 0) {
        return make_lambda<boxed>([x](boxed args) {
            return cf_test(vc(vlist_ref(args, 0), vc(x, box(args->get_runtime()))));
        }, args->get_runtime());
    }
    return box<double>(x->cast<double>() + n, args->get_runtime());
}

boxed transfer_test(boxed args) {
    auto runtime = args->get_runtime();
    return applyx(lookup_global(runtime->g_result, runtime),
                  make_global_rtenv(args->get_runtime()),
                  vlist_ref(args, 0),
                  vlist_rest(args, 0));
}

Runtime::Runtime() :
    gc_threshold(10000),
    calling_gc_callback(false),
    global_table {
        { "print", print },
        { "eprint", eprint },
        { "write", write },
        { "write-state", write },
        { "ewrite", write },
        { "save", save },
        { "restore", restore },
        { "getpid", getpid },
        { "get-config", mce::get_config },
        { "cf-test", cf_test },
        { "set-gc-callback!", mce::set_gc_callback }
    },
    core_globals {
        result,
        gapplyx,
        less_than,
        greater_than,
        plus,
        minus,
        multiply,
        divide,
        is_number_equal,
        nullptr,
        is_null,
        make_vector,
        is_vector,
        vector_length,
        vector_ref,
        vector_set,
        is_procedure,
        is_eq,
        is_string,
        is_string_equal,
        transfer,
        transfer_test
    },
    kenvfn_set({
        gapplyx,
        transfer,
        restore
    }),
    g_result(std::find(core_globals.begin(), core_globals.end(), result) -
             core_globals.begin()) {}

void Runtime::set_gc_threshold(size_t v) {
    gc_threshold = v;
}

}
