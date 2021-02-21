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
const char boolean_code    = 'b';
const char number_code     = 'c';
const char char_code       = 'd';
const char string_code     = 'e';
const char symbol_code     = 'f';
const char pair_code       = 'g';
const char vector_code     = 'h';

const char unmemoized_code = '0';
const char redirect_code   = '1';
const char result_code     = '2';
const char step_contn_code = '3';
const char transfer_code   = '4';

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
struct box_type<pair> { typedef std::shared_ptr<pair> type; };

template<>
struct box_type<vector> { typedef std::shared_ptr<vector> type; };

struct CMapHash {
    std::size_t operator()(const boxed& a) const noexcept {
        if (a->contains<pair>()) {
            return std::hash<std::shared_ptr<pair>>{}(a->cast<pair>());
        }

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

        if (x->contains<pair>()) {
            return x->cast<pair>() == y->cast<pair>();
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
typedef boxed extend_env_fn(boxed env, boxed syms, boxed values);

boxed cons(boxed car, boxed cdr) {
    auto runtime = car->get_runtime();
    auto p = std::shared_ptr<pair>(new pair(new std::pair<boxed, boxed>(car, cdr)), [runtime](auto pptr) {
        runtime->allocated.pairs.erase(pptr);
        delete pptr;
    });
    runtime->allocated.pairs[p.get()] = p;
    return box<std::shared_ptr<pair>>(p, runtime);
}

boxed make_vector(std::shared_ptr<Runtime> runtime) {
    auto v = std::shared_ptr<vector>(new vector(new std::vector<boxed>()), [runtime](auto vptr) {
        runtime->allocated.vectors.erase(vptr);
        delete vptr;
    });
    runtime->allocated.vectors[v.get()] = v;
    return box<std::shared_ptr<vector>>(v, runtime);
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
    // Get all the pairs we have now.
    std::unordered_set<std::shared_ptr<pair>> ps;
    for (auto const& entry : allocated.pairs) {
        auto const& p = entry.second.lock();
        assert(p); // deleter in cons should remove unreferenced pairs
        ps.insert(p);
    }

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
    for (auto const& p : ps) {
        p->reset();
    }
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
            allocated.pairs.size(),
            allocated.vectors.size(),
            allocated.functions.size()
        });
    }
}

std::string mce_save(boxed exp);
boxed mce_restore(const std::string& s, std::shared_ptr<Runtime> runtime);

void Runtime::maybe_gc() {
    if ((allocated.pairs.size() > gc_threshold) ||
        (allocated.vectors.size() > gc_threshold) ||
        (allocated.functions.size() > gc_threshold)) {
        std::vector<std::vector<size_t>> stats;
        add_stats(stats);
        // First we need to find which objects are still pointed to.
        // We only consider pairs, vectors and lambdas because they're the only
        // object types that can form loops. The other types can be left to
        // shared pointers only.

        // Make a big vector which will contain every object boxed.
        auto saved_allocations = std::shared_ptr<vector>(new vector(new std::vector<boxed>()));
        // Make a map which will remember where each object is in the boxed vector.
        std::unordered_map<void*, size_t> saved_indices;

        // For each pair, add it to the boxed vector and remember its position.
        for (auto const& entry : allocated.pairs) {
            auto const& p = entry.second.lock();
            assert(p); // deleter in cons should remove unreferenced pairs
            saved_indices[p.get()] = (*saved_allocations)->size();
            (*saved_allocations)->push_back(box<std::shared_ptr<pair>>(p, shared_from_this()));
        }

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
        std::unordered_set<std::shared_ptr<pair>> rps;
        for (auto const& entry : allocated.pairs) {
            auto const& p = entry.second.lock();
            assert(p);
            rps.insert(p);
        }
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
        for (auto& p : rps) {
            saved_indices2[p.get()] = (*saved_allocations)->size();
            (*saved_allocations)->push_back(
                (*restored)->at(saved_indices.at(p.get())));
        }
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
        for (auto& p : rps) {
            *p = *(*restored)->at(saved_indices2.at(p.get()))->cast<pair>();
        }
        for (auto& v : rvs) {
            *v = *(*restored)->at(saved_indices2.at(v.get()))->cast<vector>();
        }
        for (auto& f : rfs) {
            *f = *(*restored)->at(saved_indices2.at(f.get()))->cast<lambda>();
        }
        rps.clear();
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
            auto r = (**gc_callback->cast<lambda>())(cons(a, box(shared_from_this())));
            assert(!r->contains<bool>() || r->cast<bool>());
        }
    }
}

boxed list_ref(boxed l, size_t i) {
    while (i > 0) {
        l = (*l->cast<pair>())->second;
        --i;
    }

    return (*l->cast<pair>())->first;
}

boxed list_rest(boxed l, size_t i) {
    while (i > 0) {
        l = (*l->cast<pair>())->second;
        --i;
    }

    return (*l->cast<pair>())->second;
}

boxed list_to_vector(boxed l) {
    auto a = make_vector(l->get_runtime());
    auto v = a->cast<vector>();

    while (l->contains<pair>()) {
        auto p = l->cast<pair>();
        (*v)->push_back((*p)->first);
        l = (*p)->second;
    }

    return a;
}

boxed vector_to_list(boxed vec) {
    auto v = vec->cast<vector>();
    auto l = box(vec->get_runtime());
    for (auto it = (*v)->rbegin(); it != (*v)->rend(); ++it) {
        l = cons(*it, l);
    }
    return l;
}

boxed cmap(map_fn f, boxed l, cmap_table& tab, set_entry_fn set_entry) {
    auto ref = tab.find(l);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto bnil = box(l->get_runtime());
    auto entry = set_entry(tab, l, cons(bnil, bnil));
    auto ep = entry->cast<pair>();
    auto lp = l->cast<pair>();
    (*ep)->first = f((*lp)->first);
    (*ep)->second = f((*lp)->second);

    return entry;
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
    if (!args->contains<pair>()) {
        return false;
    }
    auto first = list_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-YIELD-DEFINITION");
}

boxed get_procedure_defn(boxed proc) {
    auto runtime = proc->get_runtime();
    return (**proc->cast<lambda>())(
        cons(box<symbol>("MCE-YIELD-DEFINITION", runtime), box(runtime)));
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
        } else if (exp->contains<pair>()) {
            auto bnil = box(exp->get_runtime());
            bool first = true;
            std::cout << "(";
            while (exp->contains<pair>()) {
                if (!first) {
                    out << " ";
                }
                first = false;
                auto p = exp->cast<pair>();
                xdisplay((*p)->first, out, is_write);
                exp = (*p)->second;
            }
            if (!exp->empty()) {
                out << " . ";
                xdisplay(exp, out, is_write);
            }
            out << ")";
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
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r = xdisplay((*p)->first, out, is_write);
        args = (*p)->second;
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
    return cons(k, args);
}

boxed sendv(boxed k, boxed v) {
    return send(k, cons(v, box(v->get_runtime())));
}

boxed ctenv_lookup(boxed i, boxed env) {
    auto ip = i->cast<pair>();
    auto first = (*ip)->first->cast<double>();
    auto second = (*ip)->second->cast<double>();
    auto bindings = list_ref(env, first)->cast<pair>();
    auto v = (*bindings)->second->cast<vector>();
    if (second < (*v)->size()) {
        return (**v)[second];
    }
    return box(i->get_runtime());
}

boxed ctenv_setvar(boxed name, boxed i, boxed val, boxed env) {
    auto ip = i->cast<pair>();
    auto first = (*ip)->first->cast<double>();
    auto second = (*ip)->second->cast<double>();
    auto bindings = list_ref(env, first)->cast<pair>();
    auto v = (*bindings)->second->cast<vector>();
    auto bnil = box(i->get_runtime());

    if (second >= (*v)->size()) {
        (*v)->resize(second + 1, bnil);
    }
    (**v)[second] = val;

    if ((*bindings)->first->empty()) {
        (*bindings)->first = cons(bnil, bnil);
    }
    auto p = (*bindings)->first->cast<pair>();
    while (second > 0) {
        if ((*p)->second->empty()) {
            (*p)->second = cons(bnil, bnil);
        }
        p = (*p)->second->cast<pair>();
        --second;
    }
    (*p)->first = name;

    return val;
}

boxed symbol_lookup(boxed args) {
    auto i = list_ref(args, 1);
    return make_lambda<boxed>([i](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return sendv(k, ctenv_lookup(i, env));
    }, args->get_runtime());
}

boxed send_value(boxed args) {
    auto exp = list_ref(args, 1);
    return make_lambda<boxed>([exp](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        return sendv(k, exp);
    }, args->get_runtime());
}

boxed make_step_contn(boxed k, boxed env, boxed args) {
    return cons(box<symbol>("MCE-STEP-CONTN", k->get_runtime()), cons(k, cons(env, args)));
}

bool is_step_contn(boxed args) {
    if (!args->contains<pair>()) {
        return false;
    }
    auto first = list_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-STEP-CONTN");
}

boxed step_contn_k(boxed args) {
    return list_ref(args, 1);
}

boxed step_contn_env(boxed args) {
    return list_ref(args, 2);
}

boxed step_contn_args(boxed args) {
    return list_rest(args, 2);
}

bool is_transfer(boxed args) {
    if (!args->contains<pair>()) {
        return false;
    }
    auto first = list_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-TRANSFER");
}

boxed transfer_args(boxed args) {
    return list_rest(args, 0);
}

boxed transfer(boxed args) {
    auto fn = list_ref(args, 2);
    return send(fn,
        cons(box<symbol>("MCE-TRANSFER", args->get_runtime()), list_rest(args, 2)));
}

boxed make_global_env(std::shared_ptr<Runtime> runtime) {
    auto values = make_vector(runtime);
    auto bnil = box(runtime);
    auto bindings = cons(bnil, values);
    return cons(bindings, bnil);
}

boxed applyx(boxed k, boxed env, boxed fn, boxed args) {
    return send(fn, make_step_contn(k, env, args));
}

boxed result(boxed exp) {
    auto runtime = exp->get_runtime();
    auto a = make_vector(runtime);
    auto v = a->cast<vector>();
    (*v)->push_back(box<symbol>("MCE-RESULT", runtime));
    (*v)->push_back(list_ref(exp, 0));
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
    return box<bool>(list_ref(args, 0)->cast<double>() <
                     list_ref(args, 1)->cast<double>(),
                     args->get_runtime());
}

boxed greater_than(boxed args) {
    return box<bool>(list_ref(args, 0)->cast<double>() >
                     list_ref(args, 1)->cast<double>(),
                     args->get_runtime());
}

boxed plus(boxed args) {
    double r = 0;
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r += (*p)->first->cast<double>();
        args = (*p)->second;
    }
    return box<double>(r, args->get_runtime());
}

boxed minus(boxed args) {
    auto p = args->cast<pair>();
    double r = (*p)->first->cast<double>();
    while ((args = (*p)->second)->contains<pair>()) {
        p = args->cast<pair>();
        r -= (*p)->first->cast<double>();
    }
    return box<double>(r, args->get_runtime());
}

boxed multiply(boxed args) {
    double r = 1;
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r *= (*p)->first->cast<double>();
        args = (*p)->second;
    }
    return box<double>(r, args->get_runtime());
}

boxed divide(boxed args) {
    auto p = args->cast<pair>();
    double r = (*p)->first->cast<double>();
    while ((args = (*p)->second)->contains<pair>()) {
        p = args->cast<pair>();
        r /= (*p)->first->cast<double>();
    }
    return box<double>(r, args->get_runtime());
}

boxed is_null(boxed args) {
    return box<bool>(list_ref(args, 0)->empty(), args->get_runtime());
}

boxed is_string(boxed args) {
    auto a = list_ref(args, 0);
    return box<bool>(a->contains<std::string>(), args->get_runtime());
}

boxed is_pair(boxed args) {
    auto a = list_ref(args, 0);
    return box<bool>(a->contains<pair>(), args->get_runtime());
}

boxed is_procedure(boxed args) {
    auto a = list_ref(args, 0);
    return box<bool>(a->contains<lambda>(), args->get_runtime());
}

boxed is_vector(boxed args) {
    auto a = list_ref(args, 0);
    return box<bool>(a->contains<vector>(), args->get_runtime());
}

boxed vector_length(boxed args) {
    auto a = list_ref(args, 0);
    return box<double>(static_cast<double>((*a->cast<vector>())->size()),
                       args->get_runtime());
}

boxed vector_ref(boxed args) {
    auto a = list_ref(args, 0);
    auto i = list_ref(args, 1);
    return (*a->cast<vector>())->at(i->cast<double>());
}

boxed is_string_equal(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return box<bool>(*x->cast<std::string>() == *y->cast<std::string>(),
                     args->get_runtime());
}

boxed is_number_equal(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return box<bool>(x->cast<double>() == y->cast<double>(),
                     args->get_runtime());
}

boxed car(boxed args) {
    return list_ref(list_ref(args, 0), 0);
}

boxed cdr(boxed args) {
    return list_rest(list_ref(args, 0), 0);
}

boxed set_car(boxed args) {
    auto p = list_ref(args, 0);
    auto v = list_ref(args, 1);
    (*p->cast<pair>())->first = v;
    return p;
}

boxed set_cdr(boxed args) {
    auto p = list_ref(args, 0);
    auto v = list_ref(args, 1);
    (*p->cast<pair>())->second = v;
    return p;
}

boxed is_eq(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);

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

    if (x->contains<pair>()) {
        return box<bool>(x->cast<pair>() == y->cast<pair>(), runtime);
    }

    if (x->contains<vector>()) {
        return box<bool>(x->cast<vector>() == y->cast<vector>(), runtime);
    }

    if (x->contains<lambda>()) {
        return box<bool>(x->cast<lambda>() == y->cast<lambda>(), runtime);
    }

    return box<bool>(false, runtime);
}

boxed gunmemoize(boxed args) {
    return unmemoize(list_ref(args, 0));
}

boxed gserialize(boxed args) {
    return serialize(list_ref(args, 0));
}

boxed gapplyx(boxed args) {
    return applyx(list_ref(args, 0),
                  list_ref(args, 1),
                  list_ref(args, 2),
                  list_ref(args, 3));
}

boxed save(boxed args) {
    return box<std::string>(mce_save(list_ref(args, 0)), args->get_runtime());
}

boxed restore(boxed args) {
    return sendv(list_ref(args, 0),
                 mce_restore(*list_ref(args, 2)->cast<std::string>(),
                             args->get_runtime()));
}

boxed getpid(boxed args) {
    return box<double>(static_cast<double>(::getpid()), args->get_runtime());
}

boxed gcons(boxed args) {
    return cons(list_ref(args, 0), list_ref(args, 1));
}

boxed Runtime::get_config(const std::string& k) {
    auto it = config_table.find(k);
    if (it == config_table.end()) {
        return box<bool>(false, shared_from_this());
    }
    return it->second;
}

boxed get_config(boxed args) {
    return args->get_runtime()->get_config(*list_ref(args, 0)->cast<std::string>());
}

void Runtime::set_config(const std::string& k, boxed v) {
    config_table[k] = v;
}

void Runtime::set_gc_callback(boxed v) {
    gc_callback = v;
}

boxed set_gc_callback(boxed args) {
    auto runtime = args->get_runtime();
    runtime->set_gc_callback(list_ref(args, 0));
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

boxed step(boxed state) {
    return (**list_ref(state, 0)->cast<lambda>())(list_rest(state, 0));
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

boxed lookup_global(const symbol& sym, std::shared_ptr<Runtime> runtime);

boxed handle_global_lambda_kenv(boxed args, boxed fn) {
    if (is_step_contn(args)) {
        return send(fn, cons(step_contn_k(args),
                             cons(step_contn_env(args),
                                  step_contn_args(args))));
    }

    return run(send(fn, cons(lookup_global(symbol("result"), args->get_runtime()),
                             cons(make_global_env(args->get_runtime()),
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

boxed extend_env(boxed env, boxed syms, boxed values) {
    return cons(cons(syms, list_to_vector(values)), env);
}

boxed improper_extend_env(boxed env, boxed syms, boxed values) {
    auto runtime = env->get_runtime();
    auto bnil = box(runtime);
    auto s = bnil;
    auto ps = s;
    auto av = make_vector(runtime);
    auto v = av->cast<vector>();

    while (!syms->empty() && !values->empty()) {
        if (!syms->contains<pair>()) {
            auto ns = cons(syms, bnil);
            if (s->empty()) {
                s = ns;
            } else {
                (*ps->cast<pair>())->second = ns;
            }
            (*v)->push_back(values);
            break;
        }

        auto ns = cons(list_ref(syms, 0), bnil);
        if (s->empty()) {
            s = ps = ns;
        } else {
            ps = (*ps->cast<pair>())->second = ns;
        }
        (*v)->push_back(list_ref(values, 0));
        
        syms = list_rest(syms, 0);
        values = list_rest(values, 0);
    }

    return cons(cons(s, av), env);
}

boxed handle_lambda(boxed args,
                    boxed params,
                    boxed fn,
                    boxed env,
                    extend_env_fn extend_env) {
    auto runtime = args->get_runtime();
    auto bnil = box(runtime);

    if (is_step_contn(args)) {
        return send(fn, cons(step_contn_k(args),
                             cons(extend_env(env, params, step_contn_args(args)),
                                  bnil)));
    }

    return run(send(fn, cons(lookup_global(symbol("result"), runtime),
                             cons(extend_env(env, params, args),
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
    auto self = list_ref(args, 0);
    auto defn = list_ref(args, 1)->cast<symbol>();
    return wrap_global_lambda(find_global(*defn, args->get_runtime()), self);
}

boxed evalx_initial(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda<boxed>([k, env, scanned](boxed) -> boxed {
        return send(scanned, cons(k, cons(env, box(k->get_runtime()))));
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
    auto defn = cons(n, args);
    auto runtime = args->get_runtime();

    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(forms[n->cast<double>()](cons(f2, args)), defn);

    return f;
}

boxed make_form(enum forms n, boxed args) {
    return make_form(box<double>(static_cast<double>(n), args->get_runtime()), args);
}

boxed make_form(boxed args) {
    auto p = args->cast<pair>();
    return make_form((*p)->first, (*p)->second);
}

boxed aform(enum forms n, std::shared_ptr<Runtime> runtime) {
    return box<double>(static_cast<double>(n), runtime);
}

boxed lookup_global(const symbol& sym, std::shared_ptr<Runtime> runtime) {
    auto r = find_global(sym, runtime);
    auto defn = cons(aform(forms::global_lambda, runtime),
                     cons(box<symbol>(sym, runtime), box(runtime)));
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
    auto defn = cons(aform(forms::constructed_function0, runtime),
                     cons(args, cons(cf, box(runtime))));
    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

boxed constructed_function0(boxed args) {
    auto args2 = list_ref(args, 1);
    auto cf = list_ref(args, 2);
    return make_lambda<boxed>([args2, cf](boxed args3) -> boxed {
        return applyx(make_form(forms::constructed_function1, cons(args3, box(args3->get_runtime()))),
                      make_global_env(args3->get_runtime()), cf, args2);
    }, args->get_runtime());
}

boxed constructed_function1(boxed args) {
    auto args2 = list_ref(args, 1);
    return make_lambda<boxed>([args2](boxed args3) -> boxed {
        auto f = list_ref(args3, 0);
        return send(f, args2);
    }, args->get_runtime());
}

boxed if1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scan1 = list_ref(args, 3);
    auto scan2 = list_ref(args, 4);
    return make_lambda<boxed>([k, env, scan1, scan2](boxed args) -> boxed {
        auto v = list_ref(args, 0)->cast<bool>();
        auto f = v ? scan1 : scan2;
        return send(f, cons(k, cons(env, box(args->get_runtime()))));
    }, args->get_runtime());
}

boxed if0(boxed args) {
    auto scan0 = list_ref(args, 1);
    auto scan1 = list_ref(args, 2);
    auto scan2 = list_ref(args, 3);
    return make_lambda<boxed>([scan0, scan1, scan2](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scan0,
            cons(make_form(forms::if1,
                           cons(k, cons(env, cons(scan1, cons(scan2, bnil))))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed sclis2(boxed args) {
    auto k = list_ref(args, 1);
    auto v = list_ref(args, 2);
    return make_lambda<boxed>([k, v](boxed args) -> boxed {
        auto w = list_ref(args, 0);
        return sendv(k, cons(v, w));
    }, args->get_runtime());
}

boxed sclis1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        auto bnil = box(args->get_runtime());
        return send(rest,
            cons(make_form(forms::sclis2, cons(k, cons(v, bnil))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed sclis0(boxed args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(first,
            cons(make_form(forms::sclis1,
                           cons(k, cons(env, cons(rest, bnil)))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed scseq1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed) -> boxed {
        return send(rest, cons(k, cons(env, box(k->get_runtime()))));
    }, args->get_runtime());
}

boxed scseq0(boxed args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(first,
            cons(make_form(forms::scseq1,
                           cons(k, cons(env, cons(rest, bnil)))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 3);
    auto env = list_ref(args, 4);
    return make_lambda<boxed>([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, extend_env);
    }, args->get_runtime());
}

boxed lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto len = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda<boxed>([params, len, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return sendv(k,
                     make_form(forms::lambda1,
                               cons(params, cons(len, cons(scanned, cons(env, bnil))))));
    }, args->get_runtime());
}

boxed improper_lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 3);
    auto env = list_ref(args, 4);
    return make_lambda<boxed>([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, improper_extend_env);
    }, args->get_runtime());
}

boxed improper_lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto len = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda<boxed>([params, len, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return sendv(k,
                     make_form(forms::improper_lambda1,
                               cons(params, cons(len, cons(scanned, cons(env, bnil))))));
    }, args->get_runtime());
}

boxed letcc1(boxed args) {
    auto k = list_ref(args, 1);
    return make_lambda<boxed>([k](boxed args) -> boxed {
        return handle_contn_lambda(args, k); 
    }, args->get_runtime());
}

boxed letcc0(boxed args) {
    auto name = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda<boxed>([name, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            cons(k,
                 cons(extend_env(env,
                                 cons(name, bnil),
                                 cons(make_form(forms::letcc1, cons(k, bnil)),
                                      bnil)),
                      bnil)));
    }, args->get_runtime());
}

boxed define1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto name = list_ref(args, 3);
    auto i = list_ref(args, 4);
    return make_lambda<boxed>([k, env, name, i](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        return sendv(k, ctenv_setvar(name, i, v, env));
    }, args->get_runtime());
}

boxed define0(boxed args) {
    auto name = list_ref(args, 1);
    auto i = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda<boxed>([name, i, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            cons(make_form(forms::define1,
                           cons(k, cons(env, cons(name, cons(i, bnil))))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed application1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    return make_lambda<boxed>([k, env](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        return applyx(k, env, list_ref(v, 0), list_rest(v, 0));
    }, args->get_runtime());
}

boxed application0(boxed args) {
    auto scanned = list_ref(args, 1);
    return make_lambda<boxed>([scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(scanned,
            cons(make_form(forms::application1, cons(k, cons(env, bnil))),
                 cons(env, bnil)));
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
    if (exp->contains<pair>()) {
        return cmap(fn, exp, tab, table_set);
    }

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
    if (exp->contains<pair>()) {
        return cmap(fn, exp, tab, table_set);
    }

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

boxed serialized_n(std::shared_ptr<vector> v) {
    return (**v)[1];
}

boxed serialize_aux(boxed exp,
                    cmap_table& tab,
                    map_fn fn,
                    set_entry_fn set_entry) {
    if (exp->contains<pair>()) {
        return cmap(fn, exp, tab, set_entry);
    }

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
    return cons(serialized, box<double>(counter, runtime));
}

boxed deserialize_aux(boxed exp,
                      cmap_table& tab,
                      map_fn fn,
                      set_entry_fn set_entry) {
    if (exp->contains<pair>()) {
        return cmap(fn, exp, tab, set_entry);
    }

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
    return fn((*exp->cast<pair>())->first);
}

json pickle_aux(boxed exp) {
    json j;
    if (exp->empty()) {
        j.push_back(std::string(1, null_code));
    } else if (exp->contains<bool>()) {
        j.push_back(std::string(1, boolean_code));
        j.push_back(exp->cast<bool>() ? "t" : "f");
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
    } else if (exp->contains<pair>()) {
        auto p = exp->cast<pair>();
        j.push_back(std::string(1, pair_code));
        j.push_back(pickle_aux((*p)->first));
        j.push_back(pickle_aux((*p)->second));
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
    case boolean_code:
        return box<bool>(j[1].get<std::string>() == "t", runtime);
    case number_code:
        return box<double>(j[1].get<double>(), runtime);
    case char_code:
        return box<char>(j[1].get<std::string>()[0], runtime);
    case string_code:
        return box<std::string>(j[1].get<std::string>(), runtime);
    case symbol_code:
        return box<symbol>(j[1].get<std::string>(), runtime);
    case pair_code:
        return cons(unpickle_aux(j[1], runtime), unpickle_aux(j[2], runtime));
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

// Copy binary representation to vector and return index in vector
// where it starts
template<typename T>
size_t bpickle_aux(const T& data, std::vector<unsigned char>& v) {
    size_t r = v.size();
    for (size_t i = 0; i < sizeof(data); ++i) {
        v.push_back(reinterpret_cast<const unsigned char*>(&data)[i]);
    }
    return r;
}

// Copy current size of vector to another given position in the vector
void bpickle_aux(std::vector<unsigned char>& v, size_t pos) {
    auto size = static_cast<uint64_t>(v.size());
    for (size_t i = 0; i < sizeof(size); ++i) {
        v[pos + i] = reinterpret_cast<const unsigned char*>(&size)[i];
    }
}

void bpickle(boxed exp, std::vector<uint64_t>& refs, std::vector<unsigned char>& v) {
    if (exp->empty()) {
        v.push_back(null_code);
    } else if (exp->contains<bool>()) {
        v.push_back(boolean_code);
        v.push_back(exp->cast<bool>() ? 1 : 0);
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
    } else if (exp->contains<pair>()) {
        refs.push_back(static_cast<uint64_t>(v.size()));
        v.push_back(pair_code);
        auto p = exp->cast<pair>();
        // Leave space for index/address of first and second values.
        // This means they can be found easily and changed (set-car!, set-cdr!).
        //
        // The index or address is 4 bytes (uint64) preceeded with a 0 byte
        // indicating it's an index into the vector or 1 indicating it's
        // an address.
        //
        // So the byte vector can be used to store the initial data but can
        // accommodate dynamically allocated objects using their address in
        // memory. An evaluator using malloc, for example, can allocate
        // a new object and then use its address in set-car! to change the
        // preceeding byte to 1.
        v.push_back(0);
        auto pos1 = bpickle_aux(static_cast<uint64_t>(0), v);
        v.push_back(0);
        auto pos2 = bpickle_aux(static_cast<uint64_t>(0), v);
        bpickle_aux(v, pos1);
        bpickle((*p)->first, refs, v);
        bpickle_aux(v, pos2);
        bpickle((*p)->second, refs, v);
    } else if (exp->contains<vector>()) {
        auto vec = exp->cast<vector>();
        if (is_serialized(vec)) {
            v.push_back(redirect_code);
            bpickle_aux(refs[serialized_n(vec)->cast<double>()], v);
            return;
        }
        refs.push_back(static_cast<uint64_t>(v.size()));
        if (is_unmemoized(vec)) {
            v.push_back(unmemoized_code);
            return bpickle(unmemoized_repexp(vec), refs, v);
        }
        if (is_result(exp)) {
            v.push_back(result_code);
            v.push_back(0);
            auto pos = bpickle_aux(static_cast<uint64_t>(0), v);
            bpickle_aux(v, pos);
            return bpickle(result_val(exp), refs, v);
        }
        if (is_step_contn(exp)) {
            v.push_back(step_contn_code);
            v.push_back(0);
            auto pos_k = bpickle_aux(static_cast<uint64_t>(0), v);
            v.push_back(0);
            auto pos_env = bpickle_aux(static_cast<uint64_t>(0), v);
            v.push_back(0);
            auto pos_args = bpickle_aux(static_cast<uint64_t>(0), v);
            bpickle_aux(v, pos_k);
            bpickle(step_contn_k(exp), refs, v);
            bpickle_aux(v, pos_env);
            bpickle(step_contn_env(exp), refs, v);
            bpickle_aux(v, pos_args);
            return bpickle(step_contn_args(exp), refs, v);
        }
        if (is_transfer(exp)) {
            v.push_back(transfer_code);
            v.push_back(0);
            auto pos = bpickle_aux(static_cast<uint64_t>(0), v);
            bpickle_aux(v, pos);
            return bpickle(transfer_args(exp), refs, v);
        }
        v.push_back(vector_code);
        // Add size of vector
        auto size = (*vec)->size();
        bpickle_aux(static_cast<uint64_t>(size), v);
        std::vector<size_t> posv;
        for (size_t i = 0; i < size; ++i) {
            // Leave space for index/address of each element. See comment for
            // pair above.
            v.push_back(0);
            posv.push_back(bpickle_aux(static_cast<uint64_t>(0), v));
        }
        for (size_t i = 0; i < size; ++i) {
            bpickle_aux(v, posv[i]);        
            bpickle((*vec)->at(i), refs, v);
        }
    } else {
        throw std::range_error("unknown pickle expression");
    }
}

void bconvert(const std::string& s, std::shared_ptr<Runtime> runtime) {
    auto exp = serialize(unmemoize(mce_restore(s, runtime)))->cast<pair>();

    std::vector<uint64_t> refs;
    std::vector<unsigned char> v;
    bpickle((*exp)->first, refs, v);

    if (refs.size() != (*exp)->second->cast<double>()) {
        throw std::length_error("unexpected number of references");
    }

    auto size = static_cast<uint64_t>(v.size());
    for (size_t i = 0; i < sizeof(size); ++i) {
        std::cout << reinterpret_cast<const unsigned char*>(&size)[i];
    }

    for (auto const& c : v) {
        std::cout << c;
    }
}

void bconvert(std::istream &stream, std::shared_ptr<Runtime> runtime) {
    json j;
    stream >> j;
    return bconvert(j.get<std::string>(), runtime);
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
        ("bconvert",
         "Convert CPS form or state to binary format",
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
    if (opts.count("bconvert")) {
        auto state = opts["bconvert"].as<std::string>();
        if (state.empty()) {
            bconvert(std::cin, runtime);
        } else {
            bconvert(state, runtime);
        }
        return box(runtime);
    }
    return start(std::cin, box(runtime));
}

boxed cf_test(boxed args) {
    auto n = list_ref(args, 0)->cast<double>();
    auto x = list_ref(args, 1);
    if (n == 0) {
        return make_lambda<boxed>([x](boxed args) {
            return cf_test(cons(list_ref(args, 0), cons(x, box(args->get_runtime()))));
        }, args->get_runtime());
    }
    return box<double>(x->cast<double>() + n, args->get_runtime());
}

boxed transfer_test(boxed args) {
    auto runtime = args->get_runtime();
    return applyx(lookup_global(symbol("result"), runtime),
                  make_global_env(args->get_runtime()),
                  list_ref(args, 0),
                  list_rest(args, 0));
}

Runtime::Runtime() :
    gc_threshold(10000),
    global_table {
        { "result", result },
        { "<", less_than },
        { ">", greater_than },
        { "print", print },
        { "eprint", eprint },
        { "write", write },
        { "ewrite", write },
        { "+", plus },
        { "-", minus },
        { "*", multiply },
        { "/", divide },
        { "null?", is_null },
        { "car", car },
        { "cdr", cdr },
        { "set-car!", set_car },
        { "set-cdr!", set_cdr },
        { "eq?", is_eq },
        { "=", is_number_equal },
        { "string?", is_string },
        { "pair?", is_pair },
        { "procedure?", is_procedure },
        { "string=?", is_string_equal },
        { "vector?", is_vector },
        { "vector-length", vector_length },
        { "vector-ref", vector_ref },
        { "unmemoize", gunmemoize },
        { "serialize", gserialize },
        { "apply", gapplyx },
        { "save", save },
        { "restore", restore },
        { "transfer", transfer },
        { "getpid", getpid },
        { "cons", gcons },
        { "list->vector", list_to_vector },
        { "get-config", mce::get_config },
        { "cf-test", cf_test },
        { "transfer-test", transfer_test },
        { "set-gc-callback!", mce::set_gc_callback }
    },
    kenvfn_set({
        gapplyx,
        transfer,
        restore
    }) {}

void Runtime::set_gc_threshold(size_t v) {
    gc_threshold = v;
}

}
