#include <sys/types.h>
#include <unistd.h>
#include <iostream>
#include <utility>
#include "mce.hpp"
#include "json.hpp"
#include "base64.hpp"
#include "cxxopts.hpp"

using nlohmann::json;

namespace mce {

const char true_code       = '0';
const char false_code      = '1';
const char number_code     = '2';
const char vector_code     = '3';

const char marker_code     = 'A';

const char binary_code     = 'a';
const char unmemoized_code = 'b';
const char result_code     = 'c';
const char step_contn_code = 'd';
const char transfer_code   = 'e';

typedef std::vector<uint8_t> binary;

const auto mark_prefix = {
    static_cast<uint8_t>(marker_code),
    static_cast<uint8_t>('M'),
    static_cast<uint8_t>('C'),
    static_cast<uint8_t>('E'),
    static_cast<uint8_t>('-')
};

template<size_t size>
std::shared_ptr<binary> mark(const char (&type)[size]) {
    auto b = std::make_shared<binary>(mark_prefix);
    b->insert(b->end(), type, type + size - 1);
    return b;
}

const auto yield_defn_mark = mark("YIELD-DEFINITION");
const auto step_contn_mark = mark("STEP-CONTN");
const auto transfer_mark = mark("TRANSFER");
const auto result_mark = mark("RESULT");
const auto unmemoized_mark = mark("UNMEMOIZED");
const auto serialized_mark = mark("SERIALIZED");

template<typename T>
inline boxed box(const typename box_type<T>::type& a, const RuntimeInfo& info) {
    return std::make_shared<Box>(a, info);
}

template<>
struct box_type<binary> { typedef std::shared_ptr<binary> type; };

template<>
struct box_type<vector> { typedef std::shared_ptr<vector> type; };

void debug_log(boxed exp, std::ostream& out, bool is_write) {
    if (exp->contains<bool>()) {
        out << (exp->cast<bool>() ? "#t" : "#f");
    } else if (exp->contains<double>()) {
        out << exp->cast<double>();
    } else if (exp->contains<binary>()) {
        auto b = exp->cast<binary>();
        auto s = std::string(b->begin(), b->end());
        if (is_write) {
            json j = s;
            out << j.dump();
        } else {
            out << s;
        }
    } else if (exp->contains<vector>()) {
        bool first = true;
        out << "#(";
        auto vec = exp->cast<vector>();
        for (auto const& v : **vec) {
            if (!first) {
                out << " ";
            }
            first = false;
            debug_log(v, out, is_write);
        }
        out << ")";
    } else if (exp->contains<lambda>()) {
        out << "#<procedure>";
    } else {
        throw std::range_error("unknown expression");
    }
}

struct CMapHash {
    std::size_t operator()(const boxed& a) const noexcept {
        if (a->contains<bool>()) {
            return std::hash<bool>{}(a->cast<bool>());
        }

        if (a->contains<double>()) {
            return std::hash<double>{}(a->cast<double>());
        }

        if (a->contains<vector>()) {
            return std::hash<std::shared_ptr<vector>>{}(a->cast<vector>());
        }

        if (a->contains<binary>()) {
            return std::hash<std::shared_ptr<binary>>{}(a->cast<binary>());
        }

        if (a->contains<lambda>()) {
            return std::hash<lambda>{}(a->cast<lambda>());
        }

        return std::hash<boxed>{}(a);
    }
};

struct CMapEqual {
    bool operator()(const boxed& x, const boxed& y) const noexcept {
        if (!x->contains_type_of(*y)) {
            return false;
        }

        if (x->contains<bool>()) {
            return x->cast<bool>() == y->cast<bool>();
        }

        if (x->contains<double>()) {
            return x->cast<double>() == y->cast<double>();
        }

        if (x->contains<vector>()) {
            return x->cast<vector>() == y->cast<vector>();
        }

        if (x->contains<binary>()) {
            return x->cast<binary>() == y->cast<binary>();
        }

        if (x->contains<lambda>()) {
            return x->cast<lambda>() == y->cast<lambda>();
        }

        return x == y;
    }
};

typedef std::unordered_map<boxed, boxed, CMapHash, CMapEqual> cmap_table;
typedef std::function<boxed(boxed)> map_fn;
typedef std::function<boxed(cmap_table&, boxed, boxed)> set_entry_fn;
typedef boxed extend_rtenv_fn(boxed env, size_t len, boxed values);

boxed make_vector(const RuntimeInfo& info) {
    const auto runtime = info.get_runtime();
    auto v = std::shared_ptr<vector>(new vector(new std::vector<boxed>()), [runtime](auto vptr) {
        runtime->allocated.vectors.erase(vptr);
        delete vptr;
    });
    runtime->allocated.vectors[v.get()] = v;
    return box<vector>(v, info);
}

boxed vc(boxed first, boxed second) {
    auto a = make_vector(first->get_runtime_info());
    auto v = a->cast<vector>();
    (*v)->push_back(first);
    (*v)->push_back(second);
    return a;
}

template<>
lambda make_lambda<lambda>(std::function<function> fn,
                           const RuntimeInfo& info,
                           bool has_defn) {
    const auto runtime = info.get_runtime();
    auto f = std::shared_ptr<func>(new func(new std::function<function>(fn)), [runtime](auto fptr) {
        runtime->allocated.functions.erase(fptr);
        delete fptr;
    });
    runtime->allocated.functions[f.get()] = std::make_pair(has_defn, f);
    return f;
}

template<>
boxed make_lambda<boxed>(std::function<function> fn,
                         const RuntimeInfo& info,
                         bool has_defn) {
    return box<lambda>(make_lambda<lambda>(fn, info, has_defn), info);
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
    // string which we'll restore later. The other end of the cycle will be
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

template<typename Input>
boxed mce_restore(Input& s, const RuntimeInfo& info);

void Runtime::maybe_gc(boxed x) {
    if (((allocated.vectors.size() > gc_threshold) ||
         (allocated.functions.size() > gc_threshold)) &&
        !calling_gc_callback) {
        const auto& info = x->get_runtime_info();

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
            (*saved_allocations)->push_back(box<vector>(v, info));
        }

        // For each lambda, add it to the boxed vector and remember its position.
        for (auto const& entry : allocated.functions) {
            auto const& f = entry.second.second.lock();
            assert(f); // deleter in make_lambda should remove unreferenced lambdas
            if (entry.second.first) {
                saved_indices[f.get()] = (*saved_allocations)->size();
                (*saved_allocations)->push_back(box<lambda>(f, info));
            }
        }

        // Save the boxed vector into a string.
        // Note: This process itself will have created more objects with cycles but
        // they will all be destroyed when we break cycles since we know there will
        // be nowhere else referencing them.
        auto saved = mce_save(box<vector>(saved_allocations, info));
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
        auto restored = mce_restore(saved, info)->cast<vector>();

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
        saved = mce_save(box<vector>(saved_allocations, info));
        saved_allocations.reset();

        // Break the cycles again.
        break_cycles();

        // Restore again. This will only have what we remembered was live.
        restored = mce_restore(saved, info)->cast<vector>();

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
            auto a = make_vector(info);
            auto v = a->cast<vector>();
            for (auto s : stats) {
                auto b = make_vector(info);
                auto w = b->cast<vector>();
                for (auto t : s) {
                    (*w)->push_back(box<double>(t, info));
                }
                (*v)->push_back(b);
            }
            calling_gc_callback = true;
            std::unique_ptr<bool, void(*)(bool*)> cleanup(&calling_gc_callback, [](bool *p) {
                *p = false;
            });
            auto r = (**gc_callback->cast<lambda>())(vc(a, info.get_nil()));
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
    auto a = make_vector(vl->get_runtime_info());
    auto v = a->cast<vector>();

    while (true) {
        const auto is_vector = vl->contains<vector>();
        const auto v2 = is_vector ? vl->cast<vector>() : nullptr;
        const auto size = is_vector ? (*v2)->size() : 0;

        if (is_vector && (size == 2)) {
            (*v)->push_back((*v2)->at(0));
            vl = (*v2)->at(1);
        } else {
            if (!(is_vector && (size == 0))) {
                (*v)->push_back(vl);
            }
            break;
        }
    }

    return a;
}

boxed vector_cmap(map_fn f, boxed v, cmap_table& tab, set_entry_fn set_entry) {
    auto vv = v->cast<vector>();
    if ((*vv)->empty()) {
        return v;
    }

    auto ref = tab.find(v);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto entry = set_entry(tab, v, make_vector(v->get_runtime_info()));
    auto ev = entry->cast<vector>();

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
    return first->contains<binary>() &&
           (*first->cast<binary>() == *yield_defn_mark);
}

boxed get_procedure_defn(boxed proc) {
    const auto& info = proc->get_runtime_info();
    return (**proc->cast<lambda>())(
        vc(box<binary>(yield_defn_mark, info), info.get_nil()));
}

boxed unmemoize(boxed exp);
boxed serialize(boxed exp);

boxed memoize_lambda(lambda proc, boxed defn) {
    return make_lambda<boxed>([proc, defn](boxed args) -> boxed {
        if (is_yield_defn(args)) {
            if (defn->contains<lambda>()) {
                return (**defn->cast<lambda>())(args->get_runtime_info().get_nil());
            }
            return defn;
        }
        return (**proc)(args);
    }, defn->get_runtime_info(), true);
}

boxed memoize_lambda(boxed proc, boxed defn) {
    return memoize_lambda(proc->cast<lambda>(), defn);
}

boxed send(boxed k, boxed args) {
    return vc(k, args);
}

boxed sendv(boxed k, boxed v) {
    return send(k, vc(v, v->get_runtime_info().get_nil()));
}

boxed rtenv_lookup(boxed i, boxed env) {
    auto iv = i->cast<vector>();
    auto first = (*iv)->at(0)->cast<double>();
    auto second = (*iv)->at(1)->cast<double>();
    auto v = vlist_ref(env, first)->cast<vector>();
    if (second < (*v)->size()) {
        return (**v)[second];
    }
    return i->get_runtime_info().get_nil();
}

boxed rtenv_setvar(boxed i, boxed val, boxed env) {
    auto iv = i->cast<vector>();
    auto first = (*iv)->at(0)->cast<double>();
    auto second = (*iv)->at(1)->cast<double>();
    auto v = vlist_ref(env, first)->cast<vector>();
    auto len = (*v)->size();
    if (second >= len) {
        const auto nil = i->get_runtime_info().get_nil();
        for (auto j = len; j < second; ++j) {
            (*v)->push_back(nil);
        }
        (*v)->push_back(val);
    } else {
        (**v)[second] = val;
    }
    return val;
}

boxed symbol_lookup(boxed args) {
    auto i = vlist_ref(args, 1);
    return make_lambda<boxed>([i](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        return sendv(k, rtenv_lookup(i, env));
    }, args->get_runtime_info());
}

boxed send_value(boxed args) {
    auto exp = vlist_ref(args, 1);
    return make_lambda<boxed>([exp](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        return sendv(k, exp);
    }, args->get_runtime_info());
}

boxed make_step_contn(boxed k, boxed env, boxed args) {
    return vc(box<binary>(step_contn_mark, k->get_runtime_info()), vc(k, vc(env, args)));
}

bool is_step_contn(boxed args) {
    if (!args->contains<vector>() || (*args->cast<vector>())->empty()) {
        return false;
    }
    auto first = vlist_ref(args, 0);
    return first->contains<binary>() &&
           (*first->cast<binary>() == *step_contn_mark);
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
    return first->contains<binary>() &&
           (*first->cast<binary>() == *transfer_mark);
}

boxed transfer_args(boxed args) {
    return vlist_rest(args, 0);
}

boxed transfer(boxed args) {
    return send(vlist_ref(args, 2),
        vc(box<binary>(transfer_mark, args->get_runtime_info()), vlist_rest(args, 2)));
}

boxed make_global_rtenv(boxed x) {
    const auto& info = x->get_runtime_info();
    return vc(make_vector(info), info.get_nil());
}

boxed applyx(boxed k, boxed env, boxed fn, boxed args) {
    return send(fn, make_step_contn(k, env, args));
}

boxed result(boxed args) {
    const auto& info = args->get_runtime_info();
    auto a = make_vector(info);
    auto v = a->cast<vector>();
    (*v)->push_back(box<binary>(result_mark, info));
    (*v)->push_back(vlist_ref(args, 0));
    return a;
}

bool is_result(boxed exp) {
    if (!exp->contains<vector>()) {
        return false;
    }
    auto v = exp->cast<vector>();
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<binary>() &&
           (*(**v)[0]->cast<binary>() == *result_mark);
}

boxed result_val(boxed exp) {
    return (**exp->cast<vector>())[1];
}

boxed less_than(boxed args) {
    return box<bool>(vlist_ref(args, 0)->cast<double>() <
                     vlist_ref(args, 1)->cast<double>(),
                     args->get_runtime_info());
}

boxed greater_than(boxed args) {
    return box<bool>(vlist_ref(args, 0)->cast<double>() >
                     vlist_ref(args, 1)->cast<double>(),
                     args->get_runtime_info());
}

boxed add(boxed args) {
    return box<double>(vlist_ref(args, 0)->cast<double>() +
                       vlist_ref(args, 1)->cast<double>(),
                       args->get_runtime_info());
}

boxed subtract(boxed args) {
    return box<double>(vlist_ref(args, 0)->cast<double>() -
                       vlist_ref(args, 1)->cast<double>(),
                       args->get_runtime_info());
}

boxed multiply(boxed args) {
    return box<double>(vlist_ref(args, 0)->cast<double>() *
                       vlist_ref(args, 1)->cast<double>(),
                       args->get_runtime_info());
}

boxed divide(boxed args) {
    return box<double>(vlist_ref(args, 0)->cast<double>() /
                       vlist_ref(args, 1)->cast<double>(),
                       args->get_runtime_info());
}

boxed gfloor(boxed args) {
    return box<double>(floor(vlist_ref(args, 0)->cast<double>()), args->get_runtime_info());
}

boxed is_procedure(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<lambda>(), args->get_runtime_info());
}

boxed is_boolean(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<bool>(), args->get_runtime_info());
}

boxed is_number(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<double>(), args->get_runtime_info());
}

boxed make_vector(boxed args) {
    auto n = vlist_ref(args, 0)->cast<double>();
    auto a = make_vector(args->get_runtime_info());
    auto v = a->cast<vector>();
    const auto nil = args->get_runtime_info().get_nil();
    for (auto i = n; i >= 1; --i) {
        (*v)->push_back(nil);
    }
    return a;
}

boxed is_vector(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<vector>(), args->get_runtime_info());
}

boxed vector_length(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<double>(static_cast<double>((*a->cast<vector>())->size()),
                       args->get_runtime_info());
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
    return args->get_runtime_info().get_nil();
}

boxed make_binary(boxed args) {
    auto n = vlist_ref(args, 0)->cast<double>();
    auto b = std::make_shared<binary>(n);
    return box<binary>(b, args->get_runtime_info());
}

boxed is_binary(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<bool>(a->contains<binary>(), args->get_runtime_info());
}

boxed binary_length(boxed args) {
    auto a = vlist_ref(args, 0);
    return box<double>(static_cast<double>(a->cast<binary>()->size()),
                       args->get_runtime_info());
}

boxed binary_ref(boxed args) {
    auto a = vlist_ref(args, 0);
    auto i = vlist_ref(args, 1);
    return box<double>(a->cast<binary>()->at(i->cast<double>()),
                       args->get_runtime_info());
}

boxed binary_set(boxed args) {
    auto a = vlist_ref(args, 0);
    auto i = vlist_ref(args, 1);
    auto n = vlist_ref(args, 2);
    a->cast<binary>()->at(i->cast<double>()) = n->cast<double>();
    return args->get_runtime_info().get_nil();
}

boxed is_number_equal(boxed args) {
    auto x = vlist_ref(args, 0);
    auto y = vlist_ref(args, 1);
    return box<bool>(x->cast<double>() == y->cast<double>(),
                     args->get_runtime_info());
}

boxed is_same_object(boxed args) {
    auto x = vlist_ref(args, 0);
    auto y = vlist_ref(args, 1);

    const auto& info = args->get_runtime_info();

    if (!x->contains_type_of(*y)) {
        return box<bool>(false, info);
    }

    if (x->contains<binary>()) {
        return box<bool>(x->cast<binary>() == y->cast<binary>(), info);
    }

    if (x->contains<vector>()) {
        return box<bool>(x->cast<vector>() == y->cast<vector>(), info);
    }

    if (x->contains<lambda>()) {
        return box<bool>(x->cast<lambda>() == y->cast<lambda>(), info);
    }

    return box<bool>(false, info);
}

boxed gapplyx(boxed args) {
    return applyx(vlist_ref(args, 0),
                  vlist_ref(args, 1),
                  vlist_ref(args, 2),
                  vlist_ref(args, 3));
}

boxed save(boxed args) {
    const auto& state = mce_save(vlist_ref(args, 0));
    auto b = std::make_shared<binary>(state.begin(), state.end());
    return box<binary>(b, args->get_runtime_info());
}

boxed restore(boxed args) {
    return sendv(vlist_ref(args, 0),
                 mce_restore(*vlist_ref(args, 2)->cast<binary>(),
                             args->get_runtime_info()));
}

boxed getpid(boxed args) {
    return box<double>(static_cast<double>(::getpid()), args->get_runtime_info());
}

boxed output_binary(boxed args, FILE *stream) {
    const auto b = vlist_ref(args, 0)->cast<binary>();
    const auto start = static_cast<size_t>(vlist_ref(args, 1)->cast<double>());
    const auto end = static_cast<size_t>(vlist_ref(args, 2)->cast<double>());
    assert(start <= end);
    assert(end <= b->size());
    fwrite(b->data() + start, 1, end - start, stream);
    return args->get_runtime_info().get_nil();
}

boxed output_binary_to_stdout(boxed args) {
    return output_binary(args, stdout);
}

boxed output_binary_to_stderr(boxed args) {
    return output_binary(args, stderr);
}

boxed error(boxed args) {
    const auto proc = vlist_ref(args, 0)->cast<binary>();
    const auto msg = vlist_ref(args, 1)->cast<binary>();
    throw std::runtime_error(std::string(proc->begin() + 1, proc->end()) + ": " +
                             std::string(msg->begin() + 1, msg->end()));
}

boxed Runtime::get_config(const std::string& k) {
    auto it = config_table.find(k);
    if (it == config_table.end()) {
        return nullptr;
    }
    return it->second;
}

boxed get_config(boxed args) {
    auto b = vlist_ref(args, 0)->cast<binary>();
    const auto& info = args->get_runtime_info();
    const auto r = info.get_runtime()->get_config(std::string(b->begin() + 1, b->end()));
    if (r) {
        return r;
    }
    return box<bool>(false, info);
}

void Runtime::set_config(const std::string& k, boxed v) {
    config_table[k] = v;
}

void Runtime::set_gc_callback(boxed v) {
    gc_callback = v;
}

boxed set_gc_callback(boxed args) {
    const auto& info = args->get_runtime_info();
    info.get_runtime()->set_gc_callback(vlist_ref(args, 0));
    return info.get_nil();
}

function* Runtime::get_global_function(const std::string& name) {
    auto it = global_table.find(name);
    if (it == global_table.end()) {
        return nullptr;
    }
    return it->second;
}

function* Runtime::get_global_function(const double i) {
    if (i < core_globals.size()) {
        return core_globals[i];
    }
    return nullptr;
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

bool Runtime::is_kenv_function(function f) {
    return kenvfn_set.find(f) != kenvfn_set.end();
}

boxed find_global(const binary& b, const RuntimeInfo& info) {
    const auto sym = std::string(b.begin() + 1, b.end());
    const auto f = info.get_runtime()->get_global_function(sym);
    if (!f) {
        // out_of_range doesn't show sym when serialized
        throw std::range_error(sym);
    }
    return make_lambda<boxed>(f, info);
}

boxed find_global(const double i, const RuntimeInfo& info) {
    const auto f = info.get_runtime()->get_global_function(i);
    if (!f) {
        throw std::range_error(std::to_string(i));
    }
    return make_lambda<boxed>(f, info);
}

boxed find_global(boxed b) {
    if (b->contains<double>()) {
        return find_global(b->cast<double>(), b->get_runtime_info());
    }
    return find_global(*b->cast<binary>(), b->get_runtime_info());
}

boxed step(boxed state) {
    return (**vlist_ref(state, 0)->cast<lambda>())(vlist_rest(state, 0));
}

boxed run(boxed state) {
    const auto runtime = state->get_runtime_info().get_runtime();

    while (!is_result(state)) {
        state = step(state);
        runtime->maybe_gc(state);
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

boxed lookup_global(const double i, boxed x);

boxed handle_global_lambda_kenv(boxed args, boxed fn) {
    if (is_step_contn(args)) {
        return send(fn, vc(step_contn_k(args),
                           vc(step_contn_env(args),
                              step_contn_args(args))));
    }

    return run(send(fn, vc(lookup_global(args->get_runtime_info().get_runtime()->g_result, args),
                           vc(make_global_rtenv(args),
                              args))));
}

boxed wrap_global_lambda(boxed fn, boxed cf) {
    const lambda l = fn->cast<lambda>();
    auto p = (*l)->target<function*>();
    const auto& info = fn->get_runtime_info();

    if (p && info.get_runtime()->is_kenv_function(*p)) {
        return make_lambda<boxed>([fn](boxed args) -> boxed {
            return handle_global_lambda_kenv(args, fn);
        }, info);
    }

    return make_lambda<boxed>([fn, cf](boxed args) -> boxed {
        return handle_global_lambda(args, fn, cf);
    }, info);
}

boxed extend_rtenv(boxed env, size_t, boxed values) {
    return vc(vlist_to_vector(values), env);
}

boxed improper_extend_rtenv(boxed env, size_t len, boxed values) {
    auto av = make_vector(env->get_runtime_info());
    auto v = av->cast<vector>();
    size_t i = 0;

    while (true) {
        const auto is_vector = values->contains<vector>();
        const auto v2 = is_vector ? values->cast<vector>() : nullptr;
        const auto size = is_vector ? (*v2)->size() : 0;

        if ((i == (len - 1)) || !is_vector || (size != 2)) {
            if (!is_vector || (size != 0)) {
                (*v)->push_back(values);
            }
            break;
        }

        (*v)->push_back((*v2)->at(0));
        values = (*v2)->at(1);
        ++i;
    }

    return vc(av, env);
}

boxed handle_lambda(boxed args,
                    size_t len,
                    boxed fn,
                    boxed env,
                    extend_rtenv_fn extend_rtenv) {
    const auto& info = args->get_runtime_info();
    const auto nil = info.get_nil();

    if (is_step_contn(args)) {
        return send(fn, vc(step_contn_k(args),
                           vc(extend_rtenv(env, len, step_contn_args(args)),
                              nil)));
    }

    return run(send(fn, vc(lookup_global(info.get_runtime()->g_result, args),
                           vc(extend_rtenv(env, len, args),
                              nil))));
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
        return send(scanned, vc(k, vc(env, k->get_runtime_info().get_nil())));
    }, args->get_runtime_info());
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
    const auto& info = args->get_runtime_info();

    auto f = box<double>(0, info);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, info), defn);
    *f = *memoize_lambda(forms.at(n->cast<double>())(vc(f2, args)), defn);

    return f;
}

boxed make_form(enum forms n, boxed args) {
    return make_form(box<double>(static_cast<double>(n), args->get_runtime_info()), args);
}

boxed make_form(boxed args) {
    auto vl = args->cast<vector>();
    return make_form((*vl)->at(0), (*vl)->at(1));
}

boxed aform(enum forms n, const RuntimeInfo& info) {
    return box<double>(static_cast<double>(n), info);
}

boxed lookup_global(const double i, boxed x) {
    const auto& info = x->get_runtime_info();
    auto r = find_global(i, info);
    auto defn = vc(aform(forms::global_lambda, info),
                   vc(box<double>(i, info), info.get_nil()));
    auto f = box<double>(0, info);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, info), defn);
    *f = *memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

boxed globalize(boxed x, boxed args, boxed cf) {
    if (!x->contains<lambda>()) {
        return x;
    }

    const auto& info = x->get_runtime_info();
    auto defn = vc(aform(forms::constructed_function0, info),
                   vc(args, vc(cf, info.get_nil())));
    auto f = box<double>(0, info);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (**f->cast<lambda>())(args);
    }, info), defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

boxed constructed_function0(boxed args) {
    auto args2 = vlist_ref(args, 1);
    auto cf = vlist_ref(args, 2);
    return make_lambda<boxed>([args2, cf](boxed args3) -> boxed {
        return applyx(make_form(forms::constructed_function1,
                                vc(args3, args3->get_runtime_info().get_nil())),
                      make_global_rtenv(args3), cf, args2);
    }, args->get_runtime_info());
}

boxed constructed_function1(boxed args) {
    auto args2 = vlist_ref(args, 1);
    return make_lambda<boxed>([args2](boxed args3) -> boxed {
        auto f = vlist_ref(args3, 0);
        return send(f, args2);
    }, args->get_runtime_info());
}

boxed if1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto scan1 = vlist_ref(args, 3);
    auto scan2 = vlist_ref(args, 4);
    return make_lambda<boxed>([k, env, scan1, scan2](boxed args) -> boxed {
        auto v = vlist_ref(args, 0)->cast<bool>();
        auto f = v ? scan1 : scan2;
        return send(f, vc(k, vc(env, args->get_runtime_info().get_nil())));
    }, args->get_runtime_info());
}

boxed if0(boxed args) {
    auto scan0 = vlist_ref(args, 1);
    auto scan1 = vlist_ref(args, 2);
    auto scan2 = vlist_ref(args, 3);
    return make_lambda<boxed>([scan0, scan1, scan2](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(scan0,
            vc(make_form(forms::if1,
                         vc(k, vc(env, vc(scan1, vc(scan2, nil))))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

boxed sclis2(boxed args) {
    auto k = vlist_ref(args, 1);
    auto v = vlist_ref(args, 2);
    return make_lambda<boxed>([k, v](boxed args) -> boxed {
        auto w = vlist_ref(args, 0);
        return sendv(k, vc(v, w));
    }, args->get_runtime_info());
}

boxed sclis1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto rest = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        const auto nil = args->get_runtime_info().get_nil();
        return send(rest,
            vc(make_form(forms::sclis2, vc(k, vc(v, nil))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

boxed sclis0(boxed args) {
    auto first = vlist_ref(args, 1);
    auto rest = vlist_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(first,
            vc(make_form(forms::sclis1,
                         vc(k, vc(env, vc(rest, nil)))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

boxed scseq1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto rest = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed) -> boxed {
        return send(rest, vc(k, vc(env, k->get_runtime_info().get_nil())));
    }, args->get_runtime_info());
}

boxed scseq0(boxed args) {
    auto first = vlist_ref(args, 1);
    auto rest = vlist_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(first,
            vc(make_form(forms::scseq1,
                         vc(k, vc(env, vc(rest, nil)))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

boxed lambda1(boxed args) {
    auto len = vlist_ref(args, 1)->cast<double>();
    auto scanned = vlist_ref(args, 2);
    auto env = vlist_ref(args, 3);
    return make_lambda<boxed>([len, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, len, scanned, env, extend_rtenv);
    }, args->get_runtime_info());
}

boxed lambda0(boxed args) {
    auto len = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([len, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        return sendv(k,
            make_form(forms::lambda1,
                      vc(len, vc(scanned, vc(env, args->get_runtime_info().get_nil())))));
    }, args->get_runtime_info());
}

boxed improper_lambda1(boxed args) {
    auto len = vlist_ref(args, 1)->cast<double>();
    auto scanned = vlist_ref(args, 2);
    auto env = vlist_ref(args, 3);
    return make_lambda<boxed>([len, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, len, scanned, env, improper_extend_rtenv);
    }, args->get_runtime_info());
}

boxed improper_lambda0(boxed args) {
    auto len = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([len, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        return sendv(k,
            make_form(forms::improper_lambda1,
                      vc(len, vc(scanned, vc(env, args->get_runtime_info().get_nil())))));
    }, args->get_runtime_info());
}

boxed letcc1(boxed args) {
    auto k = vlist_ref(args, 1);
    return make_lambda<boxed>([k](boxed args) -> boxed {
        return handle_contn_lambda(args, k); 
    }, args->get_runtime_info());
}

boxed letcc0(boxed args) {
    auto scanned = vlist_ref(args, 1);
    return make_lambda<boxed>([scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(scanned,
            vc(k,
               vc(extend_rtenv(env,
                               1,
                               vc(make_form(forms::letcc1, vc(k, nil)),
                                  nil)),
                  nil)));
    }, args->get_runtime_info());
}

boxed define1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    auto i = vlist_ref(args, 3);
    return make_lambda<boxed>([k, env, i](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        return sendv(k, rtenv_setvar(i, v, env));
    }, args->get_runtime_info());
}

boxed define0(boxed args) {
    auto i = vlist_ref(args, 1);
    auto scanned = vlist_ref(args, 2);
    return make_lambda<boxed>([i, scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(scanned,
            vc(make_form(forms::define1, vc(k, vc(env, vc(i, nil)))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

boxed application1(boxed args) {
    auto k = vlist_ref(args, 1);
    auto env = vlist_ref(args, 2);
    return make_lambda<boxed>([k, env](boxed args) -> boxed {
        auto v = vlist_ref(args, 0);
        return applyx(k, env, vlist_ref(v, 0), vlist_rest(v, 0));
    }, args->get_runtime_info());
}

boxed application0(boxed args) {
    auto scanned = vlist_ref(args, 1);
    return make_lambda<boxed>([scanned](boxed args) -> boxed {
        auto k = vlist_ref(args, 0);
        auto env = vlist_ref(args, 1);
        const auto nil = args->get_runtime_info().get_nil();
        return send(scanned,
            vc(make_form(forms::application1, vc(k, vc(env, nil))),
               vc(env, nil)));
    }, args->get_runtime_info());
}

bool is_unmemoized(std::shared_ptr<vector> v) {
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<binary>() &&
           (*(**v)[0]->cast<binary>() == *unmemoized_mark);
}

boxed unmemoized_repexp(std::shared_ptr<vector> v) {
    return (**v)[1];
}

boxed memoize_aux(boxed exp, cmap_table& tab, map_fn fn) {
    if (exp->contains<vector>()) {
        auto v = exp->cast<vector>();
        if (is_unmemoized(v)) {
            auto repexp = unmemoized_repexp(v);
            auto info = exp->get_runtime_info();
            auto r = box<double>(0, info);
            auto f = box<double>(0, info);
            auto entry = table_set(tab, exp, memoize_lambda(
                make_lambda<lambda>([f](boxed args) -> boxed {
                    return (**f->cast<lambda>())(args);
                }, info),
                make_lambda<boxed>([r](boxed) -> boxed {
                    return r;
                }, info)));
            *r = *fn(repexp);
            *f = Box(make_lambda<lambda>([r, f](boxed args) -> boxed {
                    *f = *make_form(r);
                    return (**f->cast<lambda>())(args);
                }, info), info);
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

        const auto& info = exp->get_runtime_info();
        auto entry = table_set(tab, exp, make_vector(info));
        auto v = entry->cast<vector>();

        (*v)->push_back(box<binary>(unmemoized_mark, info));
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

boxed make_serialized(size_t n, const RuntimeInfo& info) {
    auto a = make_vector(info);
    auto v = a->cast<vector>();

    (*v)->push_back(box<binary>(serialized_mark, info));
    (*v)->push_back(box<double>(static_cast<double>(n), info));

    return a;
}

bool is_serialized(std::shared_ptr<vector> v) {
    return ((*v)->size() == 2) &&
           (**v)[0]->contains<binary>() &&
           (*(**v)[0]->cast<binary>() == *serialized_mark);
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
    const auto& info = exp->get_runtime_info();
    set_entry_fn set_entry = [&counter, info](cmap_table& tab, boxed v, boxed entry) {
        table_set(tab, v, make_serialized(counter++, info));
        return entry;
    };
    map_fn fn = [&tab, &fn, &set_entry](boxed x) -> boxed {
        return serialize_aux(x, tab, fn, set_entry);
    };
    auto serialized = fn(exp);
    return vc(serialized, box<double>(counter, info));
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
                         box<double>(static_cast<double>(counter++), entry->get_runtime_info()),
                         entry);
    };
    map_fn fn = [&tab, &fn, &set_entry](boxed x) -> boxed {
        return deserialize_aux(x, tab, fn, set_entry);
    };
    return fn((*exp->cast<vector>())->at(0));
}

json pickle_aux(boxed exp) {
    json j;
    if (exp->contains<bool>()) {
        j.push_back(std::string(1, exp->cast<bool>() ? true_code : false_code));
    } else if (exp->contains<double>()) {
        j.push_back(std::string(1, number_code));
        j.push_back(exp->cast<double>());
    } else if (exp->contains<binary>()) {
        j = base64::encode(*exp->cast<binary>());
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

boxed unpickle_aux(const json& j, const RuntimeInfo& info) {
    if (j.is_array()) {
        switch (j[0].get<std::string>()[0]) {
            case true_code:
                return box<bool>(true, info);
            case false_code:
                return box<bool>(false, info);
            case number_code:
                return box<double>(j[1].get<double>(), info);
            case vector_code: {
                auto a = make_vector(info);
                auto v = a->cast<vector>();
                auto size = j.size();
                for (json::size_type i = 2; i < size; ++i) {
                    (*v)->push_back(unpickle_aux(j[i], info));
                }
                return a;
            }
            default:
                throw std::range_error("unknown unpickle expression");
        }
    }
    if (j.is_string()) {
        return box<binary>(
            std::make_shared<binary>(base64::decode(j.get<std::string>())),
            info);
    }
    throw std::range_error("unknown unpickle expression");
}

json parse(const std::string& s) {
    return json::parse(s);
}

json parse(const binary& b) {
    return json::parse(b.data(), b.data() + b.size());
}

json parse(std::istream& stream) {
    json j;
    stream >> j;
    return j;
}

template<typename Input>
boxed unpickle(Input& s, const RuntimeInfo& info) {
    return unpickle_aux(parse(s), info);
}

std::string mce_save(boxed exp) {
    return pickle(serialize(unmemoize(exp)));
}

template<typename Input>
boxed mce_restore(Input& s, const RuntimeInfo& info) {
    return memoize(deserialize(unpickle(s, info)));
}

template<typename Input>
boxed start(Input& s, boxed args) {
    auto r = mce_restore(s, args->get_runtime_info());
    if (r->template contains<lambda>()) {
        return (**r->template cast<lambda>())(args);
    }
    return run(r);
}

// Copy binary representation to vector
template<typename T>
void bpickle_aux(const T& data, std::vector<uint8_t>& v) {
    for (size_t i = 0; i < sizeof(data); ++i) {
        v.push_back(reinterpret_cast<const uint8_t*>(&data)[i]);
    }
}

// Copy index to another given position in a vector
void bpickle_aux(std::vector<uint8_t>& v, size_t pos, uint64_t ref) {
    for (size_t i = 0; i < sizeof(ref); ++i) {
        v[pos + i] = reinterpret_cast<const uint8_t*>(&ref)[i];
    }
}

// Copy current size of vector to another given position in the vector
void bpickle_aux(std::vector<uint8_t>& v, size_t pos) {
    bpickle_aux(v, pos, v.size());
}

void bpickle(boxed exp,
             std::vector<uint64_t>& refs,
             std::vector<uint8_t>& v,
             size_t vec_offset) {
    if (exp->contains<bool>()) {
        v.push_back(exp->cast<bool>() ? true_code : false_code);
    } else if (exp->contains<double>()) {
        v.push_back(number_code);
        bpickle_aux(exp->cast<double>(), v);
    } else if (exp->contains<binary>()) {
        v.push_back(binary_code);
        auto bin = exp->cast<binary>();
        bpickle_aux(static_cast<uint64_t>(bin->size()), v);
        for (auto const& b : *bin) {
            bpickle_aux(b, v);
        }
    } else if (exp->contains<vector>()) {
        auto vec = exp->cast<vector>();
        auto size = (*vec)->size();
        if (vec_offset == 0) {
            if (size != 0) {
                refs.push_back(static_cast<uint64_t>(v.size()));
            }
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

template<typename Input>
void bconvert_out(Input& s, const RuntimeInfo& info) {
    auto exp = serialize(unmemoize(mce_restore(s, info)))->template cast<vector>();

    std::vector<uint64_t> refs;
    std::vector<uint8_t> v;
    bpickle((*exp)->at(0), refs, v, 0);

    const auto nrefs = (*exp)->at(1)->template cast<double>();
    if (refs.size() != nrefs) {
        //std::cerr << refs.size() << " " << nrefs << std::endl;
        throw std::length_error("unexpected number of references");
    }

    auto size = static_cast<uint64_t>(v.size());
    std::cout.write(reinterpret_cast<const char*>(&size), sizeof(size));
    std::cout.write(reinterpret_cast<const char*>(v.data()), v.size());
}

boxed bunpickle(const uint8_t *s,
                const size_t len,
                const uint64_t i,
                std::unordered_map<uint64_t, boxed>& refs,
                const RuntimeInfo& info);

boxed bunpickle(const uint8_t *s,
                const size_t len,
                const uint64_t i,
                boxed mark,
                std::unordered_map<uint64_t, boxed>& refs,
                const RuntimeInfo& info) {
    auto r = make_vector(info);
    refs[i] = r;
    auto v = r->cast<vector>();
    (*v)->push_back(mark);
    assert(i + 8 < len);
    for (auto el : **bunpickle(s, len, *(uint64_t*)&s[i + 1], refs, info)->cast<vector>()) {
        (*v)->push_back(el);
    }
    return r;
}

boxed bunpickle(const uint8_t *s,
                const size_t len,
                const uint64_t i,
                std::unordered_map<uint64_t, boxed>& refs,
                const RuntimeInfo& info ) {
    auto ref = refs.find(i);
    if (ref != refs.end()) {
        return ref->second;
    }

    assert(i < len);
    switch (s[i]) {
        case true_code:
            return box<bool>(true, info);

        case false_code:
            return box<bool>(false, info);

        case number_code:
            assert(i + sizeof(double) < len);
            return box<double>(*(double*)&s[i + 1], info);

        case binary_code: {
            assert(i + 8 < len);
            const auto size = *(uint64_t*)&s[i + 1];
            assert(i + 8 + size < len);
            return box<binary>(std::make_shared<binary>(&s[i + 9], &s[i + 9 + size]), info);
        }

        case vector_code: {
            auto r = make_vector(info);
            refs[i] = r;
            auto v = r->cast<vector>();
            assert(i + 8 < len);
            for (uint64_t j = 0; j < *(uint64_t*)&s[i + 1]; ++j) {
                const auto pos = i + 9 + j * 8;
                assert(pos + 8 <= len);
                (*v)->push_back(bunpickle(s, len, *(uint64_t*)&s[pos], refs, info));
            }
            return r;    
        }

        case unmemoized_code:
            return bunpickle(s, len, i, box<binary>(unmemoized_mark, info), refs, info);

        case result_code:
            return bunpickle(s, len, i, box<binary>(result_mark, info), refs, info);

        case step_contn_code:
            return bunpickle(s, len, i, box<binary>(step_contn_mark, info), refs, info);

        case transfer_code:
            return bunpickle(s, len, i, box<binary>(transfer_mark, info), refs, info);

        default:
            throw std::range_error("unknown bunpickle expression");
    }
}

void bconvert_in(const std::string& s, const RuntimeInfo& info) {
    std::unordered_map<uint64_t, boxed> refs;
    std::cout << mce_save(bunpickle(
        &reinterpret_cast<const uint8_t*>(s.c_str())[8], s.size(), 0, refs, info));
}

void bconvert_in(std::istream &stream, const RuntimeInfo& info) {
    uint64_t size;
    stream.read(reinterpret_cast<char*>(&size), sizeof(size));

    std::vector<uint8_t> v(size);
    stream.read(reinterpret_cast<char*>(v.data()), size);

    std::unordered_map<uint64_t, boxed> refs;
    std::cout << mce_save(bunpickle(v.data(), v.size(), 0, refs, info));
}

boxed start(const int argc, const char *const argv[]) {
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
         cxxopts::value<std::vector<std::string>>())
        ("bconvert-out",
         "Convert CPS form or state to binary format",
         cxxopts::value<std::string>()->implicit_value(""))
        ("bconvert-in",
         "Convert binary format form or state to CPS",
         cxxopts::value<std::string>()->implicit_value(""));
    auto opts = options.parse(argc, argv);
    RuntimeInfo info;
    if (opts.count("help")) {
        std::cout << options.help() << std::endl;
        return info.get_nil();
    }
    const auto runtime = info.get_runtime();
    runtime->set_gc_threshold(opts["gc-threshold"].as<size_t>());
    if (opts.count("config")) {
        for (auto kv : opts["config"].as<std::vector<std::string>>()) {
            auto pos = kv.find('=');
            auto val = kv.substr(pos + 1);
            runtime->set_config(kv.substr(0, pos), mce_restore(val, info));
        }
    }
    if (opts.count("bconvert-out")) {
        auto state = opts["bconvert-out"].as<std::string>();
        if (state.empty()) {
            bconvert_out(std::cin, info);
        } else {
            bconvert_out(state, info);
        }
        return info.get_nil();
    }
    if (opts.count("bconvert-in")) {
        auto state = opts["bconvert-in"].as<std::string>();
        if (state.empty()) {
            bconvert_in(std::cin, info);
        } else {
            bconvert_in(state, info);
        }
        return info.get_nil();
    }
    if (opts.count("run")) {
        return start<const std::string>(opts["run"].as<std::string>(), info.get_nil());
    }
    return start<std::istream>(std::cin, info.get_nil());
}

boxed cf_test(boxed args) {
    auto n = vlist_ref(args, 0)->cast<double>();
    auto x = vlist_ref(args, 1);
    if (n == 0) {
        return make_lambda<boxed>([x](boxed args) {
            return cf_test(vc(vlist_ref(args, 0), vc(x, args->get_runtime_info().get_nil())));
        }, args->get_runtime_info());
    }
    return box<double>(x->cast<double>() + n, args->get_runtime_info());
}

boxed transfer_test(boxed args) {
    return applyx(lookup_global(args->get_runtime_info().get_runtime()->g_result, args),
                  make_global_rtenv(args),
                  vlist_ref(args, 0),
                  vlist_rest(args, 0));
}

Runtime::Runtime() :
    gc_threshold(10000),
    calling_gc_callback(false),
    global_table {
        { "save", save },
        { "restore", restore },
        { "getpid", getpid },
        { "get-config", mce::get_config },
        { "cf-test", cf_test },
        { "set-gc-callback!", mce::set_gc_callback },
        { "output-binary-to-stdout", output_binary_to_stdout },
        { "output-binary-to-stderr", output_binary_to_stderr }
    },
    core_globals {
        result,
        gapplyx,
        is_boolean,
        is_number,
        less_than,
        greater_than,
        add,
        subtract,
        multiply,
        divide,
        is_number_equal,
        gfloor,
        make_vector,
        is_vector,
        vector_length,
        vector_ref,
        vector_set,
        is_procedure,
        make_binary,
        is_binary,
        binary_length,
        binary_ref,
        binary_set,
        error,
        is_same_object,
        transfer,
        transfer_test
    },
    kenvfn_set {
        gapplyx,
        transfer,
        restore
    },
    g_result(std::find(core_globals.begin(), core_globals.end(), result) -
             core_globals.begin()) {}

void Runtime::set_gc_threshold(size_t v) {
    gc_threshold = v;
}

RuntimeInfo::RuntimeInfo(std::shared_ptr<Runtime> runtime, boxed nil) :
    runtime(runtime),
    nil(nil) {}

RuntimeInfo::RuntimeInfo() :
    runtime(std::make_shared<Runtime>()),
    nil(make_vector(RuntimeInfo(runtime, nullptr))) {}

}
