#include <sys/types.h>
#include <unistd.h>
#include <iostream>
#include <utility>
#include "mce.hpp"
#include "json.hpp"
#include "cxxopts.hpp"

using nlohmann::json;

const char null_code    = 'a';
const char boolean_code = 'b';
const char number_code  = 'c';
const char char_code    = 'd';
const char string_code  = 'e';
const char symbol_code  = 'f';
const char pair_code    = 'g';
const char vector_code  = 'h';

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

std::string mce_save(boxed exp);
boxed mce_restore(const std::string& s, std::shared_ptr<Runtime> runtime);

boxed Runtime::maybe_gc(boxed state) {
    if ((allocated_pairs.size() > gc_threshold) ||
        (allocated_vectors.size() > gc_threshold) ||
        (allocated_functions.size() > gc_threshold)) {
        auto saved = mce_save(state);

        std::unordered_set<std::shared_ptr<pair>> ps;
        for (auto const& entry : allocated_pairs) {
            if (auto p = entry.second.lock()) {
                ps.insert(p);
            }
        }
        for (auto const& p : ps) {
            p->first = nullptr;
            p->second = nullptr;
        }
        allocated_pairs.clear();

        std::unordered_set<std::shared_ptr<vector>> vs;
        for (auto const& entry : allocated_vectors) {
            if (auto v = entry.second.lock()) {
                vs.insert(v);
            }
        }
        for (auto const& v : vs) {
            v->clear();
        }
        allocated_vectors.clear();

        std::unordered_set<lambda> fs;
        for (auto const& entry : allocated_functions) {
            if (auto f = entry.second.lock()) {
                fs.insert(f);
            }
        }
        for (auto const& f : fs) {
            *f = nullptr;
        }
        allocated_functions.clear();

        state = mce_restore(saved, state->get_runtime());
    }
    return state;
}

boxed cons(boxed car, boxed cdr) {
    auto runtime = car->get_runtime();
    auto p = std::shared_ptr<pair>(new pair(car, cdr), [runtime](auto pptr) {
        runtime->allocated_pairs.erase(pptr);
        delete pptr;
    });
    runtime->allocated_pairs[p.get()] = p;
    return box<std::shared_ptr<pair>>(p, runtime);
}

boxed make_vector(std::shared_ptr<Runtime> runtime) {
    auto v = std::shared_ptr<vector>(new vector(), [runtime](auto vptr) {
        runtime->allocated_vectors.erase(vptr);
        delete vptr;
    });
    runtime->allocated_vectors[v.get()] = v;
    return box<std::shared_ptr<vector>>(v, runtime);
}

template<>
lambda make_lambda<lambda>(func fn, std::shared_ptr<Runtime> runtime) {
    auto f = std::shared_ptr<func>(new func(fn), [runtime](auto fptr) {
        runtime->allocated_functions.erase(fptr);
        delete fptr;
    });
    runtime->allocated_functions[f.get()] = f;
    return f;
}

template<>
boxed make_lambda<boxed>(func fn, std::shared_ptr<Runtime> runtime) {
    return box<lambda>(make_lambda<lambda>(fn, runtime), runtime);
}

boxed list_ref(boxed l, size_t i) {
    while (i > 0) {
        l = l->cast<pair>()->second;
        --i;
    }

    return l->cast<pair>()->first;
}

boxed list_rest(boxed l, size_t i) {
    while (i > 0) {
        l = l->cast<pair>()->second;
        --i;
    }

    return l->cast<pair>()->second;
}

boxed list_to_vector(boxed l) {
    auto a = make_vector(l->get_runtime());
    auto v = a->cast<vector>();

    while (l->contains<pair>()) {
        auto p = l->cast<pair>();
        v->push_back(p->first);
        l = p->second;
    }

    return a;
}

boxed vector_to_list(boxed vec) {
    auto v = vec->cast<vector>();
    auto l = box(vec->get_runtime());
    for (auto it = v->rbegin(); it != v->rend(); ++it) {
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
    ep->first = f(lp->first);
    ep->second = f(lp->second);

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

    for (auto el : *vv) {
        ev->push_back(f(el));
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
    return (*proc->cast<lambda>())(
        cons(box<symbol>("MCE-YIELD-DEFINITION", runtime), box(runtime)));
}

boxed xdisplay(boxed args, std::ostream& out, bool is_write) {
    auto exp = list_ref(args, 0);
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
            auto& s = *exp->cast<std::string>();
            if (is_write) {
                json j = s;
                out << j.dump();
            } else {
                out << s;
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
                xdisplay(cons(p->first, bnil), out, is_write);
                exp = p->second;
            }
            if (!exp->empty()) {
                out << " . ";
                xdisplay(cons(exp, bnil), out, is_write);
            }
            out << ")";
        } else if (exp->contains<vector>()) {
            auto bnil = box(exp->get_runtime());
            bool first = true;
            out << "#(";
            auto vec = exp->cast<vector>();
            for (auto v : *vec) {
                if (!first) {
                    out << " ";
                }
                first = false;
                xdisplay(cons(v, bnil), out, is_write);
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

boxed xprint(boxed args, std::ostream& out) {
    auto bnil = box(args->get_runtime());
    auto r = bnil;
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r = p->first;
        xdisplay(cons(r, bnil), out, false);
        args = p->second;
    }
    out << std::endl;
    return r;
}

boxed print(boxed args) {
    return xprint(args, std::cout);
}

boxed eprint(boxed args) {
    return xprint(args, std::cerr);
}

boxed xwrite(boxed args, std::ostream& out) {
    auto bnil = box(args->get_runtime());
    auto r = bnil;
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r = p->first;
        xdisplay(cons(r, bnil), out, true);
        args = p->second;
    }
    return r;
}

boxed write(boxed args) {
    return xwrite(args, std::cout);
}

boxed ewrite(boxed args) {
    return xwrite(args, std::cerr);
}

boxed unmemoize(boxed exp);
boxed serialize(boxed exp);

boxed memoize_lambda(lambda proc, boxed defn) {
    return make_lambda<boxed>([proc, defn](boxed args) -> boxed {
        if (is_yield_defn(args)) {
            if (defn->contains<lambda>()) {
                return (*defn->cast<lambda>())(box(args->get_runtime()));
            }
            return defn;
        }
        //print(cons(serialize(unmemoize(defn)), bnil));
        return (*proc)(args);
    }, defn->get_runtime());
}

boxed memoize_lambda(boxed proc, boxed defn) {
    return memoize_lambda(proc->cast<lambda>(), defn);
}

boxed send(boxed k, boxed v) {
    return cons(k, v);
}

boxed ctenv_lookup(boxed i, boxed env) {
    auto ip = i->cast<pair>();
    auto first = ip->first->cast<double>();
    auto second = ip->second->cast<double>();
    auto bindings = list_ref(env, first)->cast<pair>();
    auto v = bindings->second->cast<vector>();
    if (second < v->size()) {
        return (*v)[second];
    }
    return box(i->get_runtime());
}

boxed ctenv_setvar(boxed name, boxed i, boxed val, boxed env) {
    auto ip = i->cast<pair>();
    auto first = ip->first->cast<double>();
    auto second = ip->second->cast<double>();
    auto bindings = list_ref(env, first)->cast<pair>();
    auto v = bindings->second->cast<vector>();
    auto bnil = box(i->get_runtime());

    if (second >= v->size()) {
        v->resize(second + 1, bnil);
    }
    (*v)[second] = val;

    if (bindings->first->empty()) {
        bindings->first = cons(bnil, bnil);
    }
    auto p = bindings->first->cast<pair>();
    while (second > 0) {
        if (p->second->empty()) {
            p->second = cons(bnil, bnil);
        }
        p = p->second->cast<pair>();
        --second;
    }
    p->first = name;

    return val;
}

boxed symbol_lookup(boxed args) {
    auto i = list_ref(args, 1);
    return make_lambda<boxed>([i](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return send(k, ctenv_lookup(i, env));
    }, args->get_runtime());
}

boxed send_value(boxed args) {
    auto exp = list_ref(args, 1);
    return make_lambda<boxed>([exp](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        return send(k, exp);
    }, args->get_runtime());
}

boxed make_step_contn(boxed k, boxed args) {
    return cons(box<symbol>("MCE-STEP-CONTN", k->get_runtime()), cons(k, args));
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

boxed step_contn_args(boxed args) {
    return list_rest(args, 1);
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
    return (*fn->cast<lambda>())(
        cons(box<symbol>("MCE-TRANSFER", args->get_runtime()), list_rest(args, 2)));
}

boxed make_global_env(std::shared_ptr<Runtime> runtime) {
    auto values = make_vector(runtime);
    auto bnil = box(runtime);
    auto bindings = cons(bnil, values);
    return cons(bindings, bnil);
}

boxed make_env_args(boxed env, boxed args) {
    return cons(box<symbol>("MCE-ENV-ARGS", env->get_runtime()), cons(env, args));
}

bool is_env_args(boxed args) {
    if (!args->contains<pair>()) {
        return false;
    }
    auto first = list_ref(args, 0);
    return first->contains<symbol>() &&
           (*first->cast<symbol>() == "MCE-ENV-ARGS");
}

boxed env_args_env(boxed args) {
    return is_env_args(args) ? list_ref(args, 1) : make_global_env(args->get_runtime());
}

boxed env_args_args(boxed args) {
    return is_env_args(args) ? list_rest(args, 1) : args;
}

boxed applyx(boxed k, boxed env, boxed fn, boxed args) {
    return (*fn->cast<lambda>())(
        make_step_contn(k, make_env_args(env, args)));
}

boxed result(boxed exp) {
    auto runtime = exp->get_runtime();
    auto a = make_vector(runtime);
    auto v = a->cast<vector>();
    v->push_back(box<symbol>("MCE-RESULT", runtime));
    v->push_back(exp);
    return a;
}

bool is_result(boxed exp) {
    if (!exp->contains<vector>()) {
        return false;
    }
    auto v = exp->cast<vector>();
    return (v->size() == 2) &&
           (*v)[0]->contains<symbol>() &&
           (*(*v)[0]->cast<symbol>() == "MCE-RESULT");
}

boxed result_val(boxed exp) {
    return (*exp->cast<vector>())[1];
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
        r += p->first->cast<double>();
        args = p->second;
    }
    return box<double>(r, args->get_runtime());
}

boxed multiply(boxed args) {
    double r = 1;
    while (args->contains<pair>()) {
        auto p = args->cast<pair>();
        r *= p->first->cast<double>();
        args = p->second;
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
    return box<double>(static_cast<double>(a->cast<vector>()->size()),
                       args->get_runtime());
}

boxed vector_ref(boxed args) {
    auto a = list_ref(args, 0);
    auto i = list_ref(args, 1);
    return a->cast<vector>()->at(i->cast<double>());
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
    p->cast<pair>()->first = v;
    return p;
}

boxed set_cdr(boxed args) {
    auto p = list_ref(args, 0);
    auto v = list_ref(args, 1);
    p->cast<pair>()->second = v;
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
    auto a = list_ref(args, 0);
    return mce_restore(*a->cast<std::string>(), args->get_runtime());
}

boxed getpid(boxed args) {
    return box<double>(static_cast<double>(getpid()), args->get_runtime());
}

boxed gcons(boxed args) {
    return cons(list_ref(args, 0), list_ref(args, 1));
}

void Runtime::set_config(const std::string& k, boxed v) {
    config_table[k] = v;
}

boxed get_config(boxed args) {
    auto runtime = args->get_runtime();
    auto it = runtime->config_table.find(*list_ref(args, 0)->cast<std::string>());
    if (it == runtime->config_table.end()) {
        return box<bool>(false, runtime);
    }
    return it->second;
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
    return (*list_ref(state, 0)->cast<lambda>())(
        cons(list_rest(state, 0), box(state->get_runtime())));
}

boxed run(boxed state) {
    while (!is_result(state)) {
        state = state->get_runtime()->maybe_gc(step(state));
    }

    return result_val(state);
}

boxed globalize(boxed x, boxed args, boxed cf);

boxed handle_global_lambda(boxed args, boxed fn, boxed cf) {
    auto f = fn->cast<lambda>();

    if (is_transfer(args)) {
        return (*f)(args);
    }

    if (is_step_contn(args)) {
        auto sck = step_contn_k(args);
        auto sca = step_contn_args(args);
        auto eaa = env_args_args(sca);
        return send(sck, globalize((*f)(eaa), eaa, cf));
    }

    auto eaa = env_args_args(args);
    return globalize((*f)(eaa), eaa, cf);
}

boxed lookup_global(const symbol& sym, std::shared_ptr<Runtime> runtime);

boxed handle_global_lambda_kenv(boxed args, boxed fn) {
    auto f = fn->cast<lambda>();

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return (*f)(cons(step_contn_k(args),
                         cons(env_args_env(sca),
                              env_args_args(sca))));
    }

    return run((*f)(cons(lookup_global(symbol("result"), args->get_runtime()),
                         cons(env_args_env(args),
                              env_args_args(args)))));
}

boxed wrap_global_lambda(boxed fn, boxed cf) {
    const lambda l = fn->cast<lambda>();
    auto p = l->target<function*>();
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
    vector s;
    auto av = make_vector(env->get_runtime());
    auto v = av->cast<vector>();

    while (!syms->empty()) {
        if (syms->contains<symbol>()) {
            s.push_back(syms);
            v->push_back(values);
            break;
        }
        if (values->empty()) {
            break;
        }

        s.push_back(list_ref(syms, 0));
        v->push_back(list_ref(values, 0));
        
        syms = list_rest(syms, 0);
        values = list_rest(values, 0);
    }

    auto as = box(env->get_runtime());
    for (auto it = s.rbegin(); it != s.rend(); ++it) {
        as = cons(*it, as);
    }

    return cons(cons(as, av), env);
}

boxed handle_lambda(boxed args,
                    boxed params,
                    boxed fn,
                    boxed env,
                    extend_env_fn extend_env) {
    auto f = fn->cast<lambda>();
    auto runtime = args->get_runtime();
    auto bnil = box(runtime);

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return (*f)(cons(step_contn_k(args),
                         cons(extend_env(env, params, env_args_args(sca)),
                              bnil)));
    }

    return run((*f)(cons(lookup_global(symbol("result"), runtime),
                         cons(extend_env(env, params, env_args_args(args)),
                              bnil))));
}

boxed handle_contn_lambda(boxed args, boxed k) {
    auto f = k->cast<lambda>();

    if (is_transfer(args)) {
        return (*f)(env_args_args(transfer_args(args)));
    }

    if (is_step_contn(args)) {
        return (*f)(env_args_args(step_contn_args(args)));
    }

    return run((*f)(env_args_args(args)));
}

boxed constructed_function(boxed args) {
    auto self = list_ref(args, 0);
    auto args2 = list_ref(args, 1);
    auto cf = list_ref(args, 2);
    auto r = (*cf->cast<lambda>())(args2);
    if (r->contains<lambda>()) {
        return wrap_global_lambda(r, self);
    }
    return r;
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
        return (*scanned->cast<lambda>())(cons(k, cons(env, box(k->get_runtime()))));
    }, args->get_runtime());
}

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
    constructed_function,
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
        return (*f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(forms[n->cast<double>()](cons(f2, args)), defn);

    return f;
}

boxed make_form(enum forms n, boxed args) {
    return make_form(box<double>(static_cast<double>(n), args->get_runtime()), args);
}

boxed make_form(boxed args) {
    auto p = args->cast<pair>();
    return make_form(p->first, p->second);
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
        return (*f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

boxed globalize(boxed x, boxed args, boxed cf) {
    if (!x->contains<lambda>()) {
        return x;
    }

    auto runtime = x->get_runtime();
    auto defn = cons(aform(forms::constructed_function, runtime),
                     cons(args, cons(cf, box(runtime))));
    auto f = box(runtime);
    auto f2 = memoize_lambda(make_lambda<lambda>([f](boxed args) -> boxed {
        return (*f->cast<lambda>())(args);
    }, runtime), defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

boxed if1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scan1 = list_ref(args, 3);
    auto scan2 = list_ref(args, 4);
    return make_lambda<boxed>([k, env, scan1, scan2](boxed args) -> boxed {
        auto v = list_ref(args, 0)->cast<bool>();
        auto f = (v ? scan1 : scan2)->cast<lambda>();
        return (*f)(cons(k, cons(env, box(args->get_runtime()))));
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
        return (*scan0->cast<lambda>())(
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
        return send(k, cons(v, w));
    }, args->get_runtime());
}

boxed sclis1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return make_lambda<boxed>([k, env, rest](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        auto bnil = box(args->get_runtime());
        return (*rest->cast<lambda>())(
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
        return (*first->cast<lambda>())(
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
        return (*rest->cast<lambda>())(cons(k, cons(env, box(k->get_runtime()))));
    }, args->get_runtime());
}

boxed scseq0(boxed args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return make_lambda<boxed>([first, rest](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return (*first->cast<lambda>())(
            cons(make_form(forms::scseq1,
                           cons(k, cons(env, cons(rest, bnil)))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

boxed lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return make_lambda<boxed>([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, extend_env);
    }, args->get_runtime());
}

boxed lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda<boxed>([params, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(k,
                    make_form(forms::lambda1,
                              cons(params, cons(scanned, cons(env, bnil)))));
    }, args->get_runtime());
}

boxed improper_lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return make_lambda<boxed>([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, improper_extend_env);
    }, args->get_runtime());
}

boxed improper_lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda<boxed>([params, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        auto bnil = box(args->get_runtime());
        return send(k,
                    make_form(forms::improper_lambda1,
                              cons(params, cons(scanned, cons(env, bnil)))));
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
        return (*scanned->cast<lambda>())(
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
        return send(k, ctenv_setvar(name, i, v, env));
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
        return (*scanned->cast<lambda>())(
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
        return (*scanned->cast<lambda>())(
            cons(make_form(forms::application1, cons(k, cons(env, bnil))),
                 cons(env, bnil)));
    }, args->get_runtime());
}

bool is_unmemoized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           (*v)[0]->contains<symbol>() &&
           (*(*v)[0]->cast<symbol>() == "MCE-UNMEMOIZED");
}

boxed unmemoized_repexp(std::shared_ptr<vector> v) {
    return (*v)[1];
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
                    return (*f->cast<lambda>())(args);
                }, runtime),
                make_lambda<boxed>([r](boxed) -> boxed {
                    return r;
                }, runtime)));
            *r = *fn(repexp);
            *f = Box(make_lambda<lambda>([r, f](boxed args) -> boxed {
                    *f = *make_form(r);
                    return (*f->cast<lambda>())(args);
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

        v->push_back(box<symbol>("MCE-UNMEMOIZED", runtime));
        v->push_back(fn(get_procedure_defn(exp)));

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

    v->push_back(box<symbol>("MCE-SERIALIZED", runtime));
    v->push_back(box<double>(static_cast<double>(n), runtime));

    return a;
}

bool is_serialized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           (*v)[0]->contains<symbol>() &&
           (*(*v)[0]->cast<symbol>() == "MCE-SERIALIZED");
}

boxed serialized_n(std::shared_ptr<vector> v) {
    return (*v)[1];
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
    return fn(exp);
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
    return fn(exp);
}

json pickle_aux(boxed exp) {
    json j;
    if (exp->empty()) {
        j.push_back(std::string(1, null_code));
    } else {
        if (exp->contains<bool>()) {
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
            j.push_back(pickle_aux(p->first));
            j.push_back(pickle_aux(p->second));
        } else if (exp->contains<vector>()) {
            j.push_back(std::string(1, vector_code));
            j.push_back(pickle_aux(vector_to_list(exp)));
        } else {
            throw std::range_error("unknown pickle expression");
        }
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
    case vector_code:
        return list_to_vector(unpickle_aux(j[1], runtime));
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

boxed start(const json& j, std::shared_ptr<Runtime> runtime) {
    auto r = mce_restore(j.get<std::string>(), runtime);
    if (r->contains<lambda>()) {
        auto bnil = box(runtime);
        return (*r->cast<lambda>())(cons(bnil, bnil));
    }
    return run(r);
}

boxed start(std::istream &stream, std::shared_ptr<Runtime> runtime) {
    json s;
    stream >> s;
    return start(s, runtime);
}

boxed start(const std::string& s, std::shared_ptr<Runtime> runtime) {
    return start(json::parse(s), runtime);
}

boxed start(int argc, char *argv[], std::shared_ptr<Runtime> runtime) {
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
         cxxopts::value<std::string>());
    auto opts = options.parse(argc, argv);
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
        return start(opts["run"].as<std::string>(), runtime);
    }
    return start(std::cin, runtime);
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
        { "*", multiply },
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
        { "get-config", get_config }
    },
    kenvfn_set({
        gapplyx,
        transfer
    }) {}

void Runtime::set_gc_threshold(size_t v) {
    gc_threshold = v;
}
