#include <iostream>
#include <memory>
#include <utility>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <boost/any.hpp>
#include "json.hpp"

using nlohmann::json;
using boost::any;
using boost::any_cast;

const char null_code    = 'a';
const char boolean_code = 'b';
const char number_code  = 'c';
const char char_code    = 'd';
const char string_code  = 'e';
const char symbol_code  = 'f';
const char pair_code    = 'g';
const char vector_code  = 'h';

class symbol : public std::string {
public:
    symbol(const std::string& s) : std::string(s) {}
};

typedef std::pair<std::shared_ptr<any>, std::shared_ptr<any>> pair;
typedef std::vector<std::shared_ptr<any>> vector;
typedef std::unordered_map<std::shared_ptr<any>,
                           std::shared_ptr<any>> cmap_table;
typedef std::function<std::shared_ptr<any>(std::shared_ptr<any>)> map_fn;
typedef std::function<std::shared_ptr<any>(cmap_table&,
                                           std::shared_ptr<any>,
                                           std::shared_ptr<any>)> set_entry_fn;
typedef std::shared_ptr<any> function(std::shared_ptr<any>);
typedef std::function<function> lambda;
typedef std::shared_ptr<any> extend_env_fn(std::shared_ptr<any> env,
                                           std::shared_ptr<any> syms,
                                           std::shared_ptr<any> values);

const auto nil = std::make_shared<any>();

std::shared_ptr<any> make_symbol(const std::string& s) {
    return std::make_shared<any>(std::make_shared<symbol>(s));
}

std::shared_ptr<any> make_string(const std::string& s) {
    return std::make_shared<any>(std::make_shared<std::string>(s));
}

std::shared_ptr<any> cons(std::shared_ptr<any> car, std::shared_ptr<any> cdr) {
    return std::make_shared<any>(std::make_shared<pair>(car, cdr));
}

std::shared_ptr<any> list_ref(std::shared_ptr<any> l, size_t i) {
    while (i > 0) {
        l = any_cast<std::shared_ptr<pair>>(*l)->second;
        --i;
    }

    return any_cast<std::shared_ptr<pair>>(*l)->first;
}

std::shared_ptr<any> list_rest(std::shared_ptr<any> l, size_t i) {
    while (i > 0) {
        l = any_cast<std::shared_ptr<pair>>(*l)->second;
        --i;
    }

    return any_cast<std::shared_ptr<pair>>(*l)->second;
}

std::shared_ptr<any> list_to_vector(std::shared_ptr<any> l) {
    auto a = std::make_shared<any>(std::make_shared<vector>());
    auto v = any_cast<std::shared_ptr<vector>>(*a);

    while (!l->empty()) {
        auto p = any_cast<std::shared_ptr<pair>>(*l);
        v->push_back(p->first);
        l = p->second;
    }

    return a;
}

std::shared_ptr<any> cmap(map_fn f,
                          std::shared_ptr<any> l,
                          cmap_table& tab,
                          set_entry_fn set_entry) {
    if (l->empty()) {
        return l;
    }

    if (l->type() == typeid(std::shared_ptr<pair>)) {
        auto ref = tab.find(l);
        if (ref != tab.end()) {
            return ref->second;
        }

        auto entry = cons(nil, nil);
        set_entry(tab, l, entry);

        auto ep = any_cast<std::shared_ptr<pair>>(*entry);
        auto lp = any_cast<std::shared_ptr<pair>>(*l);
        ep->first = f(lp->first);
        ep->second = cmap(f, lp->second, tab, set_entry);

        return entry;
    }

    return f(l);
}

std::shared_ptr<any> vector_cmap(map_fn f,
                                 std::shared_ptr<any> v,
                                 cmap_table& tab,
                                 set_entry_fn set_entry) {
    auto ref = tab.find(v);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto entry = std::make_shared<any>(std::make_shared<vector>());
    set_entry(tab, v, entry);

    auto ev = any_cast<std::shared_ptr<vector>>(*entry);
    auto vv = any_cast<std::shared_ptr<vector>>(*v);

    for (auto el : *vv) {
        ev->push_back(f(el));
    }

    return entry;
}

std::shared_ptr<any> table_set(cmap_table& tab,
                               std::shared_ptr<any> v,
                               std::shared_ptr<any> entry)
{
    tab[v] = entry;
    return entry;
}

bool is_yield_defn(std::shared_ptr<any> args) {
    if (args->type() != typeid(std::shared_ptr<pair>)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return (first->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*first) ==
            "MCE-YIELD-DEFINITION");
}

std::shared_ptr<any> memoize_lambda(lambda proc, std::shared_ptr<any> defn) {
    return std::make_shared<any>(lambda(
        [proc, defn](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            if (is_yield_defn(args)) {
                return defn;
            }
            return proc(args);
        }));
}

std::shared_ptr<any> memoize_lambda(std::shared_ptr<any> proc,
                                    std::shared_ptr<any> defn) {
    return memoize_lambda(any_cast<lambda>(*proc), defn);
}

std::shared_ptr<any> send(std::shared_ptr<any> k, std::shared_ptr<any> v) {
    return cons(k, v);
}

std::shared_ptr<any> ctenv_lookup(std::shared_ptr<any> i,
                                  std::shared_ptr<any> env) {
    auto ip = any_cast<std::shared_ptr<pair>>(*i);
    auto first = any_cast<double>(*ip->first);
    auto second = any_cast<double>(*ip->second);
    auto bindings = any_cast<std::shared_ptr<pair>>(*list_ref(env, first));
    auto v = any_cast<std::shared_ptr<vector>>(*bindings->second);
    return (*v)[second];
}

std::shared_ptr<any> symbol_lookup(std::shared_ptr<any> args) {
    auto i = list_ref(args, 1);
    return std::make_shared<any>(lambda(
        [i](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return send(k, ctenv_lookup(i, env));
        }));
}

std::shared_ptr<any> send_value(std::shared_ptr<any> args) {
    auto exp = list_ref(args, 1);
    return std::make_shared<any>(lambda(
        [exp](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            return send(k, exp);
        }));
}

std::shared_ptr<any> make_step_contn(std::shared_ptr<any> k,
                                     std::shared_ptr<any> args) {
    return cons(make_symbol("MCE-STEP_CONTN"), cons(k, args));
}

bool is_step_contn(std::shared_ptr<any> args) {
    if (args->type() != typeid(std::shared_ptr<pair>)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return (first->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*first) ==
            "MCE-STEP-CONTN");
}

std::shared_ptr<any> step_contn_k(std::shared_ptr<any> args) {
    return list_ref(args, 1);
}

std::shared_ptr<any> step_contn_args(std::shared_ptr<any> args) {
    return list_rest(args, 1);
}

bool is_transfer(std::shared_ptr<any> args) {
    if (args->type() != typeid(std::shared_ptr<pair>)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return (first->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*first) ==
            "MCE-TRANSFER");
}

std::shared_ptr<any> transfer_args(std::shared_ptr<any> args) {
    return list_rest(args, 0);
}

std::shared_ptr<any> transfer(std::shared_ptr<any> args) {
    auto k = list_ref(args, 0);
    auto fn = list_ref(args, 2);
    return any_cast<lambda>(*fn)(make_step_contn(k,
        cons(make_symbol("MCE-TRANSFER"), list_rest(args, 2))));
}

std::shared_ptr<any> make_global_env() {
    auto values = std::make_shared<any>(std::make_shared<vector>());
    auto bindings = cons(nil, values);
    return cons(bindings, nil);
}

std::shared_ptr<any> make_env_args(std::shared_ptr<any> env,
                                   std::shared_ptr<any> args) {
    return cons(make_symbol("MCE-ENV-ARGS"), cons(env, args));
}

bool is_env_args(std::shared_ptr<any> args) {
    if (args->type() != typeid(std::shared_ptr<pair>)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return (first->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*first) ==
            "MCE-ENV-ARGS");
}

std::shared_ptr<any> env_args_env(std::shared_ptr<any> args) {
    return is_env_args(args) ? list_ref(args, 1) : make_global_env();
}

std::shared_ptr<any> env_args_args(std::shared_ptr<any> args) {
    return is_env_args(args) ? list_rest(args, 1) : args;
}

std::shared_ptr<any> result(std::shared_ptr<any> exp) {
    auto a = std::make_shared<any>(std::make_shared<vector>());
    auto v = any_cast<std::shared_ptr<vector>>(*a);
    v->push_back(make_symbol("MCE-RESULT"));
    v->push_back(exp);
    return a;
}

bool is_result(std::shared_ptr<any> exp) {
    if (exp->type() != typeid(std::shared_ptr<vector>)) {
        return false;
    }
    auto v = any_cast<std::shared_ptr<vector>>(*exp);
    return (v->size() == 2) &&
           ((*v)[0]->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*(*v)[0]) == "MCE-RESULT");
}

std::shared_ptr<any> result_val(std::shared_ptr<any> exp) {
    return (*any_cast<std::shared_ptr<vector>>(*exp))[1];
}

std::unordered_map<std::string, function*> global_table {
    { "result", result }
};

std::unordered_set<function*> kenvfn_table;

std::shared_ptr<any> find_global(const symbol& sym) {
    return std::make_shared<any>(lambda(global_table.at(sym)));
}

std::shared_ptr<any> step(std::shared_ptr<any> state) {
    return any_cast<lambda>(*list_ref(state, 0))(list_rest(state, 1));
}

std::shared_ptr<any> run(std::shared_ptr<any> state) {
    while (!is_result(state)) {
        state = step(state);
    }

    return result_val(state);
}

std::shared_ptr<any> globalize(std::shared_ptr<any> x,
                               std::shared_ptr<any> args,
                               std::shared_ptr<any> cf);

std::shared_ptr<any> handle_global_lambda(std::shared_ptr<any> args,
                                          std::shared_ptr<any> fn,
                                          std::shared_ptr<any> cf) {
    auto f = any_cast<lambda>(*fn);

    if (is_step_contn(args)) {
        auto sck = step_contn_k(args);
        auto sca = step_contn_args(args);
        if (is_transfer(sca)) {
            return f(make_step_contn(sck, transfer_args(sca)));
        }
        auto eaa = env_args_args(sca);
        return send(sck, globalize(f(eaa), eaa, cf));
    }

    auto eaa = env_args_args(args);
    return globalize(f(eaa), eaa, cf);
}

std::shared_ptr<any> lookup_global(const symbol& sym);

std::shared_ptr<any> handle_global_lambda_kenv(std::shared_ptr<any> args,
                                               std::shared_ptr<any> fn) {
    auto f = any_cast<lambda>(*fn);

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return f(cons(step_contn_k(args),
                      cons(env_args_env(sca),
                           env_args_args(sca))));
    }

    return run(f(cons(lookup_global(symbol("result")),
                      cons(env_args_env(args),
                           env_args_args(args)))));
}

std::shared_ptr<any> wrap_global_lambda(std::shared_ptr<any> fn,
                                        std::shared_ptr<any> cf) {
    const lambda l = any_cast<lambda>(fn);
    auto p = l.target<function*>();

    if (kenvfn_table.find(*p) != kenvfn_table.end()) {
        return std::make_shared<any>(lambda(
            [fn](std::shared_ptr<any> args) -> std::shared_ptr<any> {
                return handle_global_lambda_kenv(args, fn);
            }));
    }

    return std::make_shared<any>(lambda(
        [fn, cf](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_global_lambda(args, fn, cf);
        }));
}

std::shared_ptr<any> extend_env(std::shared_ptr<any> env,
                                std::shared_ptr<any> syms,
                                std::shared_ptr<any> values) {
    return cons(cons(syms, list_to_vector(values)), env);
}

std::shared_ptr<any> improper_extend_env(std::shared_ptr<any> env,
                                         std::shared_ptr<any> syms,
                                         std::shared_ptr<any> values) {
    vector s;
    auto av = std::make_shared<any>(std::make_shared<vector>());
    auto v = any_cast<std::shared_ptr<vector>>(*av);

    while (!syms->empty()) {
        if (syms->type() == typeid(std::shared_ptr<symbol>)) {
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

    auto as = nil;
    for (auto it = s.rbegin(); it != s.rend(); ++it) {
        as = cons(*it, as);
    }

    return cons(cons(as, av), env);
}

std::shared_ptr<any> handle_lambda(std::shared_ptr<any> args,
                                   std::shared_ptr<any> params,
                                   std::shared_ptr<any> fn,
                                   std::shared_ptr<any> env,
                                   extend_env_fn extend_env) {
    auto f = any_cast<lambda>(*fn);

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return f(cons(step_contn_k(args),
                      cons(extend_env(env, params, env_args_args(sca)), nil)));
    }

    return run(f(cons(lookup_global(symbol("result")),
                      cons(extend_env(env, params, env_args_args(args)),
                           nil))));
}

std::shared_ptr<any> handle_contn_lambda(std::shared_ptr<any> args,
                                         std::shared_ptr<any> k) {
    auto f = any_cast<lambda>(*k);

    if (is_step_contn(args)) {
        return f(env_args_args(step_contn_args(args)));
    }

    return run(f(env_args_args(args)));
}

std::shared_ptr<any> lookup(symbol& sym, std::shared_ptr<any> env) {
    while (!env->empty()) {
        auto ep = any_cast<std::shared_ptr<pair>>(*env);
        auto bindings = any_cast<std::shared_ptr<pair>>(*ep->first);
        auto values = any_cast<std::shared_ptr<vector>>(*bindings->second);
        auto syms = bindings->first;
        size_t i = 0;

        while (!syms->empty() && (i < values->size())) {
            auto sp = any_cast<std::shared_ptr<pair>>(*syms);
            if (*any_cast<std::shared_ptr<symbol>>(*sp->first) == sym) {
                return (*values)[i];
            }
            syms = sp->second;
            ++i;
        }

        env = ep->second;
    }

    return lookup_global(sym);
}

std::shared_ptr<any> runtime_lookup(std::shared_ptr<any> args) {
    auto name = any_cast<std::shared_ptr<symbol>>(*list_ref(args, 1));
    return std::make_shared<any>(lambda(
        [name](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return send(k, lookup(*name, env));
        }));
}

std::shared_ptr<any> constructed_function(std::shared_ptr<any> args) {
    auto self = list_ref(args, 0);
    auto args2 = list_ref(args, 1);
    auto cf = list_ref(args, 2);
    auto r = any_cast<lambda>(*cf)(args2);
    if (r->type() == typeid(lambda)) {
        return wrap_global_lambda(r, self);
    }
    return r;
}

std::shared_ptr<any> global_lambda(std::shared_ptr<any> args) {
    auto self = list_ref(args, 0);
    auto defn = any_cast<std::shared_ptr<symbol>>(*list_ref(args, 1));
    return wrap_global_lambda(find_global(*defn), self);
}

std::shared_ptr<any> if0(std::shared_ptr<any> args);
std::shared_ptr<any> if1(std::shared_ptr<any> args);
std::shared_ptr<any> sclis0(std::shared_ptr<any> args);
std::shared_ptr<any> sclis1(std::shared_ptr<any> args);
std::shared_ptr<any> sclis2(std::shared_ptr<any> args);
std::shared_ptr<any> scseq0(std::shared_ptr<any> args);
std::shared_ptr<any> scseq1(std::shared_ptr<any> args);
std::shared_ptr<any> lambda0(std::shared_ptr<any> args);
std::shared_ptr<any> lambda1(std::shared_ptr<any> args);
std::shared_ptr<any> improper_lambda0(std::shared_ptr<any> args);
std::shared_ptr<any> improper_lambda1(std::shared_ptr<any> args);
std::shared_ptr<any> letcc0(std::shared_ptr<any> args);
std::shared_ptr<any> letcc1(std::shared_ptr<any> args);

#define define_forms(...) \
std::vector<lambda> forms { \
    __VA_ARGS__ \
}; \
enum class forms { \
    __VA_ARGS__ \
};

define_forms(
    symbol_lookup,
    send_value,
    runtime_lookup,
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
    letcc1
)

std::shared_ptr<any> make_form(std::shared_ptr<any> n,
                               std::shared_ptr<any> args) {
    auto defn = cons(n, args);

    auto f = std::make_shared<any>();
    auto f2 = memoize_lambda(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return any_cast<lambda>(*f)(args);
        },
        defn);
    *f = *memoize_lambda(forms[any_cast<double>(*n)](cons(f2, args)), defn);

    return f;
}

std::shared_ptr<any> make_form(enum forms n, std::shared_ptr<any> args) {
    return make_form(std::make_shared<any>(static_cast<double>(n)), args);
}

std::shared_ptr<any> make_form(std::shared_ptr<any> args) {
    auto p = any_cast<std::shared_ptr<pair>>(*args);
    return make_form(p->first, p->second);
}

std::shared_ptr<any> aform(enum forms n) {
    return std::make_shared<any>(static_cast<double>(n));
}

std::shared_ptr<any> lookup_global(const symbol& sym) {
    auto r = find_global(sym);
    auto defn = cons(aform(forms::global_lambda), cons(make_symbol(sym), nil));
    auto f = std::make_shared<any>();
    auto f2 = memoize_lambda(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return any_cast<lambda>(*f)(args);
        },
        defn);
    *f = *memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

std::shared_ptr<any> globalize(std::shared_ptr<any> x,
                               std::shared_ptr<any> args,
                               std::shared_ptr<any> cf) {
    if (x->type() != typeid(lambda)) {
        return x;
    }

    auto defn = cons(aform(forms::constructed_function),
                     cons(args, cons(cf, nil)));
    auto f = std::make_shared<any>();
    auto f2 = memoize_lambda(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return any_cast<lambda>(*f)(args);
        },
        defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

std::shared_ptr<any> if1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scan1 = list_ref(args, 3);
    auto scan2 = list_ref(args, 4);
    return std::make_shared<any>(lambda([k, env, scan1, scan2]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = any_cast<bool>(*list_ref(args, 0));
            auto f = any_cast<lambda>(v ? *scan1 : *scan2);
            return f(cons(k, cons(env, nil)));
        }));
}

std::shared_ptr<any> if0(std::shared_ptr<any> args) {
    auto scan0 = list_ref(args, 1);
    auto scan1 = list_ref(args, 2);
    auto scan2 = list_ref(args, 3);
    return std::make_shared<any>(lambda([scan0, scan1, scan2]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return any_cast<lambda>(*scan0)(
                cons(make_form(forms::if1,
                               cons(k, cons(env, cons(scan1, cons(scan2,
                                    nil))))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> sclis2(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto v = list_ref(args, 2);
    return std::make_shared<any>(lambda([k, v]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto w = list_ref(args, 0);
            return send(k, cons(v, w));
        }));
}

std::shared_ptr<any> sclis1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return std::make_shared<any>(lambda([k, env, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = list_ref(args, 0);
            return any_cast<lambda>(*rest)(
                cons(make_form(forms::sclis2, cons(k, cons(v, nil))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> sclis0(std::shared_ptr<any> args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return std::make_shared<any>(lambda([first, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return any_cast<lambda>(*first)(
                cons(make_form(forms::sclis1,
                               cons(k, cons(env, cons(rest, nil)))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> scseq1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return std::make_shared<any>(lambda([k, env, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return any_cast<lambda>(*rest)(cons(k, cons(env, nil)));
        }));
}

std::shared_ptr<any> scseq0(std::shared_ptr<any> args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return std::make_shared<any>(lambda([first, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return any_cast<lambda>(*first)(
                cons(make_form(forms::scseq1,
                               cons(k, cons(env, cons(rest, nil)))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> lambda1(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return std::make_shared<any>(lambda([params, scanned, env]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_lambda(args, params, scanned, env, extend_env);
        }));
}

std::shared_ptr<any> lambda0(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(lambda([params, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return send(k,
                        make_form(forms::lambda1,
                                  cons(params, cons(scanned, cons(env, nil)))));
        }));
}

std::shared_ptr<any> improper_lambda1(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return std::make_shared<any>(lambda([params, scanned, env]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_lambda(args,
                                 params,
                                 scanned,
                                 env,
                                 improper_extend_env);
        }));
}

std::shared_ptr<any> improper_lambda0(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(lambda([params, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return send(k,
                        make_form(forms::improper_lambda1,
                                  cons(params, cons(scanned, cons(env, nil)))));
        }));
}

std::shared_ptr<any> letcc1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    return std::make_shared<any>(lambda([k]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_contn_lambda(args, k); 
        }));
}

std::shared_ptr<any> letcc0(std::shared_ptr<any> args) {
    auto name = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(lambda([name, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return any_cast<lambda>(*scanned)(
                cons(k,
                     cons(extend_env(env,
                                     cons(name, nil),
                                     cons(make_form(forms::letcc1,
                                                    cons(k, nil)),
                                          nil)),
                          nil)));
        }));
}

bool is_unmemoized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           ((*v)[0]->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*(*v)[0]) == "MCE-UNMEMOIZED");
}

std::shared_ptr<any> unmemoized_repexp(std::shared_ptr<vector> v) {
    return (*v)[1];
}

std::shared_ptr<any> memoize_aux(std::shared_ptr<any> exp,
                                 cmap_table& tab,
                                 map_fn fn) {
    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, table_set);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        auto v = any_cast<std::shared_ptr<vector>>(*exp);
        if (is_unmemoized(v)) {
            auto repexp = unmemoized_repexp(v);
            auto r = make_symbol("unspecified");
            auto entry = table_set(tab, exp, memoize_lambda(
                [r](std::shared_ptr<any> args) -> std::shared_ptr<any> {
                    return any_cast<lambda>(*r)(args);
                }, repexp));
            *r = *make_form(fn(repexp));
            return r;
        }
        return vector_cmap(fn, exp, tab, table_set);
    }

    return exp;
}

std::shared_ptr<any> memoize(std::shared_ptr<any> exp) {
    cmap_table tab;
    map_fn fn = [&tab, &fn](std::shared_ptr<any> x) -> std::shared_ptr<any> {
        return memoize_aux(x, tab, fn);
    };
    return fn(exp);
}

bool is_serialized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           ((*v)[0]->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*(*v)[0]) == "MCE-SERIALIZED");
}

std::shared_ptr<any> serialized_n(std::shared_ptr<vector> v) {
    return (*v)[1];
}

std::shared_ptr<any> deserialize_aux(std::shared_ptr<any> exp,
                                     cmap_table& tab,
                                     map_fn fn,
                                     set_entry_fn set_entry) {
    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, set_entry);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        auto v = any_cast<std::shared_ptr<vector>>(*exp);
        if (is_serialized(v)) {
            return tab[serialized_n(v)];
        }
        return vector_cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

std::shared_ptr<any> deserialize(std::shared_ptr<any> exp) {
    size_t counter = 0;
    cmap_table tab;
    set_entry_fn set_entry = [&counter](cmap_table& tab,
                                        std::shared_ptr<any> v,
                                        std::shared_ptr<any> entry) {
        return table_set(tab, std::make_shared<any>(counter++), entry);
    };
    map_fn fn = [&tab, &fn, &set_entry]
        (std::shared_ptr<any> x) -> std::shared_ptr<any> {
            return deserialize_aux(x, tab, fn, set_entry);
        };
    return fn(exp);
}

std::shared_ptr<any> unpickle_aux(const json& j) {
    switch (j[0].get<std::string>()[0]) {
    case boolean_code:
        return std::make_shared<any>(j[1].get<std::string>() == "t");
    case number_code:
        return std::make_shared<any>(j[1].get<double>());
    case char_code:
        return std::make_shared<any>(j[1].get<std::string>()[0]);
    case string_code:
        return make_string(j[1].get<std::string>());
    case symbol_code:
        return make_symbol(j[1].get<std::string>());
    case pair_code:
        return cons(unpickle_aux(j[1]), unpickle_aux(j[2]));
    case vector_code:
        return list_to_vector(unpickle_aux(j[1]));
    default:
        return nil;
    }
}

std::shared_ptr<any> unpickle(const std::string& s) {
    return unpickle_aux(json::parse(s));
}

std::shared_ptr<any> mce_restore(const std::string& s) {
    return memoize(deserialize(unpickle(s)));
}

int main(int argc, char *argv[]) {
    json s;
    std::cin >> s;
    mce_restore(s.get<std::string>());
}
