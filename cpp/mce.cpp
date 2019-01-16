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

typedef std::pair<std::shared_ptr<any>, std::shared_ptr<any>> pair;
typedef std::vector<std::shared_ptr<any>> vector;
typedef std::shared_ptr<any> function(std::shared_ptr<any>);
typedef std::function<function> func;
typedef std::shared_ptr<func> lambda;

class symbol : public std::string {
public:
    symbol(const std::string& s) : std::string(s) {}
};

struct CMapHash {
    std::size_t operator()(const std::shared_ptr<any>& a) const noexcept {
        if (a && !a->empty()) {
            auto& type = a->type();

            if (type == typeid(std::shared_ptr<pair>)) {
                return std::hash<std::shared_ptr<pair>>{}(
                    any_cast<std::shared_ptr<pair>>(*a));
            }

            if (type == typeid(std::shared_ptr<vector>)) {
                return std::hash<std::shared_ptr<vector>>{}(
                    any_cast<std::shared_ptr<vector>>(*a));
            }

            if (type == typeid(lambda)) {
                return std::hash<lambda>{}(any_cast<lambda>(*a));
            }

            if (type == typeid(double)) {
                return std::hash<double>{}(any_cast<double>(*a));
            }
        }

        return std::hash<std::shared_ptr<any>>{}(a);
    }
};

struct CMapEqual {
    bool operator()(const std::shared_ptr<any>& x,
                    const std::shared_ptr<any>& y) const noexcept {
        if (x && y) {
            if (x->empty()) {
                return y->empty();
            }

            if (y->empty()) {
                return false;
            }

            auto& type = x->type();
            if (y->type() != type) {
                return false;
            }

            if (type == typeid(std::shared_ptr<pair>)) {
                return any_cast<std::shared_ptr<pair>>(*x) ==
                       any_cast<std::shared_ptr<pair>>(*y);
            }

            if (type == typeid(std::shared_ptr<vector>)) {
                return any_cast<std::shared_ptr<vector>>(*x) ==
                       any_cast<std::shared_ptr<vector>>(*y);
            }

            if (type == typeid(lambda)) {
                return any_cast<lambda>(*x) == any_cast<lambda>(*y);
            }

            if (type == typeid(double)) {
                return any_cast<double>(*x) == any_cast<double>(*y);
            }
        }

        return x == y;
    }
};

typedef std::unordered_map<std::shared_ptr<any>,
                           std::shared_ptr<any>,
                           CMapHash,
                           CMapEqual> cmap_table;
typedef std::function<std::shared_ptr<any>(std::shared_ptr<any>)> map_fn;
typedef std::function<std::shared_ptr<any>(cmap_table&,
                                           std::shared_ptr<any>,
                                           std::shared_ptr<any>)> set_entry_fn;
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

std::shared_ptr<any> vector_to_list(std::shared_ptr<any> vec) {
    auto v = any_cast<std::shared_ptr<vector>>(*vec);
    auto l = nil;
    for (auto it = v->rbegin(); it != v->rend(); ++it) {
        l = cons(*it, l);
    }
    return l;
}

std::shared_ptr<any> cmap(map_fn f,
                          std::shared_ptr<any> l,
                          cmap_table& tab,
                          set_entry_fn set_entry) {
    auto ref = tab.find(l);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto entry = cons(nil, nil);
    set_entry(tab, l, entry);

    auto ep = any_cast<std::shared_ptr<pair>>(*entry);
    auto lp = any_cast<std::shared_ptr<pair>>(*l);
    ep->first = f(lp->first);
    ep->second = f(lp->second);

    return entry;
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

std::shared_ptr<any> get_procedure_defn(std::shared_ptr<any> proc) {
    return (*any_cast<lambda>(*proc))(
        cons(make_symbol("MCE-YIELD-DEFINITION"), nil));
}

std::shared_ptr<any> display(std::shared_ptr<any> args) {
    auto exp = list_ref(args, 0);
    if (exp->empty()) {
        std::cout << "()";
    } else {
        auto& type = exp->type();
        if (type == typeid(bool)) {
            std::cout << (any_cast<bool>(*exp) ? "#t" : "#f");
        } else if (type == typeid(double)) {
            std::cout << any_cast<double>(*exp);
        } else if (type == typeid(char)) {
            std::cout << any_cast<char>(*exp);
        } else if (type == typeid(std::shared_ptr<std::string>)) {
            std::cout << *any_cast<std::shared_ptr<std::string>>(*exp);
        } else if (type == typeid(std::shared_ptr<symbol>)) {
            std::cout << *any_cast<std::shared_ptr<symbol>>(*exp);
        } else if (type == typeid(std::shared_ptr<pair>)) {
            bool first = true;
            std:: cout << "(";
            while (exp->type() == typeid(std::shared_ptr<pair>)) {
                if (!first) {
                    std::cout << " ";
                }
                first = false;
                auto p = any_cast<std::shared_ptr<pair>>(*exp);
                display(cons(p->first, nil));
                exp = p->second;
            }
            if (!exp->empty()) {
                std::cout << " . ";
                display(cons(exp, nil));
            }
            std::cout << ")";
        } else if (type == typeid(std::shared_ptr<vector>)) {
            bool first = true;
            std::cout << "#(";
            auto vec = any_cast<std::shared_ptr<vector>>(*exp);
            for (auto v : *vec) {
                if (!first) {
                    std::cout << " ";
                }
                first = false;
                display(cons(v, nil));
            }
            std::cout << ")";
        } else if (type == typeid(lambda)) {
            std::cout << "#<procedure>";
        } else {
            throw std::range_error("unknown display expression");
        }
    }
    return exp;
}

std::shared_ptr<any> newline(std::shared_ptr<any> args) {
    std::cout << std::endl;
    return nil;
}

std::shared_ptr<any> print(std::shared_ptr<any> args) {
    auto r = nil;
    while (!args->empty()) {
        auto p = any_cast<std::shared_ptr<pair>>(*args);
        display(cons(p->first, nil));
        r = p->first;
        args = p->second;
    }
    newline(args);
    return r;
}

std::shared_ptr<any> unmemoize(std::shared_ptr<any> exp);
std::shared_ptr<any> serialize(std::shared_ptr<any> exp);

std::shared_ptr<any> memoize_lambda(lambda proc, std::shared_ptr<any> defn) {
    return std::make_shared<any>(std::make_shared<func>(
        [proc, defn](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            if (is_yield_defn(args)) {
                if (defn->type() == typeid(lambda)) {
                    return (*any_cast<lambda>(*defn))(nil);
                }
                return defn;
            }
            //print(cons(serialize(unmemoize(defn)), nil));
            return (*proc)(args);
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

std::shared_ptr<any> ctenv_setvar(std::shared_ptr<any> name,
                                  std::shared_ptr<any> i,
                                  std::shared_ptr<any> val,
                                  std::shared_ptr<any> env) {
    auto ip = any_cast<std::shared_ptr<pair>>(*i);
    auto first = any_cast<double>(*ip->first);
    auto second = any_cast<double>(*ip->second);
    auto bindings = any_cast<std::shared_ptr<pair>>(*list_ref(env, first));
    auto v = any_cast<std::shared_ptr<vector>>(*bindings->second);

    if (second >= v->size()) {
        v->resize(second + 1, nil);
    }
    (*v)[second] = val;

    if (bindings->first->empty()) {
        bindings->first = cons(nil, nil);
    }
    auto p = any_cast<std::shared_ptr<pair>>(*bindings->first);
    while (second > 0) {
        if (p->second->empty()) {
            p->second = cons(nil, nil);
        }
        p = any_cast<std::shared_ptr<pair>>(*p->second);
        --second;
    }
    p->first = name;

    return val;
}

std::shared_ptr<any> setvar(std::shared_ptr<any> sym,
                            std::shared_ptr<any> value,
                            std::shared_ptr<any> env) {
    auto name = any_cast<std::shared_ptr<symbol>>(*sym);

    while (!env->empty()) {
        auto ep = any_cast<std::shared_ptr<pair>>(*env);
        auto bindings = any_cast<std::shared_ptr<pair>>(*ep->first);
        auto values = any_cast<std::shared_ptr<vector>>(*bindings->second);
        auto syms = bindings->first;
        size_t i = 0;

        while (!syms->empty() && (i < values->size())) {
            auto sp = any_cast<std::shared_ptr<pair>>(*syms);
            if (*any_cast<std::shared_ptr<symbol>>(*sp->first) == *name) {
                (*values)[i] = value;
                return value;
            }
            syms = sp->second;
            ++i;
        }

        env = ep->second;
    }

    throw std::range_error(*name);
}

std::shared_ptr<any> symbol_lookup(std::shared_ptr<any> args) {
    auto i = list_ref(args, 1);
    return std::make_shared<any>(std::make_shared<func>(
        [i](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return send(k, ctenv_lookup(i, env));
        }));
}

std::shared_ptr<any> send_value(std::shared_ptr<any> args) {
    auto exp = list_ref(args, 1);
    return std::make_shared<any>(std::make_shared<func>(
        [exp](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            return send(k, exp);
        }));
}

std::shared_ptr<any> make_step_contn(std::shared_ptr<any> k,
                                     std::shared_ptr<any> args) {
    return cons(make_symbol("MCE-STEP-CONTN"), cons(k, args));
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
    return (*any_cast<lambda>(*fn))(make_step_contn(k,
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

std::shared_ptr<any> applyx(std::shared_ptr<any> k,
                            std::shared_ptr<any> env,
                            std::shared_ptr<any> fn,
                            std::shared_ptr<any> args) {
    return (*any_cast<lambda>(*fn))(
        make_step_contn(k, make_env_args(env, args)));
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

std::shared_ptr<any> less_than(std::shared_ptr<any> args) {
    bool r = any_cast<double>(*list_ref(args, 0)) <
             any_cast<double>(*list_ref(args, 1));
    return std::make_shared<any>(r);
}

std::shared_ptr<any> plus(std::shared_ptr<any> args) {
    double r = 0;
    while (!args->empty()) {
        auto p = any_cast<std::shared_ptr<pair>>(*args);
        r += any_cast<double>(*p->first);
        args = p->second;
    }
    return std::make_shared<any>(r);
}

std::shared_ptr<any> multiply(std::shared_ptr<any> args) {
    double r = 1;
    while (!args->empty()) {
        auto p = any_cast<std::shared_ptr<pair>>(*args);
        r *= any_cast<double>(*p->first);
        args = p->second;
    }
    return std::make_shared<any>(r);
}

std::shared_ptr<any> is_null(std::shared_ptr<any> args) {
    return std::make_shared<any>(list_ref(args, 0)->empty());
}

std::shared_ptr<any> is_string(std::shared_ptr<any> args) {
    auto a = list_ref(args, 0);
    return std::make_shared<any>(
        !a->empty() &&
        (a->type() == typeid(std::shared_ptr<std::string>)));
}

std::shared_ptr<any> is_pair(std::shared_ptr<any> args) {
    auto a = list_ref(args, 0);
    return std::make_shared<any>(
        !a->empty() &&
        (a->type() == typeid(std::shared_ptr<pair>)));
}

std::shared_ptr<any> is_vector(std::shared_ptr<any> args) {
    auto a = list_ref(args, 0);
    return std::make_shared<any>(
        !a->empty() &&
        (a->type() == typeid(std::shared_ptr<vector>)));
}

std::shared_ptr<any> vector_length(std::shared_ptr<any> args) {
    auto a = list_ref(args, 0);
    return std::make_shared<any>(
        static_cast<double>(any_cast<std::shared_ptr<vector>>(*a)->size()));
}

std::shared_ptr<any> vector_ref(std::shared_ptr<any> args) {
    auto a = list_ref(args, 0);
    auto i = list_ref(args, 1);
    return any_cast<std::shared_ptr<vector>>(*a)->at(any_cast<double>(*i));
}

std::shared_ptr<any> is_string_equal(std::shared_ptr<any> args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return std::make_shared<any>(
        *any_cast<std::shared_ptr<std::string>>(*x) ==
        *any_cast<std::shared_ptr<std::string>>(*y));
}

std::shared_ptr<any> is_number_equal(std::shared_ptr<any> args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return std::make_shared<any>(
        any_cast<double>(*x) ==
        any_cast<double>(*y));
}

std::shared_ptr<any> car(std::shared_ptr<any> args) {
    return list_ref(list_ref(args, 0), 0);
}

std::shared_ptr<any> cdr(std::shared_ptr<any> args) {
    return list_rest(list_ref(args, 0), 0);
}

std::shared_ptr<any> is_eq(std::shared_ptr<any> args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);

    if (x->empty()) {
        return std::make_shared<any>(y->empty());
    }

    if (y->empty()) {
        return std::make_shared<any>(false);
    }

    auto& type = x->type();
    if (y->type() != type) {
        return std::make_shared<any>(false);
    }

    if (type == typeid(bool)) {
        return std::make_shared<any>(any_cast<bool>(*x) == any_cast<bool>(*y));
    }

    if (type == typeid(char)) {
        return std::make_shared<any>(any_cast<char>(*x) == any_cast<char>(*y));
    }

    if (type == typeid(double)) {
        return std::make_shared<any>(
            any_cast<double>(*x) == any_cast<double>(*y));
    }

    if (type == typeid(std::shared_ptr<std::string>)) {
        return std::make_shared<any>(
            any_cast<std::shared_ptr<std::string>>(*x) ==
            any_cast<std::shared_ptr<std::string>>(*y));
    }

    if (type == typeid(std::shared_ptr<symbol>)) {
        return std::make_shared<any>(
            *any_cast<std::shared_ptr<symbol>>(*x) ==
            *any_cast<std::shared_ptr<symbol>>(*y));
    }

    if (type == typeid(std::shared_ptr<pair>)) {
        return std::make_shared<any>(
            any_cast<std::shared_ptr<pair>>(*x) ==
            any_cast<std::shared_ptr<pair>>(*y));
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        return std::make_shared<any>(
            any_cast<std::shared_ptr<vector>>(*x) ==
            any_cast<std::shared_ptr<vector>>(*y));
    }

    if (type == typeid(lambda)) {
        return std::make_shared<any>(
            any_cast<lambda>(*x) == any_cast<lambda>(*y));
    }

    return std::make_shared<any>(false);
}

std::unordered_map<std::string, function*> global_table {
    { "result", result },
    { "<", less_than },
    { "print", print },
    { "+", plus },
    { "*", multiply },
    { "null?" , is_null },
    { "car", car },
    { "cdr", cdr },
    { "eq?" , is_eq },
    { "=" , is_number_equal },
    { "string?", is_string },
    { "pair?", is_pair },
    { "string=?", is_string_equal },
    { "vector?" , is_vector },
    { "vector-length", vector_length },
    { "vector-ref", vector_ref }
};

std::unordered_set<function*> kenvfn_table;

std::shared_ptr<any> find_global(const symbol& sym) {
    auto it = global_table.find(sym);
    if (it == global_table.end()) {
        throw std::range_error(sym);
    }
    return std::make_shared<any>(std::make_shared<func>(it->second));
}

std::shared_ptr<any> step(std::shared_ptr<any> state) {
    return (*any_cast<lambda>(*list_ref(state, 0)))(
        cons(list_rest(state, 0), nil));
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
            return (*f)(make_step_contn(sck, transfer_args(sca)));
        }
        auto eaa = env_args_args(sca);
        return send(sck, globalize((*f)(eaa), eaa, cf));
    }

    auto eaa = env_args_args(args);
    return globalize((*f)(eaa), eaa, cf);
}

std::shared_ptr<any> lookup_global(const symbol& sym);

std::shared_ptr<any> handle_global_lambda_kenv(std::shared_ptr<any> args,
                                               std::shared_ptr<any> fn) {
    auto f = any_cast<lambda>(*fn);

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return (*f)(cons(step_contn_k(args),
                         cons(env_args_env(sca),
                              env_args_args(sca))));
    }

    return run((*f)(cons(lookup_global(symbol("result")),
                         cons(env_args_env(args),
                              env_args_args(args)))));
}

std::shared_ptr<any> wrap_global_lambda(std::shared_ptr<any> fn,
                                        std::shared_ptr<any> cf) {
    const lambda l = any_cast<lambda>(*fn);
    auto p = l->target<function*>();

    if (kenvfn_table.find(*p) != kenvfn_table.end()) {
        return std::make_shared<any>(std::make_shared<func>(
            [fn](std::shared_ptr<any> args) -> std::shared_ptr<any> {
                return handle_global_lambda_kenv(args, fn);
            }));
    }

    return std::make_shared<any>(std::make_shared<func>(
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
        return (*f)(cons(step_contn_k(args),
                         cons(extend_env(env, params, env_args_args(sca)),
                              nil)));
    }

    return run((*f)(cons(lookup_global(symbol("result")),
                         cons(extend_env(env, params, env_args_args(args)),
                              nil))));
}

std::shared_ptr<any> handle_contn_lambda(std::shared_ptr<any> args,
                                         std::shared_ptr<any> k) {
    auto f = any_cast<lambda>(*k);

    if (is_step_contn(args)) {
        return (*f)(env_args_args(step_contn_args(args)));
    }

    return run((*f)(env_args_args(args)));
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
    return std::make_shared<any>(std::make_shared<func>(
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
    auto r = (*any_cast<lambda>(*cf))(args2);
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

std::shared_ptr<any> evalx_initial(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([k, env, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return (*any_cast<lambda>(*scanned))(cons(k, cons(env, nil)));
        }));
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
std::shared_ptr<any> define0(std::shared_ptr<any> args);
std::shared_ptr<any> define1(std::shared_ptr<any> args);
std::shared_ptr<any> set0(std::shared_ptr<any> args);
std::shared_ptr<any> set1(std::shared_ptr<any> args);
std::shared_ptr<any> application0(std::shared_ptr<any> args);
std::shared_ptr<any> application1(std::shared_ptr<any> args);

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
    letcc1,
    define0,
    define1,
    set0,
    set1,
    application0,
    application1,
    evalx_initial
)

std::shared_ptr<any> make_form(std::shared_ptr<any> n,
                               std::shared_ptr<any> args) {
    auto defn = cons(n, args);

    auto f = std::make_shared<any>();
    auto f2 = memoize_lambda(std::make_shared<func>(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return (*any_cast<lambda>(*f))(args);
        }),
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
    auto f2 = memoize_lambda(std::make_shared<func>(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return (*any_cast<lambda>(*f))(args);
        }),
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
    auto f2 = memoize_lambda(std::make_shared<func>(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return (*any_cast<lambda>(*f))(args);
        }),
        defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

std::shared_ptr<any> if1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scan1 = list_ref(args, 3);
    auto scan2 = list_ref(args, 4);
    return std::make_shared<any>(std::make_shared<func>([k, env, scan1, scan2]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = any_cast<bool>(*list_ref(args, 0));
            auto f = any_cast<lambda>(v ? *scan1 : *scan2);
            return (*f)(cons(k, cons(env, nil)));
        }));
}

std::shared_ptr<any> if0(std::shared_ptr<any> args) {
    auto scan0 = list_ref(args, 1);
    auto scan1 = list_ref(args, 2);
    auto scan2 = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([scan0, scan1, scan2]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*scan0))(
                cons(make_form(forms::if1,
                               cons(k, cons(env, cons(scan1, cons(scan2,
                                    nil))))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> sclis2(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto v = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([k, v]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto w = list_ref(args, 0);
            return send(k, cons(v, w));
        }));
}

std::shared_ptr<any> sclis1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([k, env, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = list_ref(args, 0);
            return (*any_cast<lambda>(*rest))(
                cons(make_form(forms::sclis2, cons(k, cons(v, nil))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> sclis0(std::shared_ptr<any> args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([first, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*first))(
                cons(make_form(forms::sclis1,
                               cons(k, cons(env, cons(rest, nil)))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> scseq1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([k, env, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return (*any_cast<lambda>(*rest))(cons(k, cons(env, nil)));
        }));
}

std::shared_ptr<any> scseq0(std::shared_ptr<any> args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([first, rest]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*first))(
                cons(make_form(forms::scseq1,
                               cons(k, cons(env, cons(rest, nil)))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> lambda1(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([params, scanned, env]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_lambda(args, params, scanned, env, extend_env);
        }));
}

std::shared_ptr<any> lambda0(std::shared_ptr<any> args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([params, scanned]
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
    return std::make_shared<any>(std::make_shared<func>([params, scanned, env]
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
    return std::make_shared<any>(std::make_shared<func>([params, scanned]
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
    return std::make_shared<any>(std::make_shared<func>([k]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return handle_contn_lambda(args, k); 
        }));
}

std::shared_ptr<any> letcc0(std::shared_ptr<any> args) {
    auto name = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([name, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*scanned))(
                cons(k,
                     cons(extend_env(env,
                                     cons(name, nil),
                                     cons(make_form(forms::letcc1,
                                                    cons(k, nil)),
                                          nil)),
                          nil)));
        }));
}

std::shared_ptr<any> define1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto name = list_ref(args, 3);
    auto i = list_ref(args, 4);
    return std::make_shared<any>(std::make_shared<func>([k, env, name, i]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = list_ref(args, 0);
            return send(k, ctenv_setvar(name, i, v, env));
        }));
}

std::shared_ptr<any> define0(std::shared_ptr<any> args) {
    auto name = list_ref(args, 1);
    auto i = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([name, i, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*scanned))(
                cons(make_form(forms::define1,
                               cons(k, cons(env, cons(name, cons(i, nil))))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> set1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto name = list_ref(args, 3);
    return std::make_shared<any>(std::make_shared<func>([k, env, name]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = list_ref(args, 0);
            return send(k, setvar(name, v, env));
        }));
}

std::shared_ptr<any> set0(std::shared_ptr<any> args) {
    auto name = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([name, scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*scanned))(
                cons(make_form(forms::set1,
                               cons(k, cons(env, cons(name, nil)))),
                     cons(env, nil)));
        }));
}

std::shared_ptr<any> application1(std::shared_ptr<any> args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    return std::make_shared<any>(std::make_shared<func>([k, env]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto v = list_ref(args, 0);
            return applyx(k, env, list_ref(v, 0), list_rest(v, 0));
        }));
}

std::shared_ptr<any> application0(std::shared_ptr<any> args) {
    auto scanned = list_ref(args, 1);
    return std::make_shared<any>(std::make_shared<func>([scanned]
        (std::shared_ptr<any> args) -> std::shared_ptr<any> {
            auto k = list_ref(args, 0);
            auto env = list_ref(args, 1);
            return (*any_cast<lambda>(*scanned))(
                cons(make_form(forms::application1, cons(k, cons(env, nil))),
                     cons(env, nil)));
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
    if (exp->empty()) {
        return exp;
    }

    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, table_set);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        auto v = any_cast<std::shared_ptr<vector>>(*exp);
        if (is_unmemoized(v)) {
            auto repexp = unmemoized_repexp(v);
            auto r = make_symbol("unspecified");
            auto f = make_symbol("unspecified");
            auto entry = table_set(tab, exp, memoize_lambda(
                std::make_shared<func>([f]
                    (std::shared_ptr<any> args) -> std::shared_ptr<any> {
                        return (*any_cast<lambda>(*f))(args);
                    }),
                std::make_shared<any>(std::make_shared<func>([r]
                    (std::shared_ptr<any> args) -> std::shared_ptr<any> {
                        return r;
                    }))));
            *r = *fn(repexp);
            *f = std::make_shared<func>([r, f]
                (std::shared_ptr<any> args) -> std::shared_ptr<any> {
                    *f = *make_form(r);
                    return (*any_cast<lambda>(*f))(args);
                });
            return entry;
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

std::shared_ptr<any> unmemoize_aux(std::shared_ptr<any> exp,
                                   cmap_table& tab,
                                   map_fn fn) {
    if (exp->empty()) {
        return exp;
    }

    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, table_set);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        return vector_cmap(fn, exp, tab, table_set);
    }

    if (type == typeid(lambda)) {
        auto ref = tab.find(exp);
        if (ref != tab.end()) {
            return ref->second;
        }

        auto entry = table_set(tab, exp,
            std::make_shared<any>(std::make_shared<vector>()));
        auto v = any_cast<std::shared_ptr<vector>>(*entry);

        v->push_back(make_symbol("MCE-UNMEMOIZED"));
        v->push_back(fn(get_procedure_defn(exp)));

        return entry;
    }

    return exp;
}

std::shared_ptr<any> unmemoize(std::shared_ptr<any> exp) {
    cmap_table tab;
    map_fn fn = [&tab, &fn](std::shared_ptr<any> x) -> std::shared_ptr<any> {
        return unmemoize_aux(x, tab, fn);
    };
    return fn(exp);
}

std::shared_ptr<any> make_serialized(size_t n) {
    auto a = std::make_shared<any>(std::make_shared<vector>());
    auto v = any_cast<std::shared_ptr<vector>>(*a);

    v->push_back(make_symbol("MCE-SERIALIZED"));
    v->push_back(std::make_shared<any>(static_cast<double>(n)));

    return a;
}

bool is_serialized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           ((*v)[0]->type() == typeid(std::shared_ptr<symbol>)) &&
           (*any_cast<std::shared_ptr<symbol>>(*(*v)[0]) == "MCE-SERIALIZED");
}

std::shared_ptr<any> serialized_n(std::shared_ptr<vector> v) {
    return (*v)[1];
}

std::shared_ptr<any> serialize_aux(std::shared_ptr<any> exp,
                                   cmap_table& tab,
                                   map_fn fn,
                                   set_entry_fn set_entry) {
    if (exp->empty()) {
        return exp;
    }

    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, set_entry);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        return vector_cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

std::shared_ptr<any> serialize(std::shared_ptr<any> exp) {
    size_t counter = 0;
    cmap_table tab;
    set_entry_fn set_entry = [&counter](cmap_table& tab,
                                        std::shared_ptr<any> v,
                                        std::shared_ptr<any> entry) {
        return table_set(tab, v, make_serialized(counter++));
    };
    map_fn fn = [&tab, &fn, &set_entry]
        (std::shared_ptr<any> x) -> std::shared_ptr<any> {
            return serialize_aux(x, tab, fn, set_entry);
        };
    return fn(exp);
}

std::shared_ptr<any> deserialize_aux(std::shared_ptr<any> exp,
                                     cmap_table& tab,
                                     map_fn fn,
                                     set_entry_fn set_entry) {
    if (exp->empty()) {
        return exp;
    }

    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, set_entry);
    }

    if (type == typeid(std::shared_ptr<vector>)) {
        auto v = any_cast<std::shared_ptr<vector>>(*exp);
        if (is_serialized(v)) {
            return tab.at(serialized_n(v));
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
        return table_set(tab,
                         std::make_shared<any>(static_cast<double>(counter++)),
                         entry);
    };
    map_fn fn = [&tab, &fn, &set_entry]
        (std::shared_ptr<any> x) -> std::shared_ptr<any> {
            return deserialize_aux(x, tab, fn, set_entry);
        };
    return fn(exp);
}

json pickle_aux(std::shared_ptr<any> exp) {
    json j;
    if (exp->empty()) {
        j.push_back(std::string(1, null_code));
    } else {
        auto& type = exp->type();
        if (type == typeid(bool)) {
            j.push_back(std::string(1, boolean_code));
            j.push_back(any_cast<bool>(*exp) ? "t" : "f");
        } else if (type == typeid(double)) {
            j.push_back(std::string(1, number_code));
            j.push_back(any_cast<double>(*exp));
        } else if (type == typeid(char)) {
            j.push_back(std::string(1, char_code));
            j.push_back(std::string(1, any_cast<char>(*exp)));
        } else if (type == typeid(std::shared_ptr<std::string>)) {
            j.push_back(std::string(1, string_code));
            j.push_back(*any_cast<std::shared_ptr<std::string>>(*exp));
        } else if (type == typeid(std::shared_ptr<symbol>)) {
            j.push_back(std::string(1, symbol_code));
            j.push_back(*any_cast<std::shared_ptr<symbol>>(*exp));
        } else if (type == typeid(std::shared_ptr<pair>)) {
            auto p = any_cast<std::shared_ptr<pair>>(*exp);
            j.push_back(std::string(1, pair_code));
            j.push_back(pickle_aux(p->first));
            j.push_back(pickle_aux(p->second));
        } else if (type == typeid(std::shared_ptr<vector>)) {
            j.push_back(std::string(1, vector_code));
            j.push_back(pickle_aux(vector_to_list(exp)));
        } else {
            throw std::range_error("unknown pickle expression");
        }
    }
    return j;
}

std::string pickle(std::shared_ptr<any> exp) {
    return pickle_aux(exp).dump();
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

std::string mce_save(std::shared_ptr<any> exp) {
    return pickle(serialize(unmemoize(exp)));
}

std::shared_ptr<any> mce_restore(const std::string& s) {
    return memoize(deserialize(unpickle(s)));
}

int main(int argc, char *argv[]) {
    json s;
    std::cin >> s;
    auto r = mce_restore(s.get<std::string>());
    if (r->type() == typeid(lambda)) {
        (*any_cast<lambda>(*r))(cons(nil, nil));
    } else {
        run(r);
    }
    return 0;
}
