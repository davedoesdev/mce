#include <iostream>
#include <utility>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include "json.hpp"
#include "mce.hpp"

using nlohmann::json;

const char null_code    = 'a';
const char boolean_code = 'b';
const char number_code  = 'c';
const char char_code    = 'd';
const char string_code  = 'e';
const char symbol_code  = 'f';
const char pair_code    = 'g';
const char vector_code  = 'h';

typedef std::pair<boxed, boxed> pair;
typedef std::vector<boxed> vector;

class symbol : public std::string {
public:
    symbol(const std::string& s) : std::string(s) {}
};

inline bool box_is_empty(const boxed& a) {
    return a->empty();
}

inline bool boxes_contain_same_type(const boxed& a, const boxed& b) {
    return a->type() == b->type();
}

template<>
struct cast_return<std::string>{ typedef std::shared_ptr<std::string> type; };
template<>
inline std::shared_ptr<std::string> box_cast<std::string>(const boxed& a) {
    return boost::any_cast<std::shared_ptr<std::string>>(*a);
}

template<>
struct cast_return<symbol>{ typedef std::shared_ptr<symbol> type; };
template<>
inline std::shared_ptr<symbol> box_cast<symbol>(const boxed& a) {
    return boost::any_cast<std::shared_ptr<symbol>>(*a);
}

template<>
struct cast_return<pair>{ typedef std::shared_ptr<pair> type; };
template<>
inline std::shared_ptr<pair> box_cast<pair>(const boxed& a) {
    return boost::any_cast<std::shared_ptr<pair>>(*a);
}

template<>
struct cast_return<vector>{ typedef std::shared_ptr<vector> type; };
template<>
inline std::shared_ptr<vector> box_cast<vector>(const boxed& a) {
    return boost::any_cast<std::shared_ptr<vector>>(*a);
}

template<>
inline bool box_contains<std::string>(const boxed& a) {
    return a->type() == typeid(std::shared_ptr<std::string>);
}

template<>
inline bool box_contains<symbol>(const boxed& a) {
    return a->type() == typeid(std::shared_ptr<symbol>);
}

template<>
inline bool box_contains<pair>(const boxed& a) {
    return a->type() == typeid(std::shared_ptr<pair>);
}

template<>
inline bool box_contains<vector>(const boxed& a) {
    return a->type() == typeid(std::shared_ptr<vector>);
}

boxed boxin() {
    return std::make_shared<box>();
}

template<typename T>
boxed boxin(const T& a) {
    return std::make_shared<box>(a);
}

struct CMapHash {
    std::size_t operator()(const boxed& a) const noexcept {
        if (!box_is_empty(a)) {
            if (box_contains<pair>(a)) {
                return std::hash<std::shared_ptr<pair>>{}(
                    box_cast<pair>(a));
            }

            if (box_contains<vector>(a)) {
                return std::hash<std::shared_ptr<vector>>{}(
                    box_cast<vector>(a));
            }

            if (box_contains<lambda>(a)) {
                return std::hash<lambda>{}(box_cast<lambda>(a));
            }

            if (box_contains<double>(a)) {
                return std::hash<double>{}(box_cast<double>(a));
            }
        }

        return std::hash<boxed>{}(a);
    }
};

struct CMapEqual {
    bool operator()(const boxed& x, const boxed& y) const noexcept {
        if (box_is_empty(x)) {
            return box_is_empty(y);
        }

        if (box_is_empty(y)) {
            return false;
        }

        if (!boxes_contain_same_type(x, y)) {
            return false;
        }

        if (box_contains<pair>(x)) {
            return box_cast<pair>(x) == box_cast<pair>(y);
        }

        if (box_contains<vector>(x)) {
            return box_cast<vector>(x) == box_cast<vector>(y);
        }

        if (box_contains<lambda>(x)) {
            return box_cast<lambda>(x) == box_cast<lambda>(y);
        }

        if (box_contains<double>(x)) {
            return box_cast<double>(x) == box_cast<double>(y);
        }

        return x == y;
    }
};

typedef std::unordered_map<boxed, boxed, CMapHash, CMapEqual> cmap_table;
typedef std::function<boxed(boxed)> map_fn;
typedef std::function<boxed(cmap_table&, boxed, boxed)> set_entry_fn;
typedef boxed extend_env_fn(boxed env, boxed syms, boxed values);

const boxed bnil = boxin();
const boxed bfalse = boxin(false);

std::unordered_map<pair*, std::weak_ptr<pair>> allocated_pairs;
std::unordered_map<vector*, std::weak_ptr<vector>> allocated_vectors;
std::unordered_map<func*, std::weak_ptr<func>> allocated_functions;

std::string mce_save(boxed exp);
boxed mce_restore(const std::string& s);

size_t gc_threshold;

boxed maybe_gc(boxed state) {
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

        state = mce_restore(saved);
    }
    return state;
}

boxed cons(boxed car, boxed cdr) {
    auto p = std::shared_ptr<pair>(new pair(car, cdr), [](auto pptr) {
        allocated_pairs.erase(pptr);
        delete pptr;
    });
    allocated_pairs[p.get()] = p;
    return boxin(p);
}

boxed make_vector() {
    auto v = std::shared_ptr<vector>(new vector(), [](auto vptr) {
        allocated_vectors.erase(vptr);
        delete vptr;
    });
    allocated_vectors[v.get()] = v;
    return boxin(v);
}

std::shared_ptr<func> make_lambda2(func fn) {
    auto f = std::shared_ptr<func>(new func(fn), [](auto fptr) {
        allocated_functions.erase(fptr);
        delete fptr;
    });
    allocated_functions[f.get()] = f;
    return f;
}

boxed make_lambda(func fn) {
    return boxin(make_lambda2(fn));
}

boxed make_symbol(const std::string& s) {
    return boxin(std::make_shared<symbol>(s));
}

boxed make_string(const std::string& s) {
    return boxin(std::make_shared<std::string>(s));
}

boxed list_ref(boxed l, size_t i) {
    while (i > 0) {
        l = box_cast<pair>(l)->second;
        --i;
    }

    return box_cast<pair>(l)->first;
}

boxed list_rest(boxed l, size_t i) {
    while (i > 0) {
        l = box_cast<pair>(l)->second;
        --i;
    }

    return box_cast<pair>(l)->second;
}

boxed list_to_vector(boxed l) {
    auto a = make_vector();
    auto v = box_cast<vector>(a);

    while (!box_is_empty(l)) {
        auto p = box_cast<pair>(l);
        v->push_back(p->first);
        l = p->second;
    }

    return a;
}

boxed vector_to_list(boxed vec) {
    auto v = box_cast<vector>(vec);
    auto l = bnil;
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

    auto entry = set_entry(tab, l, cons(bnil, bnil));
    auto ep = box_cast<pair>(entry);
    auto lp = box_cast<pair>(l);
    ep->first = f(lp->first);
    ep->second = f(lp->second);

    return entry;
}

boxed vector_cmap(map_fn f, boxed v, cmap_table& tab, set_entry_fn set_entry) {
    auto ref = tab.find(v);
    if (ref != tab.end()) {
        return ref->second;
    }

    auto entry = set_entry(tab, v, make_vector());
    auto ev = box_cast<vector>(entry);
    auto vv = box_cast<vector>(v);

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
    if (!box_contains<pair>(args)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return box_contains<symbol>(first) &&
           (*box_cast<symbol>(first) == "MCE-YIELD-DEFINITION");
}

boxed get_procedure_defn(boxed proc) {
    return (*box_cast<lambda>(proc))(
        cons(make_symbol("MCE-YIELD-DEFINITION"), bnil));
}

boxed xdisplay(boxed args, std::ostream& out, bool is_write) {
    auto exp = list_ref(args, 0);
    if (box_is_empty(exp)) {
        out << "()";
    } else {
        if (box_contains<bool>(exp)) {
            out << (box_cast<bool>(exp) ? "#t" : "#f");
        } else if (box_contains<double>(exp)) {
            out << box_cast<double>(exp);
        } else if (box_contains<char>(exp)) {
            auto c = box_cast<char>(exp);
            if (is_write) {
                out << "#\\x" << std::hex << static_cast<int>(c);
            } else {
                out << c;
            }
        } else if (box_contains<std::string>(exp)) {
            auto& s = *box_cast<std::string>(exp);
            if (is_write) {
                json j = s;
                out << j.dump();
            } else {
                out << s;
            }
        } else if (box_contains<symbol>(exp)) {
            out << *box_cast<symbol>(exp);
        } else if (box_contains<pair>(exp)) {
            bool first = true;
            std::cout << "(";
            while (box_contains<pair>(exp)) {
                if (!first) {
                    out << " ";
                }
                first = false;
                auto p = box_cast<pair>(exp);
                xdisplay(cons(p->first, bnil), out, is_write);
                exp = p->second;
            }
            if (!box_is_empty(exp)) {
                out << " . ";
                xdisplay(cons(exp, bnil), out, is_write);
            }
            out << ")";
        } else if (box_contains<vector>(exp)) {
            bool first = true;
            out << "#(";
            auto vec = box_cast<vector>(exp);
            for (auto v : *vec) {
                if (!first) {
                    out << " ";
                }
                first = false;
                xdisplay(cons(v, bnil), out, is_write);
            }
            out << ")";
        } else if (box_contains<lambda>(exp)) {
            out << "#<procedure>";
        } else {
            throw std::range_error("unknown display expression");
        }
    }
    return exp;
}

boxed newline(std::ostream& out) {
    out << std::endl;
    return bnil;
}

boxed xprint(boxed args, std::ostream& out) {
    auto r = bnil;
    while (!box_is_empty(args)) {
        auto p = box_cast<pair>(args);
        r = p->first;
        xdisplay(cons(r, bnil), out, false);
        args = p->second;
    }
    newline(out);
    return r;
}

boxed print(boxed args) {
    return xprint(args, std::cout);
}

boxed eprint(boxed args) {
    return xprint(args, std::cerr);
}

boxed xwrite(boxed args, std::ostream& out) {
    auto r = bnil;
    while (!box_is_empty(args)) {
        auto p = box_cast<pair>(args);
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
    return make_lambda([proc, defn](boxed args) -> boxed {
        if (is_yield_defn(args)) {
            if (box_contains<lambda>(defn)) {
                return (*box_cast<lambda>(defn))(bnil);
            }
            return defn;
        }
        //print(cons(serialize(unmemoize(defn)), bnil));
        return (*proc)(args);
    });
}

boxed memoize_lambda(boxed proc, boxed defn) {
    return memoize_lambda(box_cast<lambda>(proc), defn);
}

boxed send(boxed k, boxed v) {
    return cons(k, v);
}

boxed ctenv_lookup(boxed i, boxed env) {
    auto ip = box_cast<pair>(i);
    auto first = box_cast<double>(ip->first);
    auto second = box_cast<double>(ip->second);
    auto bindings = box_cast<pair>(list_ref(env, first));
    auto v = box_cast<vector>(bindings->second);
    if (second < v->size()) {
        return (*v)[second];
    }
    return bnil;
}

boxed ctenv_setvar(boxed name, boxed i, boxed val, boxed env) {
    auto ip = box_cast<pair>(i);
    auto first = box_cast<double>(ip->first);
    auto second = box_cast<double>(ip->second);
    auto bindings = box_cast<pair>(list_ref(env, first));
    auto v = box_cast<vector>(bindings->second);

    if (second >= v->size()) {
        v->resize(second + 1, bnil);
    }
    (*v)[second] = val;

    if (box_is_empty(bindings->first)) {
        bindings->first = cons(bnil, bnil);
    }
    auto p = box_cast<pair>(bindings->first);
    while (second > 0) {
        if (box_is_empty(p->second)) {
            p->second = cons(bnil, bnil);
        }
        p = box_cast<pair>(p->second);
        --second;
    }
    p->first = name;

    return val;
}

boxed symbol_lookup(boxed args) {
    auto i = list_ref(args, 1);
    return make_lambda([i](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return send(k, ctenv_lookup(i, env));
    });
}

boxed send_value(boxed args) {
    auto exp = list_ref(args, 1);
    return make_lambda([exp](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        return send(k, exp);
    });
}

boxed make_step_contn(boxed k, boxed args) {
    return cons(make_symbol("MCE-STEP-CONTN"), cons(k, args));
}

bool is_step_contn(boxed args) {
    if (!box_contains<pair>(args)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return box_contains<symbol>(first) &&
           (*box_cast<symbol>(first) == "MCE-STEP-CONTN");
}

boxed step_contn_k(boxed args) {
    return list_ref(args, 1);
}

boxed step_contn_args(boxed args) {
    return list_rest(args, 1);
}

bool is_transfer(boxed args) {
    if (!box_contains<pair>(args)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return box_contains<symbol>(first) &&
           (*box_cast<symbol>(first) == "MCE-TRANSFER");
}

boxed transfer_args(boxed args) {
    return list_rest(args, 0);
}

boxed transfer(boxed args) {
    auto k = list_ref(args, 0);
    auto fn = list_ref(args, 2);
    return (*box_cast<lambda>(fn))(make_step_contn(k,
        cons(make_symbol("MCE-TRANSFER"), list_rest(args, 2))));
}

boxed make_global_env() {
    auto values = make_vector();
    auto bindings = cons(bnil, values);
    return cons(bindings, bnil);
}

boxed make_env_args(boxed env, boxed args) {
    return cons(make_symbol("MCE-ENV-ARGS"), cons(env, args));
}

bool is_env_args(boxed args) {
    if (!box_contains<pair>(args)) {
        return false;
    }
    auto first = list_ref(args, 0);
    return box_contains<symbol>(first) &&
           (*box_cast<symbol>(first) == "MCE-ENV-ARGS");
}

boxed env_args_env(boxed args) {
    return is_env_args(args) ? list_ref(args, 1) : make_global_env();
}

boxed env_args_args(boxed args) {
    return is_env_args(args) ? list_rest(args, 1) : args;
}

boxed applyx(boxed k, boxed env, boxed fn, boxed args) {
    return (*box_cast<lambda>(fn))(
        make_step_contn(k, make_env_args(env, args)));
}

boxed result(boxed exp) {
    auto a = make_vector();
    auto v = box_cast<vector>(a);
    v->push_back(make_symbol("MCE-RESULT"));
    v->push_back(exp);
    return a;
}

bool is_result(boxed exp) {
    if (!box_contains<vector>(exp)) {
        return false;
    }
    auto v = box_cast<vector>(exp);
    return (v->size() == 2) &&
           box_contains<symbol>((*v)[0]) &&
           (*box_cast<symbol>((*v)[0]) == "MCE-RESULT");
}

boxed result_val(boxed exp) {
    return (*box_cast<vector>(exp))[1];
}

boxed less_than(boxed args) {
    bool r = box_cast<double>(list_ref(args, 0)) <
             box_cast<double>(list_ref(args, 1));
    return boxin(r);
}

boxed greater_than(boxed args) {
    bool r = box_cast<double>(list_ref(args, 0)) >
             box_cast<double>(list_ref(args, 1));
    return boxin(r);
}

boxed plus(boxed args) {
    double r = 0;
    while (!box_is_empty(args)) {
        auto p = box_cast<pair>(args);
        r += box_cast<double>(p->first);
        args = p->second;
    }
    return boxin(r);
}

boxed multiply(boxed args) {
    double r = 1;
    while (!box_is_empty(args)) {
        auto p = box_cast<pair>(args);
        r *= box_cast<double>(p->first);
        args = p->second;
    }
    return boxin(r);
}

boxed is_null(boxed args) {
    return boxin(box_is_empty(list_ref(args, 0)));
}

boxed is_string(boxed args) {
    auto a = list_ref(args, 0);
    return boxin(!box_is_empty(a) && box_contains<std::string>(a));
}

boxed is_pair(boxed args) {
    auto a = list_ref(args, 0);
    return boxin(!box_is_empty(a) && box_contains<pair>(a));
}

boxed is_procedure(boxed args) {
    auto a = list_ref(args, 0);
    return boxin(!box_is_empty(a) && box_contains<lambda>(a));
}

boxed is_vector(boxed args) {
    auto a = list_ref(args, 0);
    return boxin(!box_is_empty(a) && box_contains<vector>(a));
}

boxed vector_length(boxed args) {
    auto a = list_ref(args, 0);
    return boxin(static_cast<double>(box_cast<vector>(a)->size()));
}

boxed vector_ref(boxed args) {
    auto a = list_ref(args, 0);
    auto i = list_ref(args, 1);
    return box_cast<vector>(a)->at(box_cast<double>(i));
}

boxed is_string_equal(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return boxin(*box_cast<std::string>(x) == *box_cast<std::string>(y));
}

boxed is_number_equal(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);
    return boxin(box_cast<double>(x) == box_cast<double>(y));
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
    box_cast<pair>(p)->first = v;
    return p;
}

boxed set_cdr(boxed args) {
    auto p = list_ref(args, 0);
    auto v = list_ref(args, 1);
    box_cast<pair>(p)->second = v;
    return p;
}

boxed is_eq(boxed args) {
    auto x = list_ref(args, 0);
    auto y = list_ref(args, 1);

    if (box_is_empty(x)) {
        return boxin(box_is_empty(y));
    }

    if (box_is_empty(y)) {
        return bfalse;
    }

    if (!boxes_contain_same_type(x, y)) {
        return bfalse;
    }

    if (box_contains<bool>(x)) {
        return boxin(box_cast<bool>(x) == box_cast<bool>(y));
    }

    if (box_contains<char>(x)) {
        return boxin(box_cast<char>(x) == box_cast<char>(y));
    }

    if (box_contains<double>(x)) {
        return boxin(box_cast<double>(x) == box_cast<double>(y));
    }

    if (box_contains<std::string>(x)) {
        return boxin(box_cast<std::string>(x) == box_cast<std::string>(y));
    }

    if (box_contains<symbol>(x)) {
        return boxin(*box_cast<symbol>(x) == *box_cast<symbol>(y));
    }

    if (box_contains<pair>(x)) {
        return boxin(box_cast<pair>(x) == box_cast<pair>(y));
    }

    if (box_contains<vector>(x)) {
        return boxin(box_cast<vector>(x) == box_cast<vector>(y));
    }

    if (box_contains<lambda>(x)) {
        return boxin(box_cast<lambda>(x) == box_cast<lambda>(y));
    }

    return bfalse;
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
    return make_string(mce_save(list_ref(args, 0)));
}

boxed restore(boxed args) {
    auto a = list_ref(args, 0);
    return mce_restore(*box_cast<std::string>(a));
}

boxed getpid(boxed) {
    return boxin(static_cast<double>(getpid()));
}

boxed gcons(boxed args) {
    return cons(list_ref(args, 0), list_ref(args, 1));
}

std::unordered_map<std::string, function*> global_table {
    { "result", result },
    { "<", less_than },
    { ">", greater_than },
    { "print", print },
    { "eprint", eprint },
    { "write", write },
    { "ewrite", write },
    { "+", plus },
    { "*", multiply },
    { "null?" , is_null },
    { "car", car },
    { "cdr", cdr },
    { "set-car!", set_car },
    { "set-cdr!", set_cdr },
    { "eq?" , is_eq },
    { "=" , is_number_equal },
    { "string?", is_string },
    { "pair?", is_pair },
    { "procedure?", is_procedure },
    { "string=?", is_string_equal },
    { "vector?" , is_vector },
    { "vector-length", vector_length },
    { "vector-ref", vector_ref },
    { "unmemoize", gunmemoize },
    { "serialize", gserialize },
    { "apply", gapplyx },
    { "save", save },
    { "restore", restore },
    { "transfer", transfer },
    { "getpid", getpid },
    { "cons", gcons }
};

std::unordered_set<function*> kenvfn_set {
    gapplyx,
    transfer
};

boxed find_global(const symbol& sym) {
    auto it = global_table.find(sym);
    if (it == global_table.end()) {
        throw std::range_error(sym);
    }
    return make_lambda(it->second);
}

boxed step(boxed state) {
    return (*box_cast<lambda>(list_ref(state, 0)))(
        cons(list_rest(state, 0), bnil));
}

boxed run(boxed state) {
    while (!is_result(state)) {
        state = maybe_gc(step(state));
    }

    return result_val(state);
}

boxed globalize(boxed x, boxed args, boxed cf);

boxed handle_global_lambda(boxed args, boxed fn, boxed cf) {
    auto f = box_cast<lambda>(fn);

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

boxed lookup_global(const symbol& sym);

boxed handle_global_lambda_kenv(boxed args, boxed fn) {
    auto f = box_cast<lambda>(fn);

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

boxed wrap_global_lambda(boxed fn, boxed cf) {
    const lambda l = box_cast<lambda>(fn);
    auto p = l->target<function*>();

    if (p && (kenvfn_set.find(*p) != kenvfn_set.end())) {
        return make_lambda( [fn](boxed args) -> boxed {
            return handle_global_lambda_kenv(args, fn);
        });
    }

    return make_lambda( [fn, cf](boxed args) -> boxed {
        return handle_global_lambda(args, fn, cf);
    });
}

boxed extend_env(boxed env, boxed syms, boxed values) {
    return cons(cons(syms, list_to_vector(values)), env);
}

boxed improper_extend_env(boxed env, boxed syms, boxed values) {
    vector s;
    auto av = make_vector();
    auto v = box_cast<vector>(av);

    while (!box_is_empty(syms)) {
        if (box_contains<symbol>(syms)) {
            s.push_back(syms);
            v->push_back(values);
            break;
        }
        if (box_is_empty(values)) {
            break;
        }

        s.push_back(list_ref(syms, 0));
        v->push_back(list_ref(values, 0));
        
        syms = list_rest(syms, 0);
        values = list_rest(values, 0);
    }

    auto as = bnil;
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
    auto f = box_cast<lambda>(fn);

    if (is_step_contn(args)) {
        auto sca = step_contn_args(args);
        return (*f)(cons(step_contn_k(args),
                         cons(extend_env(env, params, env_args_args(sca)),
                              bnil)));
    }

    return run((*f)(cons(lookup_global(symbol("result")),
                         cons(extend_env(env, params, env_args_args(args)),
                              bnil))));
}

boxed handle_contn_lambda(boxed args, boxed k) {
    auto f = box_cast<lambda>(k);

    if (is_step_contn(args)) {
        return (*f)(env_args_args(step_contn_args(args)));
    }

    return run((*f)(env_args_args(args)));
}

boxed constructed_function(boxed args) {
    auto self = list_ref(args, 0);
    auto args2 = list_ref(args, 1);
    auto cf = list_ref(args, 2);
    auto r = (*box_cast<lambda>(cf))(args2);
    if (box_contains<lambda>(r)) {
        return wrap_global_lambda(r, self);
    }
    return r;
}

boxed global_lambda(boxed args) {
    auto self = list_ref(args, 0);
    auto defn = box_cast<symbol>(list_ref(args, 1));
    return wrap_global_lambda(find_global(*defn), self);
}

boxed evalx_initial(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda([k, env, scanned] (boxed) -> boxed {
        return (*box_cast<lambda>(scanned))(cons(k, cons(env, bnil)));
    });
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

    auto f = boxin();
    auto f2 = memoize_lambda(make_lambda2([f](boxed args) -> boxed {
        return (*box_cast<lambda>(f))(args);
    }), defn);
    *f = *memoize_lambda(forms[box_cast<double>(n)](cons(f2, args)), defn);

    return f;
}

boxed make_form(enum forms n, boxed args) {
    return make_form(boxin(static_cast<double>(n)), args);
}

boxed make_form(boxed args) {
    auto p = box_cast<pair>(args);
    return make_form(p->first, p->second);
}

boxed aform(enum forms n) {
    return boxin(static_cast<double>(n));
}

boxed lookup_global(const symbol& sym) {
    auto r = find_global(sym);
    auto defn = cons(aform(forms::global_lambda), cons(make_symbol(sym), bnil));
    auto f = boxin();
    auto f2 = memoize_lambda(make_lambda2([f](boxed args) -> boxed {
        return (*box_cast<lambda>(f))(args);
    }), defn);
    *f = *memoize_lambda(wrap_global_lambda(r, f2), defn);
    return f;
}

boxed globalize(boxed x, boxed args, boxed cf) {
    if (!box_contains<lambda>(x)) {
        return x;
    }

    auto defn = cons(aform(forms::constructed_function),
                     cons(args, cons(cf, bnil)));
    auto f = boxin();
    auto f2 = memoize_lambda(make_lambda2([f](boxed args) -> boxed {
        return (*box_cast<lambda>(f))(args);
    }), defn);
    *f = *memoize_lambda(wrap_global_lambda(x, f2), defn);
    return f;
}

boxed if1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto scan1 = list_ref(args, 3);
    auto scan2 = list_ref(args, 4);
    return make_lambda([k, env, scan1, scan2](boxed args) -> boxed {
        auto v = box_cast<bool>(list_ref(args, 0));
        auto f = box_cast<lambda>(v ? scan1 : scan2);
        return (*f)(cons(k, cons(env, bnil)));
    });
}

boxed if0(boxed args) {
    auto scan0 = list_ref(args, 1);
    auto scan1 = list_ref(args, 2);
    auto scan2 = list_ref(args, 3);
    return make_lambda([scan0, scan1, scan2](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(scan0))(
            cons(make_form(forms::if1,
                           cons(k, cons(env, cons(scan1, cons(scan2, bnil))))),
                 cons(env, bnil)));
    });
}

boxed sclis2(boxed args) {
    auto k = list_ref(args, 1);
    auto v = list_ref(args, 2);
    return make_lambda([k, v](boxed args) -> boxed {
        auto w = list_ref(args, 0);
        return send(k, cons(v, w));
    });
}

boxed sclis1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return make_lambda([k, env, rest](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        return (*box_cast<lambda>(rest))(
            cons(make_form(forms::sclis2, cons(k, cons(v, bnil))),
                 cons(env, bnil)));
    });
}

boxed sclis0(boxed args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return make_lambda([first, rest](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(first))(
            cons(make_form(forms::sclis1,
                           cons(k, cons(env, cons(rest, bnil)))),
                 cons(env, bnil)));
    });
}

boxed scseq1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto rest = list_ref(args, 3);
    return make_lambda([k, env, rest](boxed) -> boxed {
        return (*box_cast<lambda>(rest))(cons(k, cons(env, bnil)));
    });
}

boxed scseq0(boxed args) {
    auto first = list_ref(args, 1);
    auto rest = list_ref(args, 2);
    return make_lambda([first, rest](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(first))(
            cons(make_form(forms::scseq1,
                           cons(k, cons(env, cons(rest, bnil)))),
                 cons(env, bnil)));
    });
}

boxed lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return make_lambda([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, extend_env);
    });
}

boxed lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda([params, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return send(k,
                    make_form(forms::lambda1,
                              cons(params, cons(scanned, cons(env, bnil)))));
    });
}

boxed improper_lambda1(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    auto env = list_ref(args, 3);
    return make_lambda([params, scanned, env](boxed args) -> boxed {
        return handle_lambda(args, params, scanned, env, improper_extend_env);
    });
}

boxed improper_lambda0(boxed args) {
    auto params = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda([params, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return send(k,
                    make_form(forms::improper_lambda1,
                              cons(params, cons(scanned, cons(env, bnil)))));
    });
}

boxed letcc1(boxed args) {
    auto k = list_ref(args, 1);
    return make_lambda([k](boxed args) -> boxed {
        return handle_contn_lambda(args, k); 
    });
}

boxed letcc0(boxed args) {
    auto name = list_ref(args, 1);
    auto scanned = list_ref(args, 2);
    return make_lambda([name, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(scanned))(
            cons(k,
                 cons(extend_env(env,
                                 cons(name, bnil),
                                 cons(make_form(forms::letcc1, cons(k, bnil)),
                                      bnil)),
                      bnil)));
    });
}

boxed define1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    auto name = list_ref(args, 3);
    auto i = list_ref(args, 4);
    return make_lambda([k, env, name, i](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        return send(k, ctenv_setvar(name, i, v, env));
    });
}

boxed define0(boxed args) {
    auto name = list_ref(args, 1);
    auto i = list_ref(args, 2);
    auto scanned = list_ref(args, 3);
    return make_lambda([name, i, scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(scanned))(
            cons(make_form(forms::define1,
                           cons(k, cons(env, cons(name, cons(i, bnil))))),
                 cons(env, bnil)));
    });
}

boxed application1(boxed args) {
    auto k = list_ref(args, 1);
    auto env = list_ref(args, 2);
    return make_lambda([k, env](boxed args) -> boxed {
        auto v = list_ref(args, 0);
        return applyx(k, env, list_ref(v, 0), list_rest(v, 0));
    });
}

boxed application0(boxed args) {
    auto scanned = list_ref(args, 1);
    return make_lambda([scanned](boxed args) -> boxed {
        auto k = list_ref(args, 0);
        auto env = list_ref(args, 1);
        return (*box_cast<lambda>(scanned))(
            cons(make_form(forms::application1, cons(k, cons(env, bnil))),
                 cons(env, bnil)));
    });
}

bool is_unmemoized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           box_contains<symbol>((*v)[0]) &&
           (*box_cast<symbol>((*v)[0]) == "MCE-UNMEMOIZED");
}

boxed unmemoized_repexp(std::shared_ptr<vector> v) {
    return (*v)[1];
}

boxed memoize_aux(boxed exp, cmap_table& tab, map_fn fn) {
    if (box_is_empty(exp)) {
        return exp;
    }

    if (box_contains<pair>(exp)) {
        return cmap(fn, exp, tab, table_set);
    }

    if (box_contains<vector>(exp)) {
        auto v = box_cast<vector>(exp);
        if (is_unmemoized(v)) {
            auto repexp = unmemoized_repexp(v);
            auto r = boxin();
            auto f = boxin();
            auto entry = table_set(tab, exp, memoize_lambda(
                make_lambda2([f](boxed args) -> boxed {
                    return (*box_cast<lambda>(f))(args);
                }),
                make_lambda([r] (boxed) -> boxed {
                    return r;
                })));
            *r = *fn(repexp);
            *f = make_lambda2([r, f](boxed args) -> boxed {
                    *f = *make_form(r);
                    return (*box_cast<lambda>(f))(args);
                });
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
    if (box_is_empty(exp)) {
        return exp;
    }

    if (box_contains<pair>(exp)) {
        return cmap(fn, exp, tab, table_set);
    }

    if (box_contains<vector>(exp)) {
        return vector_cmap(fn, exp, tab, table_set);
    }

    if (box_contains<lambda>(exp)) {
        auto ref = tab.find(exp);
        if (ref != tab.end()) {
            return ref->second;
        }

        auto entry = table_set(tab, exp, make_vector());
        auto v = box_cast<vector>(entry);

        v->push_back(make_symbol("MCE-UNMEMOIZED"));
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

boxed make_serialized(size_t n) {
    auto a = make_vector();
    auto v = box_cast<vector>(a);

    v->push_back(make_symbol("MCE-SERIALIZED"));
    v->push_back(boxin(static_cast<double>(n)));

    return a;
}

bool is_serialized(std::shared_ptr<vector> v) {
    return (v->size() == 2) &&
           box_contains<symbol>((*v)[0]) &&
           (*box_cast<symbol>((*v)[0]) == "MCE-SERIALIZED");
}

boxed serialized_n(std::shared_ptr<vector> v) {
    return (*v)[1];
}

boxed serialize_aux(boxed exp,
                    cmap_table& tab,
                    map_fn fn,
                    set_entry_fn set_entry) {
    if (box_is_empty(exp)) {
        return exp;
    }

    if (box_contains<pair>(exp)) {
        return cmap(fn, exp, tab, set_entry);
    }

    if (box_contains<vector>(exp)) {
        return vector_cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

boxed serialize(boxed exp) {
    size_t counter = 0;
    cmap_table tab;
    set_entry_fn set_entry = [&counter](cmap_table& tab, boxed v, boxed entry) {
        table_set(tab, v, make_serialized(counter++));
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
    if (box_is_empty(exp)) {
        return exp;
    }

    if (box_contains<pair>(exp)) {
        return cmap(fn, exp, tab, set_entry);
    }

    if (box_contains<vector>(exp)) {
        auto v = box_cast<vector>(exp);
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
        return table_set(tab, boxin(static_cast<double>(counter++)), entry);
    };
    map_fn fn = [&tab, &fn, &set_entry](boxed x) -> boxed {
        return deserialize_aux(x, tab, fn, set_entry);
    };
    return fn(exp);
}

json pickle_aux(boxed exp) {
    json j;
    if (box_is_empty(exp)) {
        j.push_back(std::string(1, null_code));
    } else {
        if (box_contains<bool>(exp)) {
            j.push_back(std::string(1, boolean_code));
            j.push_back(box_cast<bool>(exp) ? "t" : "f");
        } else if (box_contains<double>(exp)) {
            j.push_back(std::string(1, number_code));
            j.push_back(box_cast<double>(exp));
        } else if (box_contains<char>(exp)) {
            j.push_back(std::string(1, char_code));
            j.push_back(std::string(1, box_cast<char>(exp)));
        } else if (box_contains<std::string>(exp)) {
            j.push_back(std::string(1, string_code));
            j.push_back(*box_cast<std::string>(exp));
        } else if (box_contains<symbol>(exp)) {
            j.push_back(std::string(1, symbol_code));
            j.push_back(*box_cast<symbol>(exp));
        } else if (box_contains<pair>(exp)) {
            auto p = box_cast<pair>(exp);
            j.push_back(std::string(1, pair_code));
            j.push_back(pickle_aux(p->first));
            j.push_back(pickle_aux(p->second));
        } else if (box_contains<vector>(exp)) {
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

boxed unpickle_aux(const json& j) {
    switch (j[0].get<std::string>()[0]) {
    case boolean_code:
        return boxin(j[1].get<std::string>() == "t");
    case number_code:
        return boxin(j[1].get<double>());
    case char_code:
        return boxin(j[1].get<std::string>()[0]);
    case string_code:
        return make_string(j[1].get<std::string>());
    case symbol_code:
        return make_symbol(j[1].get<std::string>());
    case pair_code:
        return cons(unpickle_aux(j[1]), unpickle_aux(j[2]));
    case vector_code:
        return list_to_vector(unpickle_aux(j[1]));
    default:
        return bnil;
    }
}

boxed unpickle(const std::string& s) {
    return unpickle_aux(json::parse(s));
}

std::string mce_save(boxed exp) {
    return pickle(serialize(unmemoize(exp)));
}

boxed mce_restore(const std::string& s) {
    return memoize(deserialize(unpickle(s)));
}
