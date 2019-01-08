#include <iostream>
#include <memory>
#include <utility>
#include <vector>
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
typedef std::function<std::shared_ptr<any>(std::shared_ptr<any>)> lambda;

std::shared_ptr<any> cons(std::shared_ptr<any> car, std::shared_ptr<any> cdr) {
    return std::make_shared<any>(std::make_shared<pair>(car, cdr));
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

        auto entry = cons(std::make_shared<any>(), std::make_shared<any>());
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

    auto p = any_cast<std::shared_ptr<pair>>(*args);
    return (p->first->type() == typeid(symbol)) &&
           (any_cast<symbol>(*p->first) == "MCE-YIELD-DEFINITION");
}

std::shared_ptr<any> memoize_lambda(lambda proc, std::shared_ptr<any> defn) {
    return std::make_shared<any>(
        [proc, defn](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            if (is_yield_defn(args)) {
                return defn;
            }
            return proc(args);
        });
}

std::vector<lambda> forms;

std::shared_ptr<any> make_form(std::shared_ptr<any> n,
                               std::shared_ptr<any> args) {
    auto defn = cons(n, args);

    std::shared_ptr<any> f;
    std::shared_ptr<any> f2 = memoize_lambda(
        [f](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return any_cast<lambda>(f)(args);
        },
        defn);
    f = memoize_lambda(
        [n, f2](std::shared_ptr<any> args) -> std::shared_ptr<any> {
            return forms[any_cast<double>(n)](cons(f2, args));
        },
        defn);

    return f;
}

std::shared_ptr<any> make_form(std::shared_ptr<any> args) {
    auto p = any_cast<std::shared_ptr<pair>>(*args);
    return make_form(p->first, p->second);
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
        if ((v->size() == 2) &&
            ((*v)[0]->type() == typeid(symbol)) &&
            (any_cast<symbol>(*(*v)[0]) == "MCE-UNMEMOIZED")) {
            auto repexp = (*v)[1];
            auto r = std::make_shared<any>(symbol("unspecified"));
            auto entry = table_set(tab, exp, memoize_lambda(
                [r](std::shared_ptr<any> args) -> std::shared_ptr<any> {
                    return any_cast<lambda>(*r)(args);
                }, repexp));
            (*r) = make_form(fn(repexp));
            return r;
        }
        return vector_cmap(fn, exp, tab, table_set);
    }

    return exp;
}

std::shared_ptr<any> memoize(std::shared_ptr<any> exp) {
    cmap_table tab;
    map_fn f = [&tab,&f](std::shared_ptr<any> x)->std::shared_ptr<any> {
        return memoize_aux(x, tab, f);
    };
    return f(exp);
}

std::shared_ptr<any> deserialize_aux(std::shared_ptr<any> exp,
                                     cmap_table& tab,
                                     map_fn fn,
                                     set_entry_fn set_entry) {
    auto& type = exp->type();

    if (type == typeid(std::shared_ptr<pair>)) {
        return cmap(fn, exp, tab, set_entry);
    }

    return exp;
}

std::shared_ptr<any> deserialize(std::shared_ptr<any> exp) {
    unsigned int counter = 0;
    cmap_table tab;
    set_entry_fn set_entry = [](cmap_table& tab,
                                std::shared_ptr<any> v,
                                std::shared_ptr<any> entry) {
        table_set(tab, std::make_shared<any>(counter++), entry);
    };
    map_fn f= [](std::shared_ptr<any> x)->std::shared_ptr<any> {
        return deserialize_aux(x, tab, fn, set_entry);
    };
    return f(exp);
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
        return std::make_shared<any>(j[1].get<std::string>());
    case symbol_code:
        return std::make_shared<any>(symbol(j[1].get<std::string>()));
    case pair_code:
        return cons(unpickle_aux(j[1]), unpickle_aux(j[2]));
    case vector_code:
        return list_to_vector(unpickle_aux(j[1]));
    default:
        return std::make_shared<any>();
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
