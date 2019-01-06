#include <iostream>
#include <memory>
#include <vector>
#include <boost/any.hpp>
#include "json.hpp"

using nlohmann::json;
using boost::any;

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

class pair {
public:
    pair(std::shared_ptr<any> car, std::shared_ptr<any> cdr) :
        car(car), cdr(cdr) {}

    std::shared_ptr<any> car;
    std::shared_ptr<any> cdr;
};

std::shared_ptr<any> list_to_vector(std::shared_ptr<any> l) {
    std::shared_ptr<any> a = std::make_shared<any>(
        std::make_shared<std::vector<std::shared_ptr<any>>>());
    
    return nullptr;
}

std::shared_ptr<any> cons(std::shared_ptr<any> car, std::shared_ptr<any> cdr) {
    return std::make_shared<any>(std::make_shared<pair>(car, cdr));
}

std::shared_ptr<any> memoize(std::shared_ptr<any> exp) {
    return exp;
}

std::shared_ptr<any> deserialize(std::shared_ptr<any> exp) {
    return exp;
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
        return std::make_shared<any>(nullptr);
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
