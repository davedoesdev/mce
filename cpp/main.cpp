#include <iostream>
#include "json.hpp"
#include "cxxopts.hpp"
#include "mce.hpp"

using nlohmann::json;

int main(int argc, char *argv[]) {
    cxxopts::Options options("mce", "Metacircular Evaluator");
    options.add_options()
        ("h,help", "help")
        ("gc-threshold",
         "gc when object table exceeds this number",
         cxxopts::value<size_t>()->default_value("100000"));
    auto opts = options.parse(argc, argv);
    if (opts.count("help")) {
        std::cout << options.help() << std::endl;
        return 0;
    }
    gc_threshold = opts["gc-threshold"].as<size_t>();

    json s;
    std::cin >> s;
    auto r = mce_restore(s.get<std::string>());
    if (r->contains<lambda>()) {
        (*r->cast<lambda>())(cons(bnil, bnil));
    } else {
        run(r);
    }
    return 0;
}
