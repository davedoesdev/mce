#include <iostream>
#include <boost/program_options.hpp>
#include "json.hpp"
#include "mce.hpp"

using nlohmann::json;
using namespace boost::program_options;

int main(int argc, char *argv[]) {
    options_description desc("Options");
    desc.add_options()
        ("help", "help")
        ("gc-threshold",
         value<size_t>()->default_value(100000),
         "gc when object table exceeds this number");
    variables_map vm;
    store(parse_command_line(argc, argv, desc), vm);
    if (vm.count("help")) {
        std::cout << desc << std::endl;
        return 0;
    }
    gc_threshold = vm["gc-threshold"].as<size_t>();

    json s;
    std::cin >> s;
    auto r = mce_restore(s.get<std::string>());
    if (r->type() == typeid(lambda)) {
        (*box_cast<lambda>(*r))(cons(nil, nil));
    } else {
        run(r);
    }
    return 0;
}
