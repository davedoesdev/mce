#include <iostream>
#include "mce.hpp"

int main(int argc, char *argv[]) {
    std::string help, run;
    config(argc, argv, help, run);
    if (!help.empty()) {
        std::cout << help << std::endl;
    } else if (!run.empty()) {
        start(run);
    } else {
        start(std::cin);
    }
    return 0;
}
