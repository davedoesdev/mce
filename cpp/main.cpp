#include <iostream>
#include "mce.hpp"

int main(int argc, char *argv[]) {
    if (!config(argc, argv)) {
        return 0;
    }
    start(std::cin);
    return 0;
}
