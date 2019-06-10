#include "mce.hpp"

int main(int argc, char *argv[]) {
    start(argc, argv, std::make_shared<Runtime>());
    return 0;
}
