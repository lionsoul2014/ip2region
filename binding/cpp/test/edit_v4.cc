
#include "../src/edit.h"

int main() {
    std::string file_name_old = "../../data/ipv4_source.txt";
    std::string file_name_new = "./1.txt";
    xdb::edit_t xdb(file_name_old, file_name_new, xdb::ipv4);
    return 0;
}
