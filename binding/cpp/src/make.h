#ifndef MAKE_H
#define MAKE_H

#include "ip.h"

namespace xdb {

class make_t {
public:
    make_t(const string &src, const string &dst, int version);

private:
    void vector_index_push_back(int row, int col, const node_t &node);
    void vector_index_push_back(node_t &node);
    void handle_input_help(char buf[]);
    void handle_input(const std::string &file_name);

    void handle_header();
    void handle_vector_index();
    void handle_region();
    void handle_content();

    FILE *db = NULL;

    std::vector<std::pair<string, string>> vector_index[256][256];

    std::unordered_map<string, unsigned> region;

    unsigned region_index;
    ip_t     next_ip;
};

}  // namespace xdb

#endif
