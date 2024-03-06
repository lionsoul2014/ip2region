#ifndef XDB_MAKE_H
#define XDB_MAKE_H

#include <stdio.h>

#include <string>
#include <unordered_map>
#include <vector>

class xdb_make_t {
  public:
    xdb_make_t(const std::string &file_name_src,
               const std::string &file_name_dst);

  private:
    void vector_index_push_back(unsigned int row,
                                unsigned int col,
                                unsigned int ip1,
                                unsigned int ip2,
                                const char  *region);
    void vector_index_push_back(unsigned int ip1,
                                unsigned int ip2,
                                const char  *region);
    void handle_input_help(char buf[]);
    void handle_input(const std::string &file_name);

    void handle_header();
    void handle_vector_index();
    void handle_region();
    void handle_content();

    static constexpr int header_length     = 256;
    static constexpr int vector_index_rows = 256;
    static constexpr int vector_index_cols = 256;
    static constexpr int vector_index_size = 8;
    static constexpr int vector_index_length =
        vector_index_rows * vector_index_cols * vector_index_size;
    static constexpr int segment_index_size = 14;

    FILE *dst = NULL;

    std::vector<std::pair<std::string, std::string>>
        vector_index[vector_index_rows][vector_index_cols];

    std::unordered_map<std::string, unsigned int> region;
};

#endif
