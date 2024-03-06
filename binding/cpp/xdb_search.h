#ifndef XDB_SEARCH_H
#define XDB_SEARCH_H

#include <string>

class xdb_search_t {
  public:
    xdb_search_t(const std::string &file_name);
    ~xdb_search_t();

    void init_file();
    void init_vector_index();
    void init_content();

    unsigned long long get_io_count();
    unsigned long long get_cost_time();

    std::string search(const std::string &ip);

  private:
    void get_content_index(unsigned int  ip,
                           unsigned int &left,
                           unsigned int &right);

    void get_content(unsigned int    index,
                     unsigned int   &ip_left,
                     unsigned int   &ip_right,
                     unsigned short &region_len,
                     unsigned int   &region_index);

    std::string get_region(unsigned int index, unsigned short len);

    std::string search(unsigned int ip_uint);

    FILE              *db;
    char              *vector_index;
    char              *content;
    unsigned long long io_count;
    unsigned long long cost_time;

    static constexpr int header_length     = 256;
    static constexpr int vector_index_rows = 256;
    static constexpr int vector_index_cols = 256;
    static constexpr int vector_index_size = 8;
    static constexpr int vector_index_length =
        vector_index_rows * vector_index_cols * vector_index_size;
    static constexpr int segment_index_size = 14;
};

#endif
