#ifndef SEARCH_H
#define SEARCH_H

#include "header.h"
#include "ip.h"

namespace xdb {

class search_t {
protected:
    FILE *db;

    header_t header;

    int policy;

    int io_count;
    int cost_time;

    char  vector[length_vector];
    char *content;

public:
    search_t(const string &file_name, int version, int policy);
    virtual ~search_t();

    int get_io_count();
    int get_cost_time();

    string search(const string &str);

protected:
    string search(const ip_t &ip);

    void get_content_index(const ip_t &ip1, int &left, int &right);
    void get_content(int   index,
                     ip_t &left,
                     ip_t &right,
                     int  &region_len,
                     int  &region_index);

    char const *get_content_index_help(int index);
    char const *get_content_help(int index);
    string      get_region(int index, int len);
};

}  // namespace xdb

#endif
