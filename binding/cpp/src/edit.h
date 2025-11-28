#ifndef EDIT_H
#define EDIT_H

#include "ip.h"

namespace xdb {

class edit_t {
public:
    edit_t(const string& old_name, const string& new_name, int version);

private:
    void handle_new_file(const string& file_name);
    void handle_old_file(const string& file_name);
    void merge();
    void write_old_file(const string& file_name);

    std::list<node_t> old_regions;
    std::list<node_t> new_regions;
};

}  // namespace xdb

#endif
