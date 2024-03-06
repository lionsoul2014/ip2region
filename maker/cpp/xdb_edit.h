#ifndef XDB_EDIT_H
#define XDB_EDIT_H

#include <list>
#include <string>

struct xdb_node_t {
    unsigned int ip1;
    unsigned int ip2;
    std::string  region;

    xdb_node_t();
    xdb_node_t(char* buf);

    bool operator<(const xdb_node_t& rhs) const;

    std::string to_string() const;
};

class xdb_edit_t {
  public:
    xdb_edit_t(const std::string& file_name_old,
               const std::string& file_name_new);

  private:
    void handle_new_file(const std::string& file_name);
    void handle_old_file(const std::string& file_name);
    void merge();
    void write_old_file(const std::string& file_name);

    std::list<xdb_node_t> old_regions;
    std::list<xdb_node_t> new_regions;
};

#endif
