#ifndef HEADER_H
#define HEADER_H

#include "base.h"

namespace xdb {

class header_t {
public:
    header_t(FILE* db);
    virtual ~header_t();

    int version();       // xdb structure version
    int index_policy();  // cache policy
    int create_at();     // created unix timestamp
    int index_start();   // index start ptr
    int index_end();     // index end ptr
    int ip_version();    // IP version
    int ptr();           // ptr bytes

protected:
    char header[length_header];
};

}  // namespace xdb

#endif
