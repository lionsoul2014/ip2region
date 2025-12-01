#ifndef HEADER_H
#define HEADER_H

#include "base.h"

namespace xdb {

class header_t {
public:
    header_t(FILE* db);
    virtual ~header_t();

    int version();       // 版本号
    int index_policy();  // 缓存策略
    int create_at();     // 文件生成时间
    int index_start();   // 索引起始地址
    int index_end();     // 索引结束地址
    int ip_version();    // IP 版本
    int ptr();           // 指针字节数

protected:
    char header[length_header];
};

}  // namespace xdb

#endif
