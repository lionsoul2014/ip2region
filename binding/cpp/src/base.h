#ifndef BASE_H
#define BASE_H

#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include <algorithm>
#include <iostream>
#include <list>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

namespace xdb {

using std::string;

constexpr int ipv4 = 4;
constexpr int ipv6 = 6;

constexpr int policy_file    = 0;
constexpr int policy_vector  = 1;
constexpr int policy_content = 2;

constexpr int length_header = 256;
constexpr int length_vector = 256 * 256 * 8;

extern int ip_version;  // ip 版本
extern int ip_size;     // ip 占的字节数
extern int content_size;

void init_xdb(int version);

void log_exit(const string &msg);

void read_bin(int index, char *buf, size_t len, FILE *db);

unsigned to_uint(const char *buf);
unsigned to_ushort(const char *buf);
unsigned to_int(const char *buf, int n);

void write_uint(unsigned data, char buf[]);
void write_uint(unsigned data, FILE *dst);

void write_ushort(unsigned data, char buf[]);
void write_ushort(unsigned data, FILE *dst);

void write_string(const char *buf, unsigned len, FILE *dst);

unsigned long long get_time();

}  // namespace xdb

#endif
