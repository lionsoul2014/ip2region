// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/27

#include "stdio.h"
#include "xdb_searcher.h"

int main(int argc, char *argv[]) {
    char *ip_list[] = {
        "1.2.3.4", "192.168.2.3", "120.24.78.129", "255.255.255.0",
        "256.7.12.9", "12.56.78.320", "32.12.45.192", "222.221.220.219",
        "192.168.1.101 ", "132.96.12.98a", "x23.12.2.12"
    };

    int errcode, i;
    unsigned int ip;
    char ip_buff[16] = {'\0'};
    for (i = 0; i < 11; i++) {
        errcode = check_ip(ip_list[i], &ip);
        if (errcode != 0) {
            printf("invalid ip address `%s`\n", ip_list[i]);
            continue;
        }

        long2ip(ip, ip_buff);
        printf("long(%-15s)=%-10u, long2ip(%-10u)=%-15s", ip_list[i], ip, ip, ip_buff);
        if (strcmp(ip_list[i], ip_buff) != 0) {
            printf(" --[Failed]\n");
        } else {
            printf(" --[Ok]\n");
        }
    }

    return 0;
}