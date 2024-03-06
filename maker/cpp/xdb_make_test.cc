
#include "xdb_make.h"

#include <getopt.h>
#include <stdio.h>

#include <iostream>

void print_help() {
    printf("./xdb_make [command options]\n");
    printf("options:\n");
    printf(" --db string        ip2region binary xdb file path\n");
    printf(" --src string       source ip text file path\n");
    exit(-1);
}

int main(int argc, char* argv[]) {
    struct option long_options[] = {
        {"db",   required_argument, 0, 'd'},
        {"src",  required_argument, 0, 's'},
        {"help", no_argument,       0, 'h'},
        {0,      0,                 0, 0  }
    };

    std::string file_name_dst = "./ip2region.xdb";
    std::string file_name_src = "../../data/ip.merge.txt";

    while (1) {
        int c = getopt_long(argc, argv, "", long_options, NULL);
        if (c == -1)
            break;
        switch (c) {
        case 'd':
            file_name_dst = optarg;
            break;
        case 'h':
            print_help();
            break;
        case 's':
            file_name_src = optarg;
            break;
        case '?':
            exit(-1);
        }
    }

    xdb_make_t xdb(file_name_src, file_name_dst);

    return 0;
}
