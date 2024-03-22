
#include "xdb_edit.h"

#include <getopt.h>
#include <stdio.h>

#include <iostream>

void print_help() {
    printf("./xdb_edit [command options]\n");
    printf("options:\n");
    printf(" --old filename       old source ip text file path\n");
    printf(" --new filename       new source ip text file path\n");
    exit(-1);
}

int main(int argc, char* argv[]) {
    struct option long_options[] = {
        {"new",  required_argument, 0, 'n'},
        {"old",  required_argument, 0, 'o'},
        {"help", no_argument,       0, 'h'},
        {0,      0,                 0, 0  }
    };

    std::string file_name_old = "../../data/ip.merge.txt";
    std::string file_name_new = "./1.txt";

    while (1) {
        int c = getopt_long(argc, argv, "", long_options, NULL);
        if (c == -1)
            break;
        switch (c) {
        case 'n':
            file_name_new = optarg;
            break;
        case 'h':
            print_help();
            break;
        case 'o':
            file_name_old = optarg;
            break;
        case '?':
            exit(-1);
        }
    }

    xdb_edit_t xdb(file_name_old, file_name_new);

    return 0;
}
