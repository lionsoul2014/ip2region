
#include "xdb_search.h"

#include <getopt.h>

#include <iostream>

void print_help(int argc, char* argv[]) {
    printf("./xdb_search  [command options]\n");
    printf("options:\n");
    printf(" --db string              ip2region binary xdb file path\n");
    printf(
        " --cache-policy string    cache policy: "
        "file/vector_index/content\n");
    printf(" --help                   print help\n");
    exit(-1);
}

int main(int argc, char* argv[]) {
    struct option long_options[] = {
        {"db",           required_argument, 0, 'd'},
        {"cache-policy", required_argument, 0, 't'},
        {"help",         no_argument,       0, 'h'},
        {0,              0,                 0, 0  }
    };

    std::string db_file_name = "../../data/ip2region.xdb";
    std::string cache_policy = "vector_index";

    while (1) {
        int c = getopt_long(argc, argv, "", long_options, NULL);
        if (c == -1)
            break;
        switch (c) {
        case 'd':
            db_file_name = optarg;
            break;
        case 'h':
            print_help(argc, argv);
            break;
        case 't':
            cache_policy = optarg;
            break;
        case '?':
            exit(-1);
        }
    }

    xdb_search_t xdb(db_file_name);

    if (cache_policy == "content")
        xdb.init_content();
    else if (cache_policy == "vector_index")
        xdb.init_vector_index();
    else if (cache_policy == "file")
        xdb.init_file();
    else {
        std::cout << "invalid cache policy: " << cache_policy << std::endl;
        exit(-1);
    }

    std::string ip;
    for (;;) {
        std::cout << "ip2region>> ";
        std::getline(std::cin, ip);
        if (ip.empty())
            continue;
        if (ip == "exit" || ip == "quit")
            break;
        std::cout << xdb.search(ip) << std::endl;
    }
    return 0;
}
