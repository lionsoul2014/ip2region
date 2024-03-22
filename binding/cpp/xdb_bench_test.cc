
#include "xdb_bench.h"

#include <getopt.h>

#include <iostream>

void print_help(int argc, char* argv[]) {
    printf("./xdb_bench [command options]\n");
    printf("options:\n");
    printf(" --db string              ip2region binary xdb file path\n");
    printf(" --src string             source ip text file path\n");
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
        {"src",          required_argument, 0, 's'},
        {"help",         no_argument,       0, 'h'},
        {0,              0,                 0, 0  }
    };

    std::string db_file_name  = "../../data/ip2region.xdb";
    std::string src_file_name = "../../data/ip.merge.txt";
    std::string cache_policy  = "vector_index";

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
        case 's':
            src_file_name = optarg;
            break;
        case '?':
            exit(-1);
        }
    }

    xdb_bench_t xdb(db_file_name);

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

    xdb.bench(src_file_name);
    return 0;
}
