
#include "edit.h"

namespace xdb {

void handle_ip_txt(const string& name, std::list<node_t>& regions) {
    FILE* f = open_file(name, "r");
    if (f == NULL)
        log_exit("can't open " + name);

    char buf[1024];
    while (fgets(buf, sizeof(buf), f) != NULL) {
        size_t buf_len = strlen(buf);
        // clear the spaces
        while (buf_len > 0 && isspace(buf[buf_len - 1]))
            --buf_len;
        if (buf_len == 0)
            continue;
        buf[buf_len] = '\0';
        regions.push_back(node_t(buf));
    }

    fclose(f);
}

void edit_t::handle_new_file(const std::string& file_name) {
    handle_ip_txt(file_name, new_regions); 
    new_regions.sort();                     // sort the segments

    // check and deduplicate
    auto it = new_regions.begin();

    for (;;) {
        if (it == new_regions.end())
            break;
        auto next = it;
        ++next;
        if (next == new_regions.end())
            break;
        if (it->ip1 > it->ip2)
            it = new_regions.erase(it);  // ignore the invalid data
        else if (it->ip1 == next->ip1 || next->ip1 <= it->ip2) {
            if (it->region != next->region)
                log_exit("invalid data: " + it->to_string() + ", " +
                         next->to_string());
            it->ip2 = std::max(it->ip2, next->ip2);
            new_regions.erase(next);
        } else if (it->ip2 + 1 == next->ip1 && it->region == next->region) {
            it->ip2 = next->ip2;
            new_regions.erase(next);
        } else {
            ++it;
        }
    }
}

void edit_t::handle_old_file(const std::string& file_name) {
    handle_ip_txt(file_name, old_regions);
}

void edit_t::merge() {
    auto it1 = old_regions.begin();
    auto it2 = new_regions.begin();

    for (;;) {
        if (it2 == new_regions.end())
            break;
        if (it2->ip1 > it2->ip2) {
            ++it2;
            continue;
        }
        // it1->ip1 it1->ip2 it2->ip1 it2->ip2
        while (it1->ip2 < it2->ip1)
            ++it1;
        if (it1->ip2 <= it2->ip2) {
            // it1->ip1 it2->ip1 it1->ip2 it2->ip2
            node_t node;
            node.ip1    = it2->ip1;
            node.ip2    = it1->ip2;
            node.region = it2->region;

            it1->ip2 = node.ip1 - 1;
            it2->ip1 = node.ip2 + 1;

            ++it1;
            it1 = old_regions.insert(it1, node);
            ++it1;
        } else {
            // it1->ip1 it2->ip1 it2->ip2 it1->ip2
            node_t node;
            node.ip1    = it2->ip2 + 1;
            node.ip2    = it1->ip2;
            node.region = it1->region;

            it1->ip2 = it2->ip1 - 1;

            ++it1;
            it1 = old_regions.insert(it1, *it2);

            ++it1;
            it1 = old_regions.insert(it1, node);

            ++it2;
        }
    }
}

void edit_t::write_old_file(const std::string& file_name) {
    FILE* f = open_file(file_name, "w");
    if (f == NULL)
        log_exit("can't open " + file_name);

    auto it = old_regions.begin();

    // remove the invalid data
    for (;;) {
        if (it == old_regions.end())
            break;
        if (it->ip1 > it->ip2)
            it = old_regions.erase(it);
        else
            ++it;
    }

    // Merge the adjacent segments
    it = old_regions.begin();
    for (;;) {
        if (it == old_regions.end())
            break;
        auto next = it;
        ++next;
        if (next == old_regions.end())
            break;
        if (it->region == next->region) {
            it->ip2 = next->ip2;
            old_regions.erase(next);
        } else {
            ++it;
        }
    }

    for (auto& d : old_regions) {
        string res =
            d.ip1.to_string() + "|" + d.ip2.to_string() + "|" + d.region + "\n";
        fputs(res.data(), f);
    }

    fclose(f);
}

edit_t::edit_t(const string& name_old, const string& name_new, int version) {
    unsigned long long tv1 = get_time();

    init_xdb(version);

    handle_new_file(name_new);
    handle_old_file(name_old);
    merge();
    write_old_file(name_old);

    unsigned long long tv2 = get_time();

    double took = (tv2 - tv1) * 1.0 / 1000 / 1000;

    printf("took: %.2fs\n", took);
}

}  // namespace xdb
