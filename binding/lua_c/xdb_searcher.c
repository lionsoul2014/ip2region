// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/30


#include "stdio.h"
#include "lua.h"
#include "lauxlib.h"
#include "../c/xdb_api.h"

#define XDB_BUFFER_METATABLE_NAME "xdb_buffer_mt"
#define XDB_METATABLE_NAME "xdb_metatable_name"

#define xdb_header_buffer 1
#define xdb_vector_index_buffer 2
#define xdb_content_buffer 3


// --- xdb buffer interface impl

struct xdb_buffer_entry {
    int type;   // buffer type
    char *name; // buffer name
    void *ptr;  // buffer ptr
    void (*closer) (void *);
};
typedef struct xdb_buffer_entry xdb_buffer_t;

static int lua_xdb_buffer_name(lua_State *L) {
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    lua_pushstring(L, buffer->name);
    return 1;
}

static int lua_xdb_buffer_type(lua_State *L) {
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    lua_pushinteger(L, buffer->type);
    return 1;
}

static int lua_xdb_buffer_to_table(lua_State *L) {
    xdb_buffer_t *buffer;
    xdb_header_t *header;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    lua_newtable(L);
    if (buffer->type == xdb_header_buffer) {
        header = (xdb_header_t *) buffer->ptr;
        lua_pushinteger(L, header->version);
        lua_setfield(L, -2, "version");

        lua_pushinteger(L, header->index_policy);
        lua_setfield(L, -2, "index_policy");

        lua_pushinteger(L, header->created_at);
        lua_setfield(L, -2, "created_at");

        lua_pushinteger(L, header->start_index_ptr);
        lua_setfield(L, -2, "start_index_ptr");

        lua_pushinteger(L, header->end_index_ptr);
        lua_setfield(L, -2, "end_index_ptr");

        lua_pushinteger(L, header->ip_version);
        lua_setfield(L, -2, "ip_version");

        lua_pushinteger(L, header->runtime_ptr_bytes);
        lua_setfield(L, -2, "runtime_ptr_bytes");
    } else {
        // do nothing for now
    }

    return 1;
}

static int lua_xdb_buffer_length(lua_State *L) {
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    if (buffer->type == xdb_header_buffer) {
        lua_pushinteger(L, ((xdb_header_t *) buffer->ptr)->length);
    } else if (buffer->type == xdb_vector_index_buffer) {
        lua_pushinteger(L, ((xdb_vector_index_t *) buffer->ptr)->length);
    } else if (buffer->type == xdb_content_buffer) {
        lua_pushinteger(L, ((xdb_content_t *) buffer->ptr)->length);
    } else {
        lua_pushinteger(L, -1);
    }

    return 1;
}

static int lua_xdb_buffer_tostring(lua_State *L) {
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    lua_pushfstring(L, "xdb %s buffer object {name: %s, type: %d}", buffer->name, buffer->name, buffer->type);
    return 1;
}

static int lua_xdb_buffer_close(lua_State *L) {
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    buffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);

    // check and call the closer
    if (buffer->closer != NULL) {
        buffer->closer(buffer->ptr);
        buffer->closer = NULL;
    }

    return 0;
}

// module method define, should be access via ':'
static const struct luaL_Reg xdb_buffer_methods[] = {
    {"name",        lua_xdb_buffer_name},
    {"type",        lua_xdb_buffer_type},
    {"length",      lua_xdb_buffer_length},
    {"to_table",    lua_xdb_buffer_to_table},
    {"close",       lua_xdb_buffer_close},
    {"__gc",        lua_xdb_buffer_close},
    {"__tostring",  lua_xdb_buffer_tostring},
    {NULL, NULL}
};

// --- End of xdb buffer


// --- xdb util function

static int lua_xdb_load_header_from_file(lua_State *L) {
    const char *db_path;
    xdb_header_t *header;
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and the xdb file path expected");
    db_path = luaL_checkstring(L, 1);
    header = xdb_load_header_from_file(db_path);
    if (header == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "load header from `%s`", db_path);
        return 2;
    }

    // alloc the buffer.
    buffer = (xdb_buffer_t *) lua_newuserdata(L, sizeof(xdb_buffer_t));
    if (buffer == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "failed to alloc xdb buffer entry");
        return 2;
    }

    // init the buffer
    buffer->type = xdb_header_buffer;
    buffer->name = "header";
    buffer->ptr = header;
    buffer->closer = xdb_free_header;

    // set the metatable of the header buffer object and push onto the stack
    luaL_getmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_load_vector_index_from_file(lua_State *L) {
    const char *db_path;
    xdb_vector_index_t *v_index;
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and the xdb path expected");
    db_path = luaL_checkstring(L, 1);
    v_index = xdb_load_vector_index_from_file(db_path);
    if (v_index == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "load vector index from `%s`", db_path);
        return 2;
    }

    // alloc the buffer.
    buffer = (xdb_buffer_t *) lua_newuserdata(L, sizeof(xdb_buffer_t));
    if (buffer == NULL) {
        lua_pushnil(L);
        lua_pushstring(L, "failed to alloc xdb buffer entry");
        return 2;
    }

    // init the buffer
    buffer->type = xdb_vector_index_buffer;
    buffer->name = "v_index";
    buffer->ptr = v_index;
    buffer->closer = xdb_free_vector_index;

    // set the metatable of the header buffer object and push onto the stack
    luaL_getmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_load_content_from_file(lua_State *L) {
    const char *db_path;
    xdb_content_t *content;
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and xdb path expected");
    db_path = luaL_checkstring(L, 1);
    content = xdb_load_content_from_file(db_path);
    if (content == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "load xdb content from `%s`", db_path);
        return 2;
    }

    // alloc the buffer.
    buffer = (xdb_buffer_t *) lua_newuserdata(L, sizeof(xdb_buffer_t));
    if (buffer == NULL) {
        lua_pushnil(L);
        lua_pushstring(L, "failed to alloc xdb buffer entry");
        return 2;
    }

    // init the buffer
    buffer->type = xdb_content_buffer;
    buffer->name = "content";
    buffer->ptr = content;
    buffer->closer = xdb_free_content;

    // set the metatable of the header buffer object and push onto the stack
    luaL_getmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_verify_from_file(lua_State *L) {
    const char *db_path;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and the xdb file path expected");
    db_path = luaL_checkstring(L, 1);
    lua_pushboolean(L, xdb_verify_from_file(db_path) == 0 ? 1 : 0);
    return 1;
}

static int lua_xdb_version_from_header(lua_State *L) {
    xdb_version_t *version;
    xdb_buffer_t *buffer;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and xdb header expected");
    // header buffer checking
    buffer = luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);
    if (buffer->type != xdb_header_buffer) {
        return luaL_error(L, "invalid xdb header buffer");
    }

    version = xdb_version_from_header((xdb_header_t *) buffer->ptr);
    if (version == NULL) {
        lua_pushnil(L);
        lua_pushstring(L, "failed to detect version from header");
    } else {
        lua_pushinteger(L, version->id);
        lua_pushnil(L);
    }

    return 2;
}

static xdb_version_t *_get_version(lua_State *L, int arg) {
    int vid = luaL_checkinteger(L, arg);
    if (vid == xdb_ipv4_id) {
        return XDB_IPv4;
    } else if (vid == xdb_ipv6_id) {
        return XDB_IPv6;
    } else {
        return NULL;
    }
}

static int lua_xdb_version_info(lua_State *L) {
    xdb_version_t *version;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and version id expected");
    // check the ip version
    version = _get_version(L, 1);
    if (version == NULL) {
        return luaL_error(L, "invalid verison id specified");
    }

    lua_newtable(L);
    lua_pushinteger(L, version->id);
    lua_setfield(L, -2, "id");

    lua_pushstring(L, version->name);
    lua_setfield(L, -2, "name");

    lua_pushinteger(L, version->bytes);
    lua_setfield(L, -2, "bytes");

    lua_pushinteger(L, version->segment_index_size);
    lua_setfield(L, -2, "segment_index_size");
    return 1;
}

static int lua_xdb_parse_ip(lua_State *L) {
    const char *ip_str;
    bytes_ip_t ip_bytes[19] = {'\0'};
    xdb_version_t *version;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and string ip expected, eg: 1.2.3.4 / 3000::");
    ip_str = luaL_checkstring(L, 1);

    version = xdb_parse_ip(ip_str, ip_bytes + 2, sizeof(ip_bytes) - 2);
    if (version == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "failed to parse the `%s`", ip_str);
        return 2;
    }

    // append the magic char for later analysis
    // printf("ip:%s, version->id: %d\n", ip_str, version->id);
    ip_bytes[0] = '&';
    ip_bytes[1] = (bytes_ip_t) version->id;
    lua_pushlstring(L, (string_ip_t *) ip_bytes, version->bytes + 2);
    lua_pushnil(L);
    return 2;
}

static int lua_xdb_ip_to_string(lua_State *L) {
    int err, vid, bytes;
    const string_ip_t *ip_bytes;
    char ip_string[INET6_ADDRSTRLEN + 1] = {'\0'};

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and bytes ip expected");
    ip_bytes = luaL_checkstring(L, 1);
    if (strlen(ip_bytes) < 2) {
        lua_pushnil(L);
        lua_pushstring(L, "invalid binary ip bytes specified");
        return 2;
    }

    if (ip_bytes[0] != '&') {
        lua_pushnil(L);
        lua_pushstring(L, "invalid binary ip bytes specified");
        return 2;
    }

    vid = ip_bytes[1] & 0xFF;
    if (vid == xdb_ipv4_id) {
        // IPv4
        bytes = xdb_ipv4_bytes;
    } else if (vid == xdb_ipv6_id) {
        // IPv6
        bytes = xdb_ipv6_bytes;
    } else {
        lua_pushnil(L);
        lua_pushstring(L, "invalid binary ip bytes specified");
        return 2;
    }

    err = xdb_ip_to_string(((bytes_ip_t *) ip_bytes) + 2, bytes, ip_string, sizeof(ip_string));
    if (err != 0) {
        lua_pushnil(L);
        lua_pushstring(L, "failed to conver the ip bytes to string");
        return 2;
    }

    lua_pushstring(L, ip_string);
    lua_pushnil(L);
    return 2;
}

static int _validate_bytes_ip(const string_ip_t *ip_bytes) {
    if (strlen(ip_bytes) < 2) {
        return 1;
    }

    if (ip_bytes[0] != '&') {
        return 2;
    }

    int vid = ip_bytes[1] & 0xFF;
    if (vid != xdb_ipv4_id && vid != xdb_ipv6_id) {
        return 3;
    }

    return 0;
}

static int lua_xdb_ip_compare(lua_State *L) {
    int err;
    const string_ip_t *ip1_bytes, *ip2_bytes;

    luaL_argcheck(L, lua_gettop(L) == 2, 1, "call via '.' bytes ip1 and ip2 expected");
    ip1_bytes = luaL_checkstring(L, 1);
    ip2_bytes = luaL_checkstring(L, 2);

    // validate the ip1
    err = _validate_bytes_ip(ip1_bytes);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "failed to validate ip1 with errcode=%d", err);
        return 2;
    }

    // validate the ip2
    err = _validate_bytes_ip(ip2_bytes);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "failed to validate ip2 with errcode=%d", err);
        return 2;
    }

    if (ip1_bytes[1] != ip2_bytes[1]) {
        lua_pushnil(L);
        lua_pushstring(L, "ip version of ip1 and ip2 are not the same");
        return 2;
    }

    err = xdb_ip_sub_compare(((bytes_ip_t *)ip1_bytes) + 2, (ip1_bytes[1] & 0xFF), ip2_bytes, 2);
    lua_pushinteger(L, err);
    lua_pushnil(L);
    return 2;
}

static int lua_xdb_now(lua_State *L) {
    lua_pushinteger(L, xdb_now());
    return 1;
}


// --- End of xdb util api


// --- xdb searcher api

static int lua_xdb_new_with_file_only(lua_State *L) {
    int err;
    xdb_version_t *version;
    xdb_searcher_t *searcher;
    const char *db_path = NULL;

    luaL_argcheck(L, lua_gettop(L) == 2, 1, "call via '.' and ip version / xdb file path expected");

    // check the ip version
    version = _get_version(L, 1);
    if (version == NULL) {
        return luaL_error(L, "invalid verison id specified");
    }

    // check the db path
    db_path = luaL_checkstring(L, 2);

    // alloc for the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_file_only(version, searcher, db_path);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init xdb searcher on `%s`: errcode=%d", db_path, err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_new_with_vector_index(lua_State *L) {
    xdb_version_t *version;
    xdb_searcher_t *searcher;
    xdb_buffer_t *xBuffer;
    const char *db_path;
    int err;

    luaL_argcheck(L, lua_gettop(L) == 3, 1, "call via '.', ip version / xdb file path / vector index buffer expected");

    // check the ip version
    version = _get_version(L, 1);
    if (version == NULL) {
        return luaL_error(L, "invalid verison id specified");
    }

    // db_path checking
    db_path = luaL_checkstring(L, 2);

    // vector index buffer checking
    xBuffer = luaL_checkudata(L, 3, XDB_BUFFER_METATABLE_NAME);
    if (xBuffer->type != xdb_vector_index_buffer) {
        return luaL_error(L, "invalid vector index buffer");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_vector_index(version, searcher, db_path, (xdb_vector_index_t *) xBuffer->ptr);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init vector index cached xdb searcher on `%s` with errcode=%d", db_path, err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_new_with_buffer(lua_State *L) {
    xdb_version_t *version;
    xdb_searcher_t *searcher;
    xdb_buffer_t *xBuffer;
    int err;

    luaL_argcheck(L, lua_gettop(L) == 2, 1, "call via '.' and ip version / xdb content buffer expected");

    // check the ip version
    version = _get_version(L, 1);
    if (version == NULL) {
        return luaL_error(L, "invalid verison id specified");
    }

    // content buffer checking
    xBuffer = (xdb_buffer_t *) luaL_checkudata(L, 2, XDB_BUFFER_METATABLE_NAME);
    if (xBuffer->type != xdb_content_buffer) {
        return luaL_error(L, "invalid xdb content buffer");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_buffer(version, searcher, (xdb_content_t *) xBuffer->ptr);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init content cached xdb searcher with errcode=%d", err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_close(lua_State *L) {
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    if (searcher == NULL) {
        return luaL_error(L, "broken xdb searcher instance");
    }

    xdb_close(searcher);
    return 0;
}

static int lua_xdb_search(lua_State *L) {
    int err, vid, ip_len;
    const char *ip_string;
    bytes_ip_t ip_buffer[INET6_ADDRSTRLEN] = {'\0'};
    const bytes_ip_t *ip_bytes;

    xdb_version_t *version;
    xdb_region_buffer_t region;
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 2, 2, "call via ':' and string ip address expected");

    // get the searcher
    searcher  = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    ip_string = luaL_checkstring(L, 2);

    // ip string type checking
    if (strlen(ip_string) < 2) {
        lua_pushnil(L);
        lua_pushfstring(L, "invalid ip address `%s`", ip_string);
        return 2;
    }

    if (ip_string[0] == '&') {
        vid = ip_string[1] & 0xFF;
        if (vid == xdb_ipv4_id) {
            ip_len = xdb_ipv4_bytes;
        } else if (vid == xdb_ipv6_id) {
            ip_len = xdb_ipv6_bytes;
        } else {
            lua_pushnil(L);
            lua_pushstring(L, "invalid binary ip bytes specified");
            return 2;
        }

        ip_bytes = (bytes_ip_t *)ip_string + 2;
        // printf("ip_len: %d, vid: %d\n", ip_len, vid);
    } else {
        version = xdb_parse_ip(ip_string, ip_buffer, sizeof(ip_buffer));
        if (version == NULL) {
            lua_pushnil(L);
            lua_pushfstring(L, "failed to parse string ip `%s`", ip_string);
            return 2;
        }

        ip_len   = version->bytes;
        ip_bytes = ip_buffer;
    }

    // init the region buffer
    err = xdb_region_buffer_init(&region, NULL, 0);
    if (err != 0) {
        return luaL_error(L, "failed to init the region buffer with errcode=%d", err);
    }

    // do the search
    err = xdb_search(searcher, ip_bytes, ip_len, &region);
    if (err != 0) {
        lua_pushinteger(L, err);
        lua_pushfstring(L, "err=%d", err);
    } else {
        lua_pushstring(L, region.value);
        lua_pushnil(L);
    }

    // clean up the region buffer
    xdb_region_buffer_free(&region);
    return 2;
}

static int lua_xdb_get_io_count(lua_State *L) {
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':' or xdb searcher was broken");
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    lua_pushinteger(L, xdb_get_io_count(searcher));
    return 1;
}

static int lua_xdb_tostring(lua_State *L) {
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':' or xdb searcher was broken");
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    lua_pushfstring(L, "xdb %s searcher object", xdb_get_version(searcher)->name);
    return 1;
}

// cleanup the current module
static int lua_xdb_cleanup(lua_State *L) {
     xdb_clean_winsock();
    return 0;
}

// module method define, should be access via ':'
static const struct luaL_Reg xdb_searcher_methods[] = {
    {"search",      lua_xdb_search},
    {"get_io_count",lua_xdb_get_io_count},
    {"close",       lua_xdb_close},
    {"__gc",        lua_xdb_close},
    {"__tostring",  lua_xdb_tostring},
    {NULL, NULL},
};

// module function define, should be access via '.'
static const struct luaL_Reg xdb_searcher_functions[] = {
    {"new_with_file_only",    lua_xdb_new_with_file_only},
    {"new_with_vector_index", lua_xdb_new_with_vector_index},
    {"new_with_buffer",       lua_xdb_new_with_buffer},
    {"load_header",           lua_xdb_load_header_from_file},
    {"load_vector_index",     lua_xdb_load_vector_index_from_file},
    {"load_content",          lua_xdb_load_content_from_file},
    {"verify",                lua_xdb_verify_from_file},
    {"version_from_header",   lua_xdb_version_from_header},
    {"version_info",          lua_xdb_version_info},
    {"cleanup",               lua_xdb_cleanup},
    {"parse_ip",              lua_xdb_parse_ip},
    {"ip_to_string",          lua_xdb_ip_to_string},
    {"ip_compare",            lua_xdb_ip_compare},
    {"now",                   lua_xdb_now},
    {NULL, NULL}
};

// module register function
int luaopen_xdb_searcher(lua_State *L)
{
    int err = xdb_init_winsock();
    if (err != 0) {
        luaL_error(L, "failed to init the winsock with errno=%d\n", err);
        return 1;
    }
    
    // create a metatable for xdb buffer object
    luaL_newmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
#if defined(LUA_VERSION_NUM) && LUA_VERSION_NUM == 501
    // lua 5.1
    luaL_register(L, NULL, xdb_buffer_methods);
#elif defined(LUA_VERSION_NUM) && LUA_VERSION_NUM >= 502
    // lua version 5.2, 5.3, 5.4 ...
    luaL_setfuncs(L, xdb_buffer_methods, 0);
#endif

    // create a metatable for xdb searcher object
    luaL_newmetatable(L, XDB_METATABLE_NAME);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
#if defined(LUA_VERSION_NUM) && LUA_VERSION_NUM == 501
    // lua 5.1
    luaL_register(L, NULL, xdb_searcher_methods);
    luaL_register(L, NULL, xdb_searcher_functions);
#elif defined(LUA_VERSION_NUM) && LUA_VERSION_NUM >= 502
    // lua version 5.2, 5.3, 5.4 ...
    luaL_setfuncs(L, xdb_searcher_methods, 0);
    luaL_setfuncs(L, xdb_searcher_functions, 0);
#endif

    // register the constants attributes
    lua_pushinteger(L, xdb_ipv4_id);
    lua_setfield(L, -2, "IPv4");
    lua_pushinteger(L, xdb_ipv6_id);
    lua_setfield(L, -2, "IPv6");
    lua_pushinteger(L, xdb_header_buffer);
    lua_setfield(L, -2, "header_buffer");
    lua_pushinteger(L, xdb_vector_index_buffer);
    lua_setfield(L, -2, "v_index_buffer");
    lua_pushinteger(L, xdb_content_buffer);
    lua_setfield(L, -2, "content_buffer");

    return 1;
}