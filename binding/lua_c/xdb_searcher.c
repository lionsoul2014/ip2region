// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// ---
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/30


#include "stdio.h"
#include "lua.h"
#include "lauxlib.h"
#include "../c/xdb_searcher.h"

#define XDB_SEARCHER_METATABLE_NAME "xdb_searcher_mt"
#define XDB_BUFFER_METATABLE_NAME "xdb_buffer_mt"

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
    {NULL, NULL},
};

// --- End of xdb buffer


// --- buffer util function

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
    buffer->closer = xdb_close_header;

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
    buffer->closer = xdb_close_vector_index;

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
    buffer->closer = xdb_close_content;

    // set the metatable of the header buffer object and push onto the stack
    luaL_getmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

// --- End of buffer api


// --- xdb searcher api

static int lua_xdb_new_with_file_only(lua_State *L) {
    int err;
    xdb_searcher_t *searcher;
    const char *db_path = NULL;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and xdb file path expected");

    // check the db path
    db_path = luaL_checkstring(L, 1);

    // alloc for the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_file_only(searcher, db_path);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init xdb searcher on `%s`: errcode=%d", db_path, err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_SEARCHER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_new_with_vector_index(lua_State *L) {
    xdb_searcher_t *searcher;
    xdb_buffer_t *xBuffer;
    const char *db_path;
    int err;

    luaL_argcheck(L, lua_gettop(L) == 2, 1, "call via '.', xdb file path and vector index buffer expected");

    // check the db path and vector index buffer
    db_path = luaL_checkstring(L, 1);
    xBuffer = luaL_checkudata(L, 2, XDB_BUFFER_METATABLE_NAME);
    if (xBuffer->type != xdb_vector_index_buffer) {
        return luaL_error(L, "invalid vector index buffer");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_vector_index(searcher, db_path, (xdb_vector_index_t *) xBuffer->ptr);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init vector index cached xdb searcher on `%s` with errcode=%d", db_path, err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_SEARCHER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_new_with_buffer(lua_State *L) {
    xdb_searcher_t *searcher;
    xdb_buffer_t *xBuffer;
    int err;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and xdb content buffer expected");
    xBuffer = (xdb_buffer_t *) luaL_checkudata(L, 1, XDB_BUFFER_METATABLE_NAME);
    if (xBuffer->type != xdb_content_buffer) {
        return luaL_error(L, "invalid xdb content buffer");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_buffer(searcher, (xdb_content_t *) xBuffer->ptr);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "init content cached xdb searcher with errcode=%d", err);
        return 2;
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_SEARCHER_METATABLE_NAME);
    lua_setmetatable(L, -2);
    lua_pushnil(L);

    return 2;
}

static int lua_xdb_close(lua_State *L) {
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':'");
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_SEARCHER_METATABLE_NAME);
    if (searcher == NULL) {
        return luaL_error(L, "broken xdb searcher instance");
    }

    xdb_close(searcher);
    return 0;
}

static int lua_xdb_search(lua_State *L) {
    int err;
    const char *ip_str;
    unsigned int ip_int;
    char region_buffer[1024] = {'\0'};
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 2, 2, "call via ':' and string or long ip address expected");

    // get the searcher
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_SEARCHER_METATABLE_NAME);

    // input ip type checking
    if (lua_isinteger(L, 2)) {
        ip_int = lua_tointeger(L, 2);
    } else if (lua_isstring(L, 2)) {
        ip_str = lua_tostring(L, 2);
        err = xdb_check_ip(ip_str, &ip_int);
        if (err != 0) {
            lua_pushnil(L);
            lua_pushfstring(L, "invalid string ip `%s`: errcode=%d", ip_str, err);
            return 2;
        }
    } else {
        return luaL_error(L, "input ip not integer or string");
    }

    // do the search
    err = xdb_search(searcher, ip_int, region_buffer, sizeof(region_buffer));
    if (err != 0) {
        lua_pushinteger(L, err);
        lua_pushfstring(L, "err=%d", err);
    } else {
        lua_pushstring(L, region_buffer);
        lua_pushnil(L);
    }

    return 2;
}

static int lua_xdb_get_io_count(lua_State *L) {
    xdb_searcher_t *searcher;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via ':' or xdb searcher was broken");
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_SEARCHER_METATABLE_NAME);
    lua_pushinteger(L, xdb_get_io_count(searcher));
    return 1;
}

static int lua_xdb_tostring(lua_State *L) {
    lua_pushliteral(L, "xdb searcher object");
    return 1;
}

// -- static util function

static int lua_xdb_check_ip(lua_State *L) {
    int err;
    unsigned int ip;
    const char *ip_str;

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and string ip expected, eg: 1.2.3.4");
    ip_str = luaL_checkstring(L, 1);
    err = xdb_check_ip(ip_str, &ip);
    if (err != 0) {
        lua_pushinteger(L, 0);
        lua_pushfstring(L, "err=%d", err);
    } else {
        lua_pushinteger(L, ip);
        lua_pushnil(L);
    }

    return 2;
}

static int lua_xdb_long2ip(lua_State *L) {
    unsigned int ip;
    char ip_buff[16] = {'\0'};

    luaL_argcheck(L, lua_gettop(L) == 1, 1, "call via '.' and long ip expected");
    ip = luaL_checkinteger(L, 1);
    xdb_long2ip(ip, ip_buff);
    lua_pushstring(L, ip_buff);

    return 1;
}

static int lua_xdb_now(lua_State *L) {
    lua_pushinteger(L, xdb_now());
    return 1;
}

// --- End of static util

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
    {"check_ip",              lua_xdb_check_ip},
    {"long2ip",               lua_xdb_long2ip},
    {"now",                   lua_xdb_now},
    {NULL, NULL}
};

// module register function
int luaopen_xdb_searcher(lua_State *L)
{
    // register the constants
    lua_pushinteger(L, xdb_header_buffer);
    lua_setglobal(L, "header_buffer");
    lua_pushinteger(L, xdb_vector_index_buffer);
    lua_setglobal(L, "v_index_buffer");
    lua_pushinteger(L, xdb_content_buffer);
    lua_setglobal(L, "content_buffer");

    // create a metatable for xdb buffer object
    luaL_newmetatable(L, XDB_BUFFER_METATABLE_NAME);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
    luaL_setfuncs(L, xdb_buffer_methods, 0);

    // create a metatable for xdb searcher object
    luaL_newmetatable(L, XDB_SEARCHER_METATABLE_NAME);
    lua_pushvalue(L, -1);
    lua_setfield(L, -2, "__index");
    luaL_setfuncs(L, xdb_searcher_methods, 0);
    luaL_setfuncs(L, xdb_searcher_functions, 0);

    return 1;
}