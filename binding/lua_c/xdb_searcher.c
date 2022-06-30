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

#define XDB_METATABLE_NAME "xdb_searcher_mt"

static int lua_xdb_new_with_file_only(lua_State *L) {
    int err;
    xdb_searcher_t *searcher;
    const char *db_path = NULL;

    // check the db path
    db_path = luaL_checkstring(L, 1);
    if (db_path == NULL) {
        return luaL_error(L, "xdb file path expected");
    }

    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_file_only(searcher, db_path);
    if (err != 0) {
        return luaL_error(L, "failed to init xdb searcher on `%s`: errcode=%d", db_path, err);
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);

    return 1;
}

static int lua_xdb_new_with_vector_index(lua_State *L) {
    xdb_searcher_t *searcher;
    const char *v_index;
    const char *db_path;
    int err, top_len;

    top_len = lua_gettop(L);
    luaL_argcheck(L, top_len >= 2, 1, "xdb file path and vector index buffer expected");

    // check the db path and vector index buffer
    db_path = luaL_checkstring(L, 1);
    if (db_path == NULL) {
        return luaL_error(L, "ip2region.xdb file path expected");
    }

    v_index = luaL_checkstring(L, 2);
    if (v_index == NULL) {
        return luaL_error(L, "vector index buffer expected");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_vector_index(searcher, db_path, v_index);
    if (err != 0) {
        return luaL_error(L, "failed to init vector index cached xdb searcher on `%s` with errcode=%d", db_path, err);
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);

    return 1;
}

static int lua_xdb_new_with_buffer(lua_State *L) {
    xdb_searcher_t *searcher;
    const char *c_buffer;
    int err;

    c_buffer = luaL_checkstring(L, 1);
    if (c_buffer == NULL) {
        return luaL_error(L, "xdb content buffer expected");
    }

    // alloc the searcher
    searcher = (xdb_searcher_t *) lua_newuserdata(L, sizeof(xdb_searcher_t));
    if (searcher == NULL) {
        return luaL_error(L, "failed to alloc xdb searcher entry");
    }

    // init the xdb searcher
    err = xdb_new_with_buffer(searcher, c_buffer);
    if (err != 0) {
        return luaL_error(L, "failed to init content cached xdb searcher with errcode=%d", err);
    }

    // push the metatable onto the stack and
    // set it as the metatable of the current searcher
    luaL_getmetatable(L, XDB_METATABLE_NAME);
    lua_setmetatable(L, -2);

    return 1;
}

static int lua_xdb_close(lua_State *L) {
    xdb_searcher_t *searcher;

    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
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

    int top_len = lua_gettop(L);
    luaL_argcheck(L, top_len >= 2, 2, "string or long ip address expected");

    // get the searcher
    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    if (searcher == NULL) {
        return luaL_error(L, "call via ':' or broken xdb searcher instance");
    }

    // input ip type checking
    if (lua_isinteger(L, 2)) {
        ip_int = lua_tointeger(L, 2);
    } else if (lua_isstring(L, 2)) {
        ip_str = lua_tostring(L, 2);
        err = xdb_check_ip(ip_str, &ip_int);
        if (err != 0) {
            luaL_error(L, "invalid string ip `%s`: errcode=%d", ip_str, err);
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

    searcher = (xdb_searcher_t *) luaL_checkudata(L, 1, XDB_METATABLE_NAME);
    if (searcher == NULL) {
        return luaL_error(L, "broken xdb searcher instance");
    }

    lua_pushinteger(L, xdb_get_io_count(searcher));
    return 1;
}

static int lua_xdb_tostring(lua_State *L) {
    lua_pushliteral(L, "xdb searcher object");
    return 1;
}

// -- buffer util function

static int lua_xdb_load_header_from_file(lua_State *L) {
    int err;
    const char *db_path;
    xdb_header_t header;

    db_path = luaL_checkstring(L, 1);
    if (db_path == NULL) {
        return luaL_error(L, "xdb file path expected");
    }

    err = xdb_load_header_from_file(db_path, &header);
    if (err != 0) {
        lua_pushnil(L);
        lua_pushfstring(L, "failed to load header from `%s` with errcode=%d", db_path, err);
        return 2;
    }

    // push the header table onto the stack as return
    lua_newtable(L);
    lua_pushinteger(L, header.version);
    lua_setfield(L, -2, "version");

    lua_pushinteger(L, header.index_policy);
    lua_setfield(L, -2, "index_policy");

    lua_pushinteger(L, header.created_at);
    lua_setfield(L, -2, "created_at");

    lua_pushinteger(L, header.start_index_ptr);
    lua_setfield(L, -2, "start_index_ptr");

    lua_pushinteger(L, header.end_index_ptr);
    lua_setfield(L, -2, "end_index_ptr");

    lua_pushnil(L); // empty error
    return 2;
}

static int lua_xdb_load_vector_index_from_file(lua_State *L) {
    char *v_index;
    const char *db_path;

    db_path = luaL_checkstring(L, 1);
    if (db_path == NULL) {
        return luaL_error(L, "xdb file path expected");
    }

    v_index = xdb_load_vector_index_from_file(db_path);
    if (v_index == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "load xdb vector index from `%s`", db_path);
        return 2;
    }

    // push the vector index onto the stack
    lua_pushstring(L, v_index);
    lua_pushnil(L);

    // free the local vector index allocation.
    // the lua engine will manager the allocation in the VM
    xdb_free(v_index);
    v_index = NULL;

    return 2;
}

static int lua_xdb_load_content_from_file(lua_State *L) {
    char *c_buffer;
    const char *db_path;

    db_path = luaL_checkstring(L, 1);
    if (db_path == NULL) {
        return luaL_error(L, "xdb file path expected");
    }

    c_buffer = xdb_load_content_from_file(db_path);
    if (c_buffer == NULL) {
        lua_pushnil(L);
        lua_pushfstring(L, "load xdb content from `%s`", db_path);
        return 2;
    }

    // push the content buffer onto the stack
    lua_pushstring(L, c_buffer);
    lua_pushnil(L);

    // free the local content buffer allocation.
    // let the lua VM manager it.
    xdb_free(c_buffer);
    c_buffer = NULL;

    return 2;
}

// -- static util function

static int lua_xdb_check_ip(lua_State *L) {
    int err;
    unsigned int ip;
    const char *ip_str;

    ip_str = luaL_checkstring(L, 1);
    if (ip_str == NULL) {
        return luaL_error(L, "ip expected, eg: 1.2.3.4");
    }

    err = xdb_check_ip(ip_str, &ip);
    if (err != 0) {
        lua_pushinteger(L, err);
        lua_pushboolean(L, 0);
    } else {
        lua_pushinteger(L, ip);
        lua_pushboolean(L, 1);
    }

    return 2;
}

static int lua_xdb_long2ip(lua_State *L) {
    unsigned int ip;
    char ip_buff[16] = {'\0'};

    ip = luaL_checkinteger(L, 1);
    xdb_long2ip(ip, ip_buff);
    lua_pushstring(L, ip_buff);

    return 1;
}

static int lua_xdb_now(lua_State *L) {
    lua_pushinteger(L, xdb_now());
    return 1;
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
    {"check_ip",              lua_xdb_check_ip},
    {"long2ip",               lua_xdb_long2ip},
    {"now",                   lua_xdb_now},
    {NULL, NULL}
};

// module register function
int luaopen_xdb_searcher(lua_State *L)
{
    // create a metatable and push it onto the stack
    luaL_newmetatable(L, XDB_METATABLE_NAME);

    // Duplicate the metatable on the stack
    lua_pushvalue(L, -1);

    // Pop the first metatable off the stack
    // and assign it to the __index of the second one.
    // so we set the metatable to the table itself.
    lua_setfield(L, -2, "__index");

    // Set the methods fo the metatable that could and should be
    // access via object:func in lua block
    luaL_setfuncs(L, xdb_searcher_methods, 0);
    luaL_setfuncs(L, xdb_searcher_functions, 0);

    return 1;
}