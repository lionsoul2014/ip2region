/**
 * ip2region header file
 *
 * @author	chenxin<chenxin619315@gmail.com>
*/

#ifndef _IP2REGION_H
#define _IP2REGION_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//yat, just take it as this way, 99 percent and they works find
#if ( defined(_WIN32) || defined(_WINDOWS_) || defined(__WINDOWS_) )
#	define OS_WINDOW
#else
#	define OS_LINUX
#endif

#ifdef OS_WINDOW
#	define IP2R_API extern __declspec(dllexport)
#else
/*platform shared library statement :: unix*/
#	define IP2R_API extern
#endif

#define print(str) printf("%s", str)
#define println(str) printf("%s\n", str)

/*
 * memory allocation macro definition.
 * 		cause we should use emalloc,ecalloc .ege. in php.
 * so you could make it better apdat the php environment.
 */
#define IP2R_CALLOC(_bytes, _blocks) calloc(_bytes, _blocks)
#define IP2R_MALLOC(_bytes) malloc(_bytes)
#define IP2R_FREE(_ptr) free(_ptr)

typedef unsigned short ushort_t;
typedef unsigned char uchar_t;
typedef unsigned int uint_t;
typedef unsigned long ulong_t;

#define INDEX_BLOCK_LENGTH 12
#define TOTAL_HEADER_LENGTH 4096

/*
 * ip2region properties struct
*/
typedef struct {
	uint_t *HeaderSip;		//header start ip blocks
	uint_t *HeaderPtr;		//header ptr blocks
	uint_t headerLen;		//header block number
    char *dbFile;           //path of db file
	FILE *dbHandler;		//file handler
    char *dbBinStr;         //db binary string for memory search mode

	uint_t firstIndexPtr;	//first index ptr
	uint_t lastIndexPtr;	//last index ptr
	uint_t totalBlocks;		//total index blocks number
} ip2region_entry;
typedef ip2region_entry * ip2region_t;

/*
 * data block
*/
typedef struct {
	uint_t city_id;
	char region[256];
} datablock_entry;
typedef datablock_entry * datablock_t;


/**
 * create a new ip2region object
 *
 * @param	ip2rObj
 * @param	dbFile path
*/
IP2R_API uint_t ip2region_create(ip2region_t, char *);

/**
 * destroy the specified ip2region object
 *
 * @param	ip2region_t
*/
IP2R_API uint_t ip2region_destroy(ip2region_t);

/**
 * get the region associated with the specified ip address with the memory binary search algorithm
 *
 * @param   ip2region_t
 * @param   uint_t
 * @param   datablock_t
 * @date    2016/06/30
*/
IP2R_API uint_t ip2region_memory_search(ip2region_t, uint_t, datablock_t);
IP2R_API uint_t ip2region_memory_search_string(ip2region_t, char *, datablock_t);


/**
 * get the region associated with the specified ip address with binary search algorithm
 *
 * @param	ip2rObj
 * @param	ip
 * @param	datablock
 * @return	uint_t
*/
IP2R_API uint_t ip2region_binary_search(ip2region_t, uint_t, datablock_t);
IP2R_API uint_t ip2region_binary_search_string(ip2region_t, char *, datablock_t);

/**
 * get the region associated with the specified ip address with b-tree algorithm
 *
 * @param	ip2rObj
 * @param	ip
 * @param	datablock
 * @return	uint_t
*/
IP2R_API uint_t ip2region_btree_search(ip2region_t, uint_t, datablock_t);
IP2R_API uint_t ip2region_btree_search_string(ip2region_t, char *, datablock_t);

/**
 * get a unsinged long(4bytes) from a specified buffer start from the specified offset
 *
 * @param	buffer
 * @param	offset
 * @return	uint_t
*/
IP2R_API uint_t getUnsignedInt(char *, int);

/**
 * string ip to long
 *
 * @param	ip
 * @return	uint_t
*/
IP2R_API uint_t ip2long(char *);

/**
 * long to string ip
 *
 * @param	ip
 * @param	buffer
 * @return	uint_t(1 for success and 0 for failed)
*/
IP2R_API uint_t long2ip(uint_t, char *);

#endif	/*end ifndef*/
