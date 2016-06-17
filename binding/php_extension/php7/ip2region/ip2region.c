/*
  +----------------------------------------------------------------------+
  | PHP Version 7                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2016 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author: dongyado<dongyado@gmail.com>                                 |
  +----------------------------------------------------------------------+
*/

/* $Id$ */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "lib/ip2region.h"
#include "php_ip2region.h"
#include <sys/time.h>

ZEND_DECLARE_MODULE_GLOBALS(ip2region)

/* True global resources - no need for thread safety here */
static int le_ip2region;
static ip2region_t      g_resource_ptr;
static ip2region_entry  g_resource;

// class
static zend_class_entry *ip2region_class_entry_ptr;


//static double getTime()
//{
//	struct timeval tv;
//	struct timezone tz;
//	gettimeofday(&tv, &tz);
//
//	return (tv.tv_sec * 1000 + ((double)tv.tv_usec)/1000);
//}


void search(
		ip2region_t g_resouce_ptr, 
		uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t), 
		long ip, 
		zval ** return_value, 
		datablock_t _block)
{
	int res = 0;

	if (g_resource_ptr != NULL){
		res 	= (*func_ptr)(g_resouce_ptr, (uint_t) ip, _block);
	}

	array_init( *return_value );

	if ( res == 1 )
	{
		add_assoc_long( *return_value,   "cityId", (*_block).city_id);
		//add_assoc_string( *return_value, "region", (*_block).region, 1);
		// for phpng
		add_assoc_string( *return_value, "region", (*_block).region);
	} else {
		add_assoc_long( *return_value,   "cityId", 0);
		//add_assoc_string( *return_value, "region", "[Error] Search Failed! Please check the  path of ip2region db file.", 1);
		// for phpng
		add_assoc_string( *return_value, "region", "[Error] Search Failed! Please check the  path of ip2region db file.");
	}
}


/* {{{ php_ip2region_init_globals
 */
static void php_ip2region_init_globals(zend_ip2region_globals *ip2region_globals)
{
	ip2region_globals->db_file = NULL;
}
/* }}} */


/* {{{ PHP_INI
 */
PHP_INI_BEGIN()
    STD_PHP_INI_ENTRY("ip2region.db_file", NULL, PHP_INI_ALL, OnUpdateString, db_file, zend_ip2region_globals, ip2region_globals)
PHP_INI_END()
/* }}} */


/* {{{ 
 *  btree search string
 * */
PHP_METHOD(ip2region_class_entry_ptr, btreeSearchString)
{
	char *ip = NULL;
	size_t arg_len;
	datablock_entry  _block;
	uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t);

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "s", &ip, &arg_len) == FAILURE) {
		return;
	}

	func_ptr = ip2region_btree_search;

	search(g_resource_ptr, func_ptr, ip2long(ip), &return_value, &_block);
}
/* }}} */



/* {{{ 
 *  btree search 
 * */
PHP_METHOD(ip2region_class_entry_ptr, btreeSearch)
{
	long ip;
	datablock_entry  _block;
	uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t);

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "l", &ip) == FAILURE) {
		return;
	}

	func_ptr = ip2region_btree_search;

	search(g_resource_ptr, func_ptr, ip, &return_value, &_block);
}
/* }}} */


/* {{{
 * binary search string
 * */
PHP_METHOD(ip2region_class_entry_ptr,  binarySearchString)
{
	char *ip = NULL;
	//int arg_len;
	// for phpng
	size_t arg_len;
	datablock_entry  _block;
	uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t);

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "s", &ip, &arg_len) == FAILURE) {
		return;
	}

	func_ptr = ip2region_binary_search;

	search(g_resource_ptr, func_ptr, ip2long(ip), &return_value, &_block);
}
/* }}} */


/* {{{
 * binary search 
 * */
PHP_METHOD(ip2region_class_entry_ptr,  binarySearch)
{
	long ip;
	datablock_entry  _block;
	uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t);

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "l", &ip) == FAILURE) {
		return;
	}

	func_ptr = ip2region_binary_search;
	search(g_resource_ptr, func_ptr, ip, &return_value, &_block);
}
/* }}} */




/* {{{ PHP_MSHUTDOWN_FUNCTION
 */
PHP_MSHUTDOWN_FUNCTION(ip2region)
{
	UNREGISTER_INI_ENTRIES();
	ip2region_destroy(&g_resource);
	return SUCCESS;
}
/* }}} */


/* Remove if there's nothing to do at request start */
/* {{{ PHP_RINIT_FUNCTION
 */
PHP_RINIT_FUNCTION(ip2region)
{
	return SUCCESS;
}
/* }}} */


/* Remove if there's nothing to do at request end */
/* {{{ PHP_RSHUTDOWN_FUNCTION
 */
PHP_RSHUTDOWN_FUNCTION(ip2region)
{
	return SUCCESS;
}
/* }}} */


/* {{{ PHP_MINFO_FUNCTION
 */
PHP_MINFO_FUNCTION(ip2region)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "ip2region support", "enabled");
	php_info_print_table_end();

	DISPLAY_INI_ENTRIES();
}
/* }}} */


/* {{{ ip2region_class_functions[]
 *
 * Every user visible function must have an entry in ip2region_functions[].
 */
const zend_function_entry ip2region_class_functions[] = {
	PHP_ME( ip2region_class_entry_ptr,  btreeSearchString,  NULL, ZEND_ACC_PUBLIC | ZEND_ACC_STATIC)
	PHP_ME( ip2region_class_entry_ptr,  binarySearchString, NULL, ZEND_ACC_PUBLIC | ZEND_ACC_STATIC )
	PHP_ME( ip2region_class_entry_ptr,  btreeSearch,  NULL, ZEND_ACC_PUBLIC | ZEND_ACC_STATIC)
	PHP_ME( ip2region_class_entry_ptr,  binarySearch, NULL, ZEND_ACC_PUBLIC | ZEND_ACC_STATIC )
	PHP_FE_END	/* Must be the last line in ip2region_functions[] */
};
/* }}} */


/* {{{ ip2region_functions[]
 *
 * Every user visible function must have an entry in ip2region_functions[].
 */
const zend_function_entry ip2region_functions[] = {
	PHP_FE_END	/* Must be the last line in ip2region_functions[] */
};
/* }}} */


/* {{{ PHP_MINIT_FUNCTION
 */
PHP_MINIT_FUNCTION(ip2region)
{
	zend_class_entry ip2region_class_entry;

	// @NOTE the line should before REGISTRE_INI_ENTRIES, 
	// and should not comment it when you have glocbals 
	ZEND_INIT_MODULE_GLOBALS(ip2region, php_ip2region_init_globals, NULL);  
	REGISTER_INI_ENTRIES();


	if (ip2region_create( &g_resource, IP2REGION_G(db_file)) == 0)
	{
		g_resource_ptr = NULL;
	} else {
		g_resource_ptr = &g_resource;
	}


	//le_ip2region = zend_register_list_destructors_ex(
	//	ip2region_destruction_handler, NULL, le_ip2region_name, module_number);

	// init class
	INIT_CLASS_ENTRY( ip2region_class_entry, "ip2region", ip2region_class_functions);
	ip2region_class_entry_ptr = 
		zend_register_internal_class(&ip2region_class_entry TSRMLS_CC);

	return SUCCESS;
}
/* }}} */


/* {{{ ip2region_module_entry
 */
zend_module_entry ip2region_module_entry = {
	STANDARD_MODULE_HEADER,
	"ip2region",
	ip2region_functions,
	PHP_MINIT(ip2region),
	PHP_MSHUTDOWN(ip2region),
	PHP_RINIT(ip2region),		
	PHP_RSHUTDOWN(ip2region),
	PHP_MINFO(ip2region),
	PHP_IP2REGION_VERSION,
	STANDARD_MODULE_PROPERTIES
};
/* }}} */


#ifdef COMPILE_DL_IP2REGION
ZEND_GET_MODULE(ip2region)
#endif

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 * vim600: noet sw=4 ts=4 fdm=marker
 * vim<600: noet sw=4 ts=4
 */
