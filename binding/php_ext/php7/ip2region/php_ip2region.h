/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2015 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author:  dongyado<dongyado@gmail.com>                                |
  +----------------------------------------------------------------------+
*/

/* $Id$ */

#ifndef PHP_IP2REGION_H
#define PHP_IP2REGION_H

extern zend_module_entry ip2region_module_entry;
#define phpext_ip2region_ptr &ip2region_module_entry

#define PHP_IP2REGION_VERSION "0.1.0" /* Replace with version number for your extension */

#ifdef PHP_WIN32
#	define PHP_IP2REGION_API __declspec(dllexport)
#elif defined(__GNUC__) && __GNUC__ >= 4
#	define PHP_IP2REGION_API __attribute__ ((visibility("default")))
#else
#	define PHP_IP2REGION_API
#endif

#ifdef ZTS
#include "TSRM.h"
#endif



ZEND_BEGIN_MODULE_GLOBALS(ip2region)
	char *db_file;
ZEND_END_MODULE_GLOBALS(ip2region)

/* In every utility function you add that needs to use variables 
   in php_ip2region_globals, call TSRMLS_FETCH(); after declaring other 
   variables used by that function, or better yet, pass in TSRMLS_CC
   after the last function argument and declare your utility function
   with TSRMLS_DC after the last declared argument.  Always refer to
   the globals in your function as IP2REGION_G(variable).  You are 
   encouraged to rename these macros something shorter, see
   examples in any other php module directory.
*/

#ifdef ZTS
#define IP2REGION_G(v) TSRMG(ip2region_globals_id, zend_ip2region_globals *, v)
#else
#define IP2REGION_G(v) (ip2region_globals.v)
#endif

static void php_ip2region_init_globals(zend_ip2region_globals *);

#define le_ip2region_name "Ip2region"


void search( ip2region_t, uint_t (*func_ptr) (ip2region_t, uint_t, datablock_t), long , zval **, datablock_t);

#endif	/* PHP_IP2REGION_H */
/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * End:
 * vim600: noet sw=4 ts=4 fdm=marker
 * vim<600: noet sw=4 ts=4
 */
