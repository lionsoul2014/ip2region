dnl $Id$
dnl config.m4 for extension ip2region

dnl Comments in this file start with the string 'dnl'.
dnl Remove where necessary. This file will not work
dnl without editing.

dnl If your extension references something external, use with:

dnl  PHP_ARG_WITH(ip2region, for ip2region support,
dnl Make sure that the comment is aligned:
dnl [  --with-ip2region             Include ip2region support])

dnl Otherwise use enable:

PHP_ARG_ENABLE(ip2region, whether to enable ip2region support,
Make sure that the comment is aligned:
[  --enable-ip2region           Enable ip2region support])

if test "$PHP_IP2REGION" != "no"; then
dnl   dnl Write more examples of tests here...
dnl 
dnl   dnl # --with-ip2region -> check with-path
dnl    SEARCH_PATH="/home/slayer/code/php-5.6.7/ext/ip2region/lib "     # you might want to change this
dnl    SEARCH_FOR="ip2region.h"  # you most likely want to change this
dnl    if test -r $PHP_IP2REGION/$SEARCH_FOR; then # path given as parameter
dnl      IP2REGION_DIR=$PHP_IP2REGION
dnl    else # search default path list
dnl      AC_MSG_CHECKING([for ip2region files in default path])
dnl      for i in $SEARCH_PATH ; do
dnl        if test -r $i/$SEARCH_FOR; then
dnl          IP2REGION_DIR=$i
dnl          AC_MSG_RESULT(found in $i)
dnl        fi
dnl      done
dnl    fi
dnl   
dnl    if test -z "$IP2REGION_DIR"; then
dnl      AC_MSG_RESULT([not found])
dnl      AC_MSG_ERROR([Please reinstall the ip2region distribution])
dnl    fi
dnl 
dnl    dnl # --with-ip2region -> add include path
dnl    PHP_ADD_INCLUDE($IP2REGION_DIR/include)
dnl 
dnl    dnl # --with-ip2region -> check for lib and symbol presence
dnl    LIBNAME=ip2region # you may want to change this
dnl    LIBSYMBOL=ip2region # you most likely want to change this 
dnl 
dnl    PHP_CHECK_LIBRARY($LIBNAME,$LIBSYMBOL,
dnl    [
dnl      PHP_ADD_LIBRARY_WITH_PATH($LIBNAME, $IP2REGION_DIR/$PHP_LIBDIR, IP2REGION_SHARED_LIBADD)
dnl      AC_DEFINE(HAVE_IP2REGIONLIB,1,[ ])
dnl    ],[
dnl      AC_MSG_ERROR([wrong ip2region lib version or lib not found])
dnl    ],[
dnl      -L$IP2REGION_DIR/$PHP_LIBDIR -lm
dnl    ])
dnl   
dnl    PHP_SUBST(IP2REGION_SHARED_LIBADD)
dnl 
     PHP_NEW_EXTENSION(ip2region, ip2region.c lib/ip2region.c , $ext_shared)
fi
