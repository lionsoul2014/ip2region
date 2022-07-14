// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/07/14

package org.lionsoul.ip2region.xdb;

import java.text.SimpleDateFormat;
import java.util.Date;

// simple log implementation
public class Log {

    /* Log level constants define */
    public static final int DEBUG = 0;
    public static final int INFO = 1;
    public static final int WARN = 2;
    public static final int ERROR = 3;

    // level name
    public static final String[] level_string = new String[] {
        "DEBUG",
        "INFO",
        "WARN",
        "ERROR"
    };

    public final Class<?> baseClass;
    private static int level;

    public Log(Class<?> baseClass) {
        this.baseClass = baseClass;
    }

    public static Log getLogger(Class<?> baseClass) {
        return new Log(baseClass);
    }

    public String format(int level, String format, Object... args) {
        // append the datetime
        final StringBuilder sb = new StringBuilder();
        final SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sb.append(String.format("%s %-5s ", sdf.format(new Date()), level_string[level]));

        // append the class name
        sb.append(baseClass.getName()).append(' ');
        sb.append(String.format(format, args));
        return sb.toString();
    }

    public void printf(int level, String format, Object... args) {
        if (level < DEBUG || level > ERROR) {
            throw new IndexOutOfBoundsException("invalid level index " + level);
        }

        // level filter
        if (level < Log.level) {
            return;
        }

        System.out.println(format(level, format, args));
        System.out.flush();
    }

    public String getDebugf(String format, Object... args) {
        return format(DEBUG, format, args);
    }

    public void debugf(String format, Object... args) {
        printf(DEBUG, format, args);
    }

    public String getInfof(String format, Object... args) {
        return format(INFO, format, args);
    }

    public void infof(String format, Object... args) {
        printf(INFO, format, args);
    }

    public String getWarnf(String format, Object... args) {
        return format(WARN, format, args);
    }

    public void warnf(String format, Object... args) {
        printf(WARN, format, args);
    }

    public String getErrorf(String format, Object... args) {
        return format(ERROR, format, args);
    }

    public void errorf(String format, Object... args) {
        printf(ERROR, format, args);
    }

    public static void setLevel(int level) {
        Log.level = level;
    }

    public static void setLevel(String level) {
        String v = level.toLowerCase();
        if ("debug".equals(v)) {
            Log.level = DEBUG;
        } else if ("info".equals(v)) {
            Log.level = INFO;
        } else if ("warn".equals(v)) {
            Log.level = WARN;
        } else if ("error".equals(v)) {
            Log.level = ERROR;
        }
    }

}