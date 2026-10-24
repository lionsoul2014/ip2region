package org.lionsoul.ip2region.service;

public class InvalidConfigException extends Exception {
    private static final long serialVersionUID = 1L;

    public InvalidConfigException(String str) {
        super(str);
    }
}
