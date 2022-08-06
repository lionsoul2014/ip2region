package org.lionsoul.ip2region.xdb;

/**
 * Searcher RuntimeException
 */
public class SearcherException extends RuntimeException {
    public SearcherException() {
    }

    public SearcherException(String message) {
        super(message);
    }

    public SearcherException(String message, Throwable cause) {
        super(message, cause);
    }

    public SearcherException(Throwable cause) {
        super(cause);
    }

    public SearcherException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
