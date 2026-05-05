package ip2region.service

import std.sync.*
import std.core.*
import std.time.*
import std.collection.concurrent.*
import ip2region.xdb.*

// SearcherPool is a thread-safe pool of xdb.Searcher instances.
// Uses Semaphore for backpressure and ConcurrentLinkedQueue for storage.
public class SearcherPool {
    let config: Config
    let queue: ConcurrentLinkedQueue<Searcher>
    let semaphore: Semaphore
    let closing: AtomicBool
    let loanCnt: AtomicInt64

    public init(config: Config) {
        this.config = config
        this.queue = ConcurrentLinkedQueue<Searcher>()
        this.semaphore = Semaphore(config.searchers)
        this.closing = AtomicBool(false)
        this.loanCnt = AtomicInt64(0)

        let poolSize = config.searchers
        for (_ in 0..poolSize) {
            let searcher = createPoolSearcher(config)
            this.queue.add(searcher)
            this.semaphore.release(amount: 1)
        }
    }

    // borrow takes a Searcher from the pool, blocking until one is available.
    // Throws if the pool is closing or closed.
    public func borrow(): Searcher {
        while (!this.closing.load()) {
            if (this.semaphore.tryAcquire(amount: 1)) {
                let opt = this.queue.remove()
                if (opt.isNone()) {
                    this.semaphore.release(amount: 1)
                    continue
                }
                let s = opt.getOrThrow()
                this.loanCnt.fetchAdd(1)
                return s
            }
            sleep(Duration.millisecond * 5)
        }
        throw Exception("SearcherPool is closing")
    }

    // return_ returns a Searcher to the pool, or closes it if the pool is shutting down.
    public func return_(searcher: Searcher) {
        if (this.closing.load()) {
            searcher.close()
        } else {
            this.queue.add(searcher)
            this.semaphore.release(amount: 1)
        }
        this.loanCnt.fetchSub(1)
    }

    // close closes the pool with a default 10 second timeout.
    public func close() {
        this.closeTimeout(Duration.second * 10)
    }

    // closeTimeout closes the pool with the specified timeout.
    public func closeTimeout(timeout: Duration) {
        this.closing.store(true)

        // Drain the queue: acquire all permits and close searchers
        while (true) {
            if (!this.semaphore.tryAcquire(amount: 1)) {
                break
            }
            try {
                let opt = this.queue.remove()
                if (opt.isSome()) {
                    opt.getOrThrow().close()
                }
            } catch (_) {
                break
            }
        }

        // Wait for outstanding loans to return (with timeout)
        let deadline = MonoTime.now() + timeout
        while (this.loanCnt.load() > 0) {
            if (MonoTime.now() >= deadline) {
                break
            }
            sleep(Duration.millisecond * 10)
        }
    }

    public func getLoanCount(): Int64 {
        return this.loanCnt.load()
    }
}

// Helper to create a Searcher based on Config's cache policy
func createPoolSearcher(config: Config): Searcher {
    if (config.cachePolicy == ContentBuff) {
        return Searcher(config.ipVersion, config.cBuffer, config.xdbPath)
    } else if (config.cachePolicy == VectorIndex) {
        return Searcher(config.ipVersion, config.xdbPath, config.vIndex)
    } else {
        return Searcher(config.ipVersion, config.xdbPath)
    }
}
