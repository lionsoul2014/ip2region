package org.lionsoul.ip2region.example;

import org.lionsoul.ip2region.xdb.Searcher;

import java.util.concurrent.TimeUnit;

/**
 * @see org.lionsoul.ip2region.xdb.Searcher
 */
public class SearcherExample {

    public static void main(String[] args) throws Exception {
        String dbPath = "ip2region.xdb file path";

        // 基于文件查询，单次 search 需要文件 IO，非线程安全，性能最差
        try (Searcher fileSearcher = Searcher.newWithFileOnly(dbPath)) {
            searchExample(fileSearcher);
        }

        // 基于 VectorIndex 查询，单次 search 需要内存查询 + 文件 IO，非线程安全，性能次之
        try (Searcher indexSearcher = Searcher.newWithVectorIndex(dbPath)) {
            searchExample(indexSearcher);
        }

        // 推荐：基于 Buffer 查询，单次 search 全使用内存查询，线程安全，支持序列化，性能最佳
        // 可重写 loadFile 方法扩展从其他文件系统(hdfs/s3/oss...)装载 buffer 数据
        try (Searcher bufferSearcher = Searcher.newWithBuffer(dbPath)) {
            searchExample(bufferSearcher);
        }

        // 备注：并发查询时，对于非线程安全实现推荐为每个线程创建一个独立的 searcher 对象单独使用。
    }

    private static void searchExample(Searcher searcher) {
        String ip = "1.2.3.4";
        try {
            long sTime = System.nanoTime();
            Searcher.Region region = searcher.searchRegion(ip);
            long cost = TimeUnit.NANOSECONDS.toMicros(System.nanoTime() - sTime);
            System.out.printf("{region: %s, searcher: %s, ioCount: %d, took: %d μs}\n",
                    region, searcher.getClass().getSimpleName(), region.getIoCount(), cost);
            System.out.println(region.toRegionMsg());
        } catch (Exception e) {
            System.out.printf("failed to search(%s): %s\n", ip, e);
        }
    }
}
