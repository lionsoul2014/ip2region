import { VectorIndexCacheStrategy } from '../../src/cache/vector-index-cache-strategy';

describe('VectorIndexCacheStrategy Tests', () => {
  let strategy: VectorIndexCacheStrategy;
  //const dbPath = join(__dirname, '..', 'data', 'ip2region.xdb');
  const dbPath = "../../data/ip2region.xdb";

  beforeAll(() => {
    strategy = new VectorIndexCacheStrategy(dbPath);
  });

  afterAll(async () => {
    await strategy.closeFile();
  });

  it('should cache the vector index in memory', async () => {
    // 首次调用会把矢量索引读到内存
    const initIo = strategy.ioCount;
    const vecIndex1 = await strategy.getVectorIndex(0x01020304);
    expect(vecIndex1.length).toBe(8);
    const afterIo1 = strategy.ioCount;
    expect(afterIo1).toBeGreaterThan(initIo);

    // 第二次获取矢量索引不会增加IO
    const vecIndex2 = await strategy.getVectorIndex(0x05060708);
    expect(vecIndex2.length).toBe(8);
    const afterIo2 = strategy.ioCount;
    expect(afterIo2).toBe(afterIo1);
  });

  it('should still read segment data from file as needed', async () => {
    // 模拟读取部分数据段
    const offset = 1024; // 示例偏移
    const length = 32;
    const segmentData = await strategy.getData(offset, length);
    expect(segmentData.length).toBe(length);
    // 这会增加IO
    expect(strategy.ioCount).toBeGreaterThan(0);
  });
});