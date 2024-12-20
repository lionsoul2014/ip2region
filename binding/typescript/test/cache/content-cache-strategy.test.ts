import { ContentCacheStrategy } from '../../src/cache/content-cache-strategy';

describe('ContentCacheStrategy Tests', () => {
  let strategy: ContentCacheStrategy;
  const dbPath = "../../data/ip2region.xdb";

  beforeAll(() => {
    strategy = new ContentCacheStrategy(dbPath);
  });

  afterAll(async () => {
    await strategy.closeFile();
  });

  it('should load the entire file into memory', async () => {
    // 首次访问会触发 ensureCacheLoaded
    const vectorIndex = await strategy.getVectorIndex(0x01020304);
    expect(vectorIndex.length).toBe(8); // VECTOR_INDEX_SIZE = 8

    // 再次读取不会增加IO次数太多
    const initIo = strategy.ioCount;
    await strategy.getVectorIndex(0x05060708);
    const newIo = strategy.ioCount;
    expect(newIo).toBe(initIo); // 全量缓存后几乎不增加IO
  });

  it('should read arbitrary data after caching', async () => {
    const data = await strategy.getData(0, 32); // 读取前32字节
    expect(data.length).toBe(32);
  });
});