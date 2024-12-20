import { FileCacheStrategy } from '../../src/cache/file-cache-strategy';

describe('FileCacheStrategy Tests', () => {
  let strategy: FileCacheStrategy;
  //const dbPath = join(__dirname, '..', 'data', 'ip2region.xdb');
  const dbPath = "../../data/ip2region.xdb";
  beforeAll(() => {
    strategy = new FileCacheStrategy(dbPath);
  });

  afterAll(async () => {
    await strategy.closeFile();
  });

  it('should read vector index from file each time', async () => {
    // 每次获取矢量索引，都会触发一次文件读取
    const initIo = strategy.ioCount;
    const vecIndex1 = await strategy.getVectorIndex(0x01020304);
    expect(vecIndex1.length).toBe(8);
    const afterIo1 = strategy.ioCount;
    expect(afterIo1).toBeGreaterThan(initIo);

    // 第二次读取仍增加IO，因为不缓存
    const vecIndex2 = await strategy.getVectorIndex(0x05060708);
    expect(vecIndex2.length).toBe(8);
    const afterIo2 = strategy.ioCount;
    expect(afterIo2).toBeGreaterThan(afterIo1);
  });

  it('should read arbitrary data directly from file', async () => {
    const offset = 0;
    const length = 64;
    const data = await strategy.getData(offset, length);
    expect(data.length).toBe(length);
  });
});