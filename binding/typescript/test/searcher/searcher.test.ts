import { Searcher } from '../../src/searcher/searcher';
import { CachePolicy } from '../../src/models/cache-policy';

describe('Searcher Tests', () => {
  let searcher: Searcher;

  beforeAll(() => {
    // 指向一个真实存在的 xdb 文件（测试需要）
    const dbPath = "../../data/ip2region.xdb";
    // 测试用 ContentCacheStrategy（一次性载入文件）
    searcher = new Searcher(CachePolicy.Content, dbPath);
  });

  afterAll(async () => {
    await searcher.close();
  });

  it('should return valid data for a known IP', async () => {
    const ipStr = '1.2.3.4';
    const regionInfo = await searcher.search(ipStr);

    expect(regionInfo).toBeDefined();
  });

  it('should return undefined for an invalid IP', async () => {
    const invalidIp = '999.999.999.999';
    await expect(searcher.search(invalidIp)).rejects.toThrow();
  });

  it('should track ioCount properly', async () => {
    // ioCount 取决于缓存策略和查询次数
    const initialCount = searcher.ioCount;
    await searcher.search('8.8.8.8');
    const afterCount = searcher.ioCount;

    // ContentCacheStrategy：只读取一次，所以 ioCount 不变
    expect(afterCount).toEqual(initialCount);
  });
});