import { AbstractCacheStrategy } from "../cache/abstract-cache-strategy";
import { CacheStrategyFactory } from "../cache/cache-strategy-factory";
import { CachePolicy } from "../models/cache-policy";
import { Util } from "../util/ip-util";
import { ISearcher } from "./ISearcher";

export class Searcher implements ISearcher {
    private readonly cacheStrategy: AbstractCacheStrategy;

    constructor(cachePolicy: CachePolicy, dbPath: string) {
        const factory = new CacheStrategyFactory(dbPath);
        this.cacheStrategy = factory.createCacheStrategy(cachePolicy);
    }

    public get ioCount(): number {
        return this.cacheStrategy.ioCount;
    }

    /** 主动关闭 */
    public async close(): Promise<void> {
        await this.cacheStrategy.closeFile();
    }

    public async search(ipStr: string): Promise<string | undefined> {
        const ip = Util.ipAddressToUInt32(ipStr);
        return this.searchByUint32(ip);
    }

    /**
     * 优化后的二分查找——一次性读 sPtr~ePtr 所有段数据，而不是循环 getData()
     */
    public async searchByUint32(ip: number): Promise<string | undefined> {
        const index = await this.cacheStrategy.getVectorIndex(ip);
        if (index.length < 8) {
            return undefined;
        }

        // 读取 sPtr, ePtr（uint32, 小端序）
        const sPtr = index.readUInt32LE(0);
        const ePtr = index.readUInt32LE(4);

        const SEGMENT_INDEX_SIZE = 14;
        const totalCount = Math.floor((ePtr - sPtr) / SEGMENT_INDEX_SIZE) + 1;
        if (totalCount <= 0) {
            return undefined;
        }

        // 一次性批量读取 [sPtr, ePtr] 范围内的所有 segment
        const blockSize = totalCount * SEGMENT_INDEX_SIZE;
        const blockBuff = await this.cacheStrategy.getData(sPtr, blockSize);

        // 在内存中完成二分查找
        let l = 0;
        let h = totalCount - 1;
        let dataLen = 0;
        let dataPtr = 0;

        while (l <= h) {
            const mid = (l + h) >>> 1;  // 中点
            const offset = mid * SEGMENT_INDEX_SIZE;

            const sip = blockBuff.readUInt32LE(offset);
            const eip = blockBuff.readUInt32LE(offset + 4);

            if (ip < sip) {
                h = mid - 1;
            } else if (ip > eip) {
                l = mid + 1;
            } else {
                dataLen = blockBuff.readUInt16LE(offset + 8);
                dataPtr = blockBuff.readUInt32LE(offset + 10);
                break;
            }
        }

        if (dataLen === 0) {
            return undefined;
        }

        // 读取 region 数据
        const regionBuff = await this.cacheStrategy.getData(dataPtr, dataLen);
        return regionBuff.toString('utf8');
    }
}
