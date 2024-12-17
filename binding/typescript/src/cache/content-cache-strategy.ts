import { AbstractCacheStrategy } from "./abstract-cache-strategy";


export class ContentCacheStrategy extends AbstractCacheStrategy {
    private cacheData: Buffer | null = null;

    constructor(xdbPath: string) {
        super(xdbPath);
    }

    public async getVectorIndex(ip: number): Promise<Buffer> {
        if (!this.cacheData) {
            await this.ensureCacheLoaded();
        }
        if (!this.cacheData) {
            throw new Error('Cache data is not loaded.');
        }
        const idx = this.getVectorIndexStartPos(ip);
        return this.cacheData.subarray(
            AbstractCacheStrategy.HEADER_INFO_LENGTH + idx,
            AbstractCacheStrategy.HEADER_INFO_LENGTH + idx + AbstractCacheStrategy.VECTOR_INDEX_SIZE
        );
    }

    public override async getData(offset: number, length: number): Promise<Buffer> {
        if (!this.cacheData) {
            await this.ensureCacheLoaded();
        }
        if (!this.cacheData) {
            throw new Error('Cache data is not loaded.');
        }
        return this.cacheData.subarray(offset, offset + length);
    }

    private async ensureCacheLoaded(): Promise<void> {
        if (this.cacheData !== null) {
            return;
        }
        await this.openFile();
        const fileSize = this.getFileSize();
        this.cacheData = await super.getData(0, fileSize);
        // 全量读取后立即关闭文件
        await this.closeFile();
    }
}