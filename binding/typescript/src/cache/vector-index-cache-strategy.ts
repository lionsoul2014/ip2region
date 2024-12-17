import { AbstractCacheStrategy } from "./abstract-cache-strategy";

export class VectorIndexCacheStrategy extends AbstractCacheStrategy {
    private vectorIndex: Buffer | null = null;

    constructor(xdbPath: string) {
        super(xdbPath);
    }

    public async getVectorIndex(ip: number): Promise<Buffer> {
        if (!this.vectorIndex) {
            await this.ensureVectorIndexLoaded();
        }
        if (!this.vectorIndex) {
            throw new Error('Vector index data not loaded.');
        }
        const idx = this.getVectorIndexStartPos(ip);
        return this.vectorIndex.subarray(idx, idx + AbstractCacheStrategy.VECTOR_INDEX_SIZE);
    }

    private async ensureVectorIndexLoaded(): Promise<void> {
        if (this.vectorIndex !== null) {
            return;
        }
        await this.openFile();
        const vectorLength = AbstractCacheStrategy.VECTOR_INDEX_ROWS
                           * AbstractCacheStrategy.VECTOR_INDEX_COLS
                           * AbstractCacheStrategy.VECTOR_INDEX_SIZE;
        this.vectorIndex = await super.getData(AbstractCacheStrategy.HEADER_INFO_LENGTH, vectorLength);
        // 保留 fileHandle 打开，因为后续可能还要继续读段数据
    }
}