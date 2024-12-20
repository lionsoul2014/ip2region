import { AbstractCacheStrategy } from "./abstract-cache-strategy";

export class FileCacheStrategy extends AbstractCacheStrategy {
    public constructor(xdbPath: string) {
        super(xdbPath);
    }

    public async getVectorIndex(ip: number): Promise<Buffer> {
        const idx = this.getVectorIndexStartPos(ip);
        return this.getData(
            AbstractCacheStrategy.HEADER_INFO_LENGTH + idx,
            AbstractCacheStrategy.VECTOR_INDEX_SIZE
        );
    }
}