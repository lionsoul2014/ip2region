import { CachePolicy } from "../models/cache-policy";
import { AbstractCacheStrategy } from "./abstract-cache-strategy";
import { ContentCacheStrategy } from "./content-cache-strategy";
import { FileCacheStrategy } from "./file-cache-strategy";
import { VectorIndexCacheStrategy } from "./vector-index-cache-strategy";

export class CacheStrategyFactory {
    constructor(private readonly xdbPath: string) {}

    public createCacheStrategy(cachePolicy: CachePolicy): AbstractCacheStrategy {
        switch (cachePolicy) {
            case CachePolicy.Content:
                return new ContentCacheStrategy(this.xdbPath);
            case CachePolicy.VectorIndex:
                return new VectorIndexCacheStrategy(this.xdbPath);
            case CachePolicy.File:
                return new FileCacheStrategy(this.xdbPath);
            default:
                throw new Error(`Unknown cachePolicy: ${cachePolicy}`);
        }
    }
}
