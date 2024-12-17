export interface ISearcher {
    search(ipStr: string): Promise<string | undefined>;
    searchByUint32(ip: number): Promise<string | undefined>;
    ioCount: number;
    close(): Promise<void>;
}