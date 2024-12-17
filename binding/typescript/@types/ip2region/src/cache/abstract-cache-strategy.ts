import * as fs from 'fs/promises';

export abstract class AbstractCacheStrategy {
    protected static readonly HEADER_INFO_LENGTH = 256;
    protected static readonly VECTOR_INDEX_ROWS = 256;
    protected static readonly VECTOR_INDEX_COLS = 256;
    protected static readonly VECTOR_INDEX_SIZE = 8;

    private fileHandle: fs.FileHandle | null = null;
    private fileSize: number = 0;
    private ioCounter = 0;

    protected constructor(protected readonly xdbPath: string) {
        // 构造不立即打开文件，懒加载
    }

    public get ioCount(): number {
        return this.ioCounter;
    }

    protected incrementIoCount() {
        this.ioCounter++;
    }

    /** 打开文件，若已打开则跳过 */
    protected async openFile(): Promise<void> {
        if (this.fileHandle === null) {
            const fh = await fs.open(this.xdbPath, 'r');
            this.fileHandle = fh;
            const stat = await fh.stat();
            this.fileSize = stat.size;
        }
    }

    protected getFileSize(): number {
        return this.fileSize;
    }

    /** 关闭文件 */
    public async closeFile(): Promise<void> {
        if (this.fileHandle) {
            await this.fileHandle.close();
            this.fileHandle = null;
        }
    }

    /**
     * 计算矢量索引的起始位置 
     * @param ip uint32
     */
    protected getVectorIndexStartPos(ip: number): number {
        const il0 = (ip >>> 24) & 0xFF;
        const il1 = (ip >>> 16) & 0xFF;
        const idx = il0 * AbstractCacheStrategy.VECTOR_INDEX_COLS * AbstractCacheStrategy.VECTOR_INDEX_SIZE
                  + il1 * AbstractCacheStrategy.VECTOR_INDEX_SIZE;
        return idx;
    }

    public abstract getVectorIndex(ip: number): Promise<Buffer>;

    /**
     * 异步读取文件数据（无需单资源锁，Node.js可安全并发读）
     * @param offset 文件起始偏移
     * @param length 读取字节数
     */
    public async getData(offset: number, length: number): Promise<Buffer> {
        await this.openFile();
        if (!this.fileHandle) {
            throw new Error('File handle is not available.');
        }

        // Node.js 通常会一次性读完，但仍保留循环以防偶发读不足
        let totalBytesRead = 0;
        const buffer = Buffer.allocUnsafe(length);

        while (totalBytesRead < length) {
            const { bytesRead } = await this.fileHandle.read({
                buffer,
                offset: totalBytesRead,
                length: length - totalBytesRead,
                position: offset + totalBytesRead
            });
            this.incrementIoCount();

            if (bytesRead === 0) {
                break; // 文件结束或出错
            }
            totalBytesRead += bytesRead;
        }

        return buffer.subarray(0, totalBytesRead);
    }
}