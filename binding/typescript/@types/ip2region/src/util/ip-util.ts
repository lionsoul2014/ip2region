export class Util {

    public static isValidIpAddress(ipStr: string): boolean {
        const octets = ipStr.split('.');
        if (octets.length !== 4) {
            return false;
        }
        for (const octet of octets) {
            if (isNaN(Number(octet)) || Number(octet) < 0 || Number(octet) > 255) {
                return false;
            }
        }
        return true;
    }

    /**
     * 把字符串形式的 IPv4 转为 uint32，小端序表示
     * 例如 "1.2.3.4" -> 0x04030201
     */
    public static ipAddressToUInt32(ipStr: string): number {
        if (!Util.isValidIpAddress(ipStr)) {
            throw new Error(`Invalid IPv4: ${ipStr}`);
        }
        const octets = ipStr.split('.').map(Number);       
        // 数据库是小端存储，如 1.2.3.4 => 0x04030201
        return (
            ((octets[3] & 0xff) << 24) |
            ((octets[2] & 0xff) << 16) |
            ((octets[1] & 0xff) << 8) |
            (octets[0] & 0xff)
        ) >>> 0;
    }
}
