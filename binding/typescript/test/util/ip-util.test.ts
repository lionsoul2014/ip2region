import { Util } from '../../src/util/ip-util';

describe('IP Util Tests', () => {
  it('should convert valid IPv4 string to uint32 correctly', () => {
    const ipStr = '1.2.3.4';
    const uint32 = Util.ipAddressToUInt32(ipStr);
    // 1.2.3.4 小端序 => 0x04030201 => 67305985
    expect(uint32).toBe(67305985); 
  });

  it('should throw error for invalid IPv4 string', () => {
    expect(() => {
      Util.ipAddressToUInt32('256.256.256.256');
    }).toThrow();
  });
});