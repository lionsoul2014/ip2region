// This test is used for tesing of exceptions

const IP2Region = require('../../ip2region');

describe('Constructor Test', () => {
  let instance;

  beforeAll(() => {
    instance = new IP2Region({ dbPath: '../../data/ip2region.db' })
  });

  afterAll(() => {
    instance.destroy();
  });

  test('IP invalid test', () => {
    const invalidIps = ['255.234.233', '255.255.-1.255', null, undefined, '', 'x.255.y.200'];
    for (const ip of invalidIps) {
      expect(() => instance.btreeSearchSync(ip)).toThrow();
      expect(() => instance.binarySearchSync(ip)).toThrow();
    }
  });

  test('File Not Found test', () => {
    expect(() => new IP2Region({ dbPath: 'A Bad File or Path Here' })).toThrow();
  });

});
