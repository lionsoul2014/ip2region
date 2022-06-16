// This test is used for tesing of a static function `create` of IP2Region
const IP2Region = require('../../ip2region');
const testIps = require('../utils/testData');
const asyncFor = require('../utils/asyncFor');

describe('Constructor Test', () => {
  let instance;

  beforeAll(() => {
    instance = new IP2Region({ dbPath: '../../data/ip2region.db' });
  });

  afterAll(() => {
    IP2Region.destroy();
  });

  test('btreeSearchSync query', () => {
    for (const ip of testIps) {
      expect(instance.btreeSearchSync(ip)).toMatchSnapshot();
    }
  });

  test('binarySearchSync query', () => {
    for (const ip of testIps) {
      expect(instance.binarySearchSync(ip)).toMatchSnapshot();
    }
  });

  test('memorySearchSync query', () => {
    for (const ip of testIps) {
      expect(instance.memorySearchSync(ip)).toMatchSnapshot();
    }
  });

  //#region callBack
  test('binarySearch query', (done) => {
    asyncFor(testIps,
      (value, continueCallBack) => {
        instance.binarySearch(value, (err, result) => {
          expect(err).toBe(null);
          expect(result).toMatchSnapshot();
          continueCallBack();
        });
      },
      () => { done() });
  });

  test('btreeSearch query', (done) => {
    asyncFor(testIps,
      (value, continueCallBack) => {
        instance.btreeSearch(value, (err, result) => {
          expect(err).toBe(null);
          expect(result).toMatchSnapshot();
          continueCallBack();
        });
      },
      () => { done() });
  });

  test('memorySearch query', (done) => {
    asyncFor(testIps,
      (value, continueCallBack) => {
        instance.memorySearch(value, (err, result) => {
          expect(err).toBe(null);
          expect(result).toMatchSnapshot();
          continueCallBack();
        });
      },
      () => { done() });
  });

  //#endregion

  //#region Async Promisify test
  const node_ver = require('../utils/fetchMainVersion');

  // If we have Nodejs >= 8, we now support `async` and `await`
  if (node_ver >= 8) {

    const asyncBinarySearch = async (ip) => {

      return new Promise((resolve, reject) => {
        instance.binarySearch(ip, (err, result) => {
          if (err) {
            reject(err);
          }
          else {
            resolve(result);
          }
        });
      });

    };

    const asyncBtreeSearch = async (ip) => {

      return new Promise((resolve, reject) => {
        instance.btreeSearch(ip, (err, result) => {
          if (err) {
            reject(err);
          }
          else {
            resolve(result);
          }
        });
      });

    };

    const asyncMemorySearch = async (ip) => {
      return new Promise((succ, fail) => {
        instance.memorySearch(ip, (err, result) => {
          if (err) {
            fail(err);
          }
          else {
            succ(result);
          }
        });
      });
    }
    
    test('async binarySearch query', async () => {
      for (let i = 0; i < testIps.length; ++i) {
        const result = await asyncBinarySearch(testIps[i]);
        expect(result).toMatchSnapshot();
      }
    });

    test('async btreeSearch query', async () => {
      for (let i = 0; i < testIps.length; ++i) {
        const result = await asyncBtreeSearch(testIps[i]);
        expect(result).toMatchSnapshot();
      }
    });

    test('async memorySearch query', async () => {
      for (let i = 0; i < testIps.length; ++i) {
        const result = await asyncMemorySearch(testIps[i]);
        expect(result).toMatchSnapshot();
      }
    });
  }
  //#endregion
});
