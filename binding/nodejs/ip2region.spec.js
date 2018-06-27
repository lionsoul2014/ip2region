const path = require('path');
const IP2Region = require('./ip2region');

describe('ip2region', () => {
  let instance;

  beforeAll(() => {
    instance = IP2Region.create(path.join(__dirname, '../../data/ip2region.db'));
  });

  afterAll(() => {
    instance.destroy();
  });

  test('should query', () => {
    expect(instance.btreeSearchSync('120.24.78.68')).toMatchSnapshot();
    expect(instance.btreeSearchSync('10.10.10.10')).toMatchSnapshot();
  });

  test('binarySearch', () => {
    expect(instance.binarySearchSync('120.24.78.68')).toMatchSnapshot();
    expect(instance.binarySearchSync('10.10.10.10')).toMatchSnapshot();
  });
});
