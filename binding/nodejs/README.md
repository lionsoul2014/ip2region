# nodejs 客户端

## Install
```
npm install lionsoul-ip2region --save 
```

## Example

```
const searcher = require('lionsoul-ip2region').create();
searcher.btreeSearchSync('xxx.xxx.xxx.xxx')
// => { city: 2163, region: '中国|0|广东省|深圳市|联通' }
```

## 实现情况：

现已实现同步和异步查询，具体使用方法可以参考 `nodejs\tests\constructorTest.spec.js` 和`nodejs\tests\createTest.spec.js`。

## 如何贡献？

你可以任意修改代码，但必须确保通过全部的单元测试。要保证通过全部的单元测试，请在 Nodejs 控制台下切换到 nodejs 目录：

1）在此之前，请先运行 `npm i` 确保你已经安装了各类初始化第三方工具。
2）然后运行 `npm run coverage` 确保你的代码可以通过全部测试（必要时可以添加测试）。

```bash
D:\Projects\ip2region\binding\nodejs>npm run coverage

> ip2region@0.0.1 coverage D:\Projects\ip2region\binding\nodejs
> npm run test && jest --coverage


> ip2region@0.0.1 test D:\Projects\ip2region\binding\nodejs
> jest

 PASS  tests\constructorTest.spec.js
 PASS  tests\createTest.spec.js
 PASS  tests\exceptionTest.spec.js

Snapshot Summary
 › 168 snapshots written in 2 test suites.

Test Suites: 3 passed, 3 total
Tests:       14 passed, 14 total
Snapshots:   168 added, 168 total
Time:        1.645s
Ran all test suites.
 PASS  tests\constructorTest.spec.js
 PASS  tests\createTest.spec.js
 PASS  tests\exceptionTest.spec.js
----------------------|----------|----------|----------|----------|-------------------|
File                  |  % Stmts | % Branch |  % Funcs |  % Lines | Uncovered Line #s |
----------------------|----------|----------|----------|----------|-------------------|
All files             |    92.34 |    80.77 |       96 |    93.83 |                   |
 nodejs               |    91.95 |    80.26 |    95.65 |    93.51 |                   |
  ip2region.js        |    91.95 |    80.26 |    95.65 |    93.51 |... 09,410,460,484 |
 nodejs/tests/utils   |      100 |      100 |      100 |      100 |                   |
  asyncFor.js         |      100 |      100 |      100 |      100 |                   |
  fetchMainVersion.js |      100 |      100 |      100 |      100 |                   |
  testData.js         |      100 |      100 |      100 |      100 |                   |
----------------------|----------|----------|----------|----------|-------------------|

Test Suites: 3 passed, 3 total
Tests:       14 passed, 14 total
Snapshots:   168 passed, 168 total
Time:        1.792s
Ran all test suites.
```
3）使用benchmark测试，结果如下：
```bash
D:\Projects\ip2region\binding\nodejs>node D:\Projects\ip2region\binding\nodejs\tests\benchmarkTests\main.js
MemorySearchSync x 55,969 ops/sec ±2.22% (90 runs sampled)
BinarySearchSync x 610 ops/sec ±5.41% (77 runs sampled)
BtreeSearchSync x 2,439 ops/sec ±6.93% (69 runs sampled)
MemorySearch x 2,924 ops/sec ±0.67% (85 runs sampled)
BinarySearch x 154 ops/sec ±2.20% (69 runs sampled)
BtreeSearch x 294 ops/sec ±2.58% (76 runs sampled)
Rand    Name                Time (in milliseconds)
1       MemorySearchSync    0.018
2       MemorySearch        0.342
3       BtreeSearchSync     0.410
4       BinarySearchSync    1.639
5       BtreeSearch         3.407
6       BinarySearch        6.497
```