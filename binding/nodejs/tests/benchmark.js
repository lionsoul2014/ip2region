/*
 * Created by Wu Jian Ping on - 2022/07/22.
 */

const Benchmark = require('benchmark')
const path = require('path')
const Searcher = require('..')

const dbPath = path.join(__dirname, '..', '..', '..', 'data', 'ip2region.xdb')
const buffer = Searcher.loadContentFromFile(dbPath)
const searcher1 = Searcher.newWithBuffer(buffer)

const vectorIndex = Searcher.loadVectorIndexFromFile(dbPath)
const searcher2 = Searcher.newWithVectorIndex(dbPath, vectorIndex)

const searcher3 = Searcher.newWithFileOnly(dbPath)

const suite = new Benchmark.Suite()
suite
  .add('#缓存整个xdb数据【搜索218.4.167.70】', async () => {
    const ip = '218.4.167.70'
    return searcher1.search(ip)
  })
  .add('#缓存VectorIndex索引【搜索218.4.167.70】', async () => {
    const ip = '218.4.167.70'
    return searcher2.search(ip)
  })
  .add('#完全基于文件的查询【搜索218.4.167.70】', async () => {
    const ip = '218.4.167.70'
    return searcher3.search(ip)
  })
  .on('cycle', function (event) {
    console.log(String(event.target)) // eslint-disable-line
  })
  .on('complete', function () {
    console.log('Fastest is ' + this.filter('fastest').map('name')) // eslint-disable-line
  })
  .run({ async: true })
