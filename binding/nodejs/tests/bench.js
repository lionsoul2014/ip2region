const Benchmark = require('benchmark')
const Searcher = require('..')

const dbPath = '../../../data/ip2region.xdb'
const buffer = Searcher.loadContentFromFile(dbPath)
const seacher = Searcher.newWithBuffer(buffer)

const suite = new Benchmark.Suite()
suite
  .add('#search - 1', async () => {
    const ip = '202.97.77.50'
    return seacher.search(ip)
  })
  .add('#search - 2', async () => {
    const ip = '218.4.167.70'
    return seacher.search(ip)
  })
  .on('cycle', function (event) {
    console.log(String(event.target)) // eslint-disable-line
  })
  .on('complete', function () {
    console.log('Fastest is ' + this.filter('fastest').map('name')) // eslint-disable-line
  })
  .run({ async: true })
