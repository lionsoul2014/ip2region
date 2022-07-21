const path = require('path')
const Searcher = require('.')

const dbPath = path.join(__dirname, '..', '..', '..', 'data', 'ip2region.xdb')
const buffer = Searcher.loadContentFromFile(dbPath)
const searcher1 = Searcher.newWithBuffer(buffer)

const vectorIndex = Searcher.loadVectorIndexFromFile(dbPath)
const searcher2 = Searcher.newWithVectorIndex(dbPath, vectorIndex)

const searcher3 = Searcher.newWithFileOnly(dbPath)

;(async () => {
  const data1 = await Promise
    .all([
      searcher1.search('202.97.77.50'),
      searcher1.search('218.4.167.70'),
      searcher1.search('112.31.187.151'),
      searcher1.search('202.98.17.164'),
      searcher1.search('170.148.132.136'),
      searcher1.search('194.138.202.210')
    ])

  console.log(data1)

  const data2 = await Promise
    .all([
      searcher2.search('202.97.77.50'),
      searcher2.search('218.4.167.70'),
      searcher2.search('112.31.187.151'),
      searcher2.search('202.98.17.164'),
      searcher2.search('170.148.132.136'),
      searcher2.search('194.138.202.210')
    ])

  console.log(data2)

  const data3 = await Promise
    .all([
      searcher3.search('202.97.77.50'),
      searcher3.search('218.4.167.70'),
      searcher3.search('112.31.187.151'),
      searcher3.search('202.98.17.164'),
      searcher3.search('170.148.132.136'),
      searcher3.search('194.138.202.210')
    ])

  console.log(data3)
})()
