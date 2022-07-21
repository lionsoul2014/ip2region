const path = require('path')
const Searcher = require('..')

const dbPath = path.join(__dirname, '..', '..', '..', 'data', 'ip2region.xdb')
const buffer = Searcher.loadContentFromFile(dbPath)
const seacher1 = Searcher.newWithBuffer(buffer)

const vectorIndex = Searcher.loadVectorIndexFromFile(dbPath)
const seacher2 = Searcher.newWithVectorIndex(dbPath, vectorIndex)

const seacher3 = Searcher.newWithFileOnly(dbPath)

;(async () => {
  const data1 = await Promise
    .all([
      seacher1.search('202.97.77.50'),
      seacher1.search('218.4.167.70'),
      seacher1.search('112.31.187.151'),
      seacher1.search('202.98.17.164'),
      seacher1.search('170.148.132.136'),
      seacher1.search('194.138.202.210')
    ])

  console.log(data1)

  const data2 = await Promise
    .all([
      seacher2.search('202.97.77.50'),
      seacher2.search('218.4.167.70'),
      seacher2.search('112.31.187.151'),
      seacher2.search('202.98.17.164'),
      seacher2.search('170.148.132.136'),
      seacher2.search('194.138.202.210')
    ])

  console.log(data2)

  const data3 = await Promise
    .all([
      seacher3.search('202.97.77.50'),
      seacher3.search('218.4.167.70'),
      seacher3.search('112.31.187.151'),
      seacher3.search('202.98.17.164'),
      seacher3.search('170.148.132.136'),
      seacher3.search('194.138.202.210')
    ])

  console.log(data3)
})()
