const Searcher = require('../')
const path = require('path')
const { ArgumentParser } = require('argparse')

// 处理输入参数
const parser = new ArgumentParser({
  add_help: true,
  description: 'ip2region test app',
  prog: 'node test.app.js',
  usage: 'Usage %(prog)s <agrs>'
})

parser.add_argument('-d', '--db', { help: `ip2region binary xdb file path, default: ${path.join(__dirname, '..', '..', '..', 'data', 'ip2region.xdb')}` })
parser.add_argument('-c', '--cache-policy', { help: 'cache policy: file/vectorIndex/content, default: content' })

const args = parser.parse_args()
const db = args.db
const policy = args.cache_policy

// 创建searcher对象
const createSearcher = () => {
  const dbPath = db || '../../data/ip2region.xdb'
  const cachePolicy = policy || 'content'

  let searcher = null
  let vectorIndex = null
  let buffer = null

  switch (cachePolicy) {
    case 'file':
      searcher = Searcher.newWithFileOnly(dbPath)
      break
    case 'vectorIndex':
      vectorIndex = Searcher.loadVectorIndexFromFile(dbPath)
      searcher = Searcher.newWithVectorIndex(dbPath, vectorIndex)
      break
    default:
      buffer = Searcher.loadContentFromFile(dbPath)
      searcher = Searcher.newWithBuffer(buffer)
  }
  console.log('parameters: ')
  console.log(`    dbPath: ${dbPath}`)
  console.log(`    cache-policy: ${cachePolicy}`)
  console.log('')
  return searcher
}

// 从控制台读取用户一行输入
const readlineSync = () => {
  return new Promise((resolve, reject) => {
    process.stdin.resume()
    process.stdin.on('data', data => {
      process.stdin.pause()
      resolve(data.toString('utf-8'))
    })
  })
}

const searcher = createSearcher()

async function main () {
  console.log('type \'quit\' to exit')
  while (true) {
    process.stdout.write('ip2region>> ')
    const ip = (await readlineSync()).trim()
    if (ip === 'quit') {
      process.exit(0)
    } else {
      try {
        const response = await searcher.search(ip)
        console.log(response)
      } catch (err) {
        console.log(err)
      }
    }
  }
}

main()
