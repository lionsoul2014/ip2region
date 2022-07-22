/*
 * Created by Wu Jian Ping on - 2022/07/22.
 */

const fs = require('fs')

// 常量定义
// 每个 vector 索引项的字节数
const VectorIndexSize = 8
// vector 索引的列数
const VectorIndexCols = 256
// vector 索引段整个的字节数
const VectorIndexLength = 256 * 256 * (4 + 4)
// 二分索引项的字节数
const SegmentIndexSize = 14
// IPv4检查正则
const IP_REGEX = /^((\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])\.){3}(\d|[1-9]\d|1\d\d|2[0-4]\d|25[0-5])$/

const getStartEndPtr = Symbol('#getStartEndPtr')
const getBuffer = Symbol('#getBuffer')
const openFilePromise = Symbol('#openFilePromise')

class Searcher {
  constructor (dbFile, vectorIndex, buffer) {
    this._dbFile = dbFile
    this._vectorIndex = vectorIndex
    this._buffer = buffer

    if (this._buffer) {
      this._vectorIndex = this._buffer.subarray(256, 256 + VectorIndexLength)
    }
  }

  async [getStartEndPtr] (idx, fd, ioStatus) {
    if (this._vectorIndex) {
      // 区域二分索引的开始地址
      // idx 开始读取 4 个字节，用小端字节序解码得到一个整数
      const sPtr = this._vectorIndex.readUInt32LE(idx)
      // 二分区域索引的结束地址
      // idx + 4 处读取 4 个字节，用小端字节序解码得到一个整数
      const ePtr = this._vectorIndex.readUInt32LE(idx + 4)
      return { sPtr, ePtr }
    } else {
      const buf = await this[getBuffer](256 + idx, 8, fd, ioStatus)
      const sPtr = buf.readUInt32LE()
      const ePtr = buf.readUInt32LE(4)
      return { sPtr, ePtr }
    }
  }

  async [getBuffer] (offset, length, fd, ioStatus) {
    if (this._buffer) {
      return this._buffer.subarray(offset, offset + length)
    } else {
      // 从文件中读取
      const buf = Buffer.alloc(length)
      return new Promise((resolve, reject) => {
        ioStatus.ioCount += 1
        fs.read(fd, buf, 0, length, offset, (err) => {
          if (err) {
            reject(err)
          } else {
            resolve(buf)
          }
        })
      })
    }
  }

  // fsPromises.open需要node.js v10.0.0以上版本才行，这边使用Promise包一下
  [openFilePromise] (fileName) {
    return new Promise((resolve, reject) => {
      fs.open(fileName, 'r', (err, fd) => {
        if (err) {
          reject(err)
        } else {
          resolve(fd)
        }
      })
    })
  }

  async search (ip) {
    const startTime = process.hrtime()
    const ioStatus = {
      ioCount: 0
    }

    if (!isValidIp(ip)) {
      throw new Error(`IP: ${ip} is invalid`)
    }

    let fd = null

    // 假如不是通过newWithBuffer创建的对象，那么一定是需要依赖文件的
    if (!this._buffer) {
      // 不存在_buffer的情况
      fd = await this[openFilePromise](this._dbFile)
    }

    // 切割IP
    const ps = ip.split('.')
    // 将各段转成int
    const i0 = parseInt(ps[0])
    const i1 = parseInt(ps[1])
    const i2 = parseInt(ps[2])
    const i3 = parseInt(ps[3])

    // 假如使用移位操作的话，这边可能产生负数
    const ipInt = i0 * 256 * 256 * 256 + i1 * 256 * 256 + i2 * 256 + i3

    // 计算得到 vector 索引项的开始地址。
    // 这里可以对比上述的 vector table 结构进行理解
    const idx = i0 * VectorIndexCols * VectorIndexSize + i1 * VectorIndexSize

    // 区域二分索引的开始地址和结束地址
    const { sPtr, ePtr } = await this[getStartEndPtr](idx, fd, ioStatus)

    // 二分搜索低位
    let l = 0
    // 二分搜索高位
    // 上第一步得到的结束索引位置减去开始索引位置
    // 再除以每个索引的字节大小就是这次搜索要扫描的索引的个数了。
    let h = (ePtr - sPtr) / SegmentIndexSize

    let result = null

    while (l <= h) {
      // 得到中间的那个索引
      const m = (l + h) >> 1

      // 计算中间索引项的指针地址
      // 在起始地址 sPtr 上加上 m 的相对地址即可
      const p = sPtr + m * SegmentIndexSize

      // 从 p 位置开始读取 SegmentIndexSize = 14 个字节到 buff
      // 得到一个完整的上述描述的二分索引项，不过为了减少不必要的操作
      // 我们是按需要解码，此处 buff 为 p 开始的 14 个 byte 的数据
      const buff = await this[getBuffer](p, SegmentIndexSize, fd, ioStatus)

      // 前面 4 个字节是起始 IP
      const sip = buff.readUInt32LE(0)

      if (ipInt < sip) {
        // 目标比 sip 小，也就是在 p 位置的左边
        h = m - 1
      } else {
        // 4 ~ 7 之间的 4 个字节是结束 IP 地址
        const eip = buff.readUInt32LE(4)
        if (ipInt > eip) {
          // 目标 ip 比 eip 大，也就是在 p 位置的右边
          l = m + 1
        } else {
          // 搜索命中
          // 目标 ip 正好在 sip 和 eip 之间
          // 也就是找到目标 ip 了

          // 8 ~ 10 两个字节是地域数据的长度
          const dataLen = buff.readUInt16LE(8)
          // 10 ~ 13 的 4 个字节是地域数据的地址
          const dataPtr = buff.readUInt32LE(10)
          const data = await this[getBuffer](dataPtr, dataLen, fd, ioStatus)
          result = data.toString('utf-8')
          break
        }
      }
    }

    // 异步关闭文件，使用同步关闭的话，会极大影响性能
    // 超高并发下，由于是异步close，新开文件会导致too many open files错误
    // 建议使用完全缓存xdb文件模式
    if (fd) {
      fs.close(fd)
    }

    const diff = process.hrtime(startTime)

    const took = (diff[0] * 1e9 + diff[1]) / 1e3
    return { region: result, ioCount: ioStatus.ioCount, took }
  }
}

const _checkFile = dbPath => {
  try {
    fs.accessSync(dbPath, fs.constants.F_OK)
  } catch (err) {
    throw new Error(`${dbPath} ${err ? 'does not exist' : 'exists'}`)
  }

  try {
    fs.accessSync(dbPath, fs.constants.R_OK)
  } catch (err) {
    throw new Error(`${dbPath} ${err ? 'is not readable' : 'is readable'}`)
  }
}

const isValidIp = ip => {
  return IP_REGEX.test(ip)
}

const newWithFileOnly = dbPath => {
  _checkFile(dbPath)

  return new Searcher(dbPath, null, null)
}

const newWithVectorIndex = (dbPath, vectorIndex) => {
  _checkFile(dbPath)

  if (!Buffer.isBuffer(vectorIndex)) {
    throw new Error('vectorIndex is invalid')
  }

  return new Searcher(dbPath, vectorIndex, null)
}

const newWithBuffer = buffer => {
  if (!Buffer.isBuffer(buffer)) {
    throw new Error('buffer is invalid')
  }

  return new Searcher(null, null, buffer)
}

// 从文件中获取VectorIndex数据
const loadVectorIndexFromFile = dbPath => {
  const fd = fs.openSync(dbPath, 'r')
  const buffer = Buffer.alloc(VectorIndexLength)
  fs.readSync(fd, buffer, 0, VectorIndexLength, 256)
  fs.close(fd)
  return buffer
}

// 将文件转换成buffer
const loadContentFromFile = dbPath => {
  const stats = fs.statSync(dbPath)
  const buffer = Buffer.alloc(stats.size)
  const fd = fs.openSync(dbPath, 'r')
  fs.readSync(fd, buffer, 0, stats.size, 0)
  fs.close(fd)
  return buffer
}

module.exports = {
  isValidIp,
  loadVectorIndexFromFile,
  loadContentFromFile,
  newWithFileOnly,
  newWithVectorIndex,
  newWithBuffer
}
