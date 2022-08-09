/*
 * Created by Wu Jian Ping on - 2022/07/22.
 */

const fs = require('fs')

const VectorIndexSize = 8
const VectorIndexCols = 256
const VectorIndexLength = 256 * 256 * (4 + 4)
const SegmentIndexSize = 14
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
      const sPtr = this._vectorIndex.readUInt32LE(idx)
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

  async writeString(fd, str) {
    return new Promise((resolve, reject) => {
      fs.write(fd, str, (err) => {
        if (err) reject(err);
        else resolve(err);
      })
    })
  }

  async closeFilePromise(fd) {
    return new Promise((resolve) => {
      fs.close(fd, (err) => {
        resolve(err);
      })
    })    
  }

  [openFilePromise] (fileName, op='r') {
    return new Promise((resolve, reject) => {
      fs.open(fileName, op, (err, fd) => {
        if (err) {
          reject(err)
        } else {
          resolve(fd)
        }
      })
    })
  }

  async dumptxt(fileName) {
    const startTime = process.hrtime();
    const ioStatus = {
      ioCount: 0
    };

    let fd = null

    if (!this._buffer) {
      fd = await this[openFilePromise](this._dbFile)
    }

    let fdOut = await this[openFilePromise](fileName, "w+");

    const idx0 = 0;
    const idx1 = 255 * VectorIndexCols * VectorIndexSize + 255 * VectorIndexSize;
    const { sPtr:sPtr0, ePtr:_ePtr0 } = await this[getStartEndPtr](idx0, fd, ioStatus)
    const { sPtr:_sPtr1, ePtr:ePtr1 } = await this[getStartEndPtr](idx1, fd, ioStatus)

    console.log(`${sPtr0}: ${ePtr1}`);
    let p = sPtr0;
    while (p < ePtr1) {
      p += SegmentIndexSize;
      const buff = await this[getBuffer](p, SegmentIndexSize, fd, ioStatus);
      if (buff.length == SegmentIndexSize) {
        const ip0 = buff.readUInt32LE(0);
        const ip1 = buff.readUInt32LE(4);
        const sip0 = this.Int2IP(ip0);
        const sip1 = this.Int2IP(ip1);
        const dataLen = buff.readUInt16LE(8);
        const dataPtr = buff.readUInt32LE(10);
        const data = await this[getBuffer](dataPtr, dataLen, fd, ioStatus);
        const result = data.toString('utf-8');
        const line = `${sip0}|${sip1}|${result}\n`;
        //console.log(line);
        await this.writeString(fdOut, line);
      }
    }
    await this.closeFilePromise(fdOut);
  }

  Int2IP(i) {
    let i0 = i >>> 24;
    let i1 = (i >>> 16) % 256;
    let i2 = (i >>> 8) % 256;
    let i3 = i % 256;
    return `${i0}.${i1}.${i2}.${i3}`;
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

    if (!this._buffer) {
      fd = await this[openFilePromise](this._dbFile)
    }

    const ps = ip.split('.')
    const i0 = parseInt(ps[0])
    const i1 = parseInt(ps[1])
    const i2 = parseInt(ps[2])
    const i3 = parseInt(ps[3])

    const ipInt = i0 * 256 * 256 * 256 + i1 * 256 * 256 + i2 * 256 + i3
    const idx = i0 * VectorIndexCols * VectorIndexSize + i1 * VectorIndexSize
    const { sPtr, ePtr } = await this[getStartEndPtr](idx, fd, ioStatus)
    let l = 0
    let h = (ePtr - sPtr) / SegmentIndexSize
    let result = null

    while (l <= h) {
      const m = (l + h) >> 1

      const p = sPtr + m * SegmentIndexSize

      const buff = await this[getBuffer](p, SegmentIndexSize, fd, ioStatus)

      const sip = buff.readUInt32LE(0)

      if (ipInt < sip) {
        h = m - 1
      } else {
        const eip = buff.readUInt32LE(4)
        if (ipInt > eip) {
          l = m + 1
        } else {
          const dataLen = buff.readUInt16LE(8)
          const dataPtr = buff.readUInt32LE(10)
          const data = await this[getBuffer](dataPtr, dataLen, fd, ioStatus)
          result = data.toString('utf-8')
          break
        }
      }
    }
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

const loadVectorIndexFromFile = dbPath => {
  const fd = fs.openSync(dbPath, 'r')
  const buffer = Buffer.alloc(VectorIndexLength)
  fs.readSync(fd, buffer, 0, VectorIndexLength, 256)
  fs.close(fd)
  return buffer
}

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
