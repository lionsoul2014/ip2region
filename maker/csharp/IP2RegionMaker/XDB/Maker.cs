// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Alan Lee <lzh.shap@gmail.com>
// @Date   2022/8/8

// --- Ip2Region v2.0 data structure
//
// +----------------+--------------------------+---------------+--------------+
// | header space   | vector speed up index    |  data payload | block index  |
// +----------------+--------------------------+---------------+--------------+
// | 256 bytes      | 512 KiB (fixed)          | dynamic size  | dynamic size |
// +----------------+--------------------------+---------------+--------------+
//
// 1. padding space : for header info like block index ptr, version, release date eg ... or any other temporary needs.
// -- 2bytes: version number, different version means structure update, it fixed to 2 for now
// -- 2bytes: index algorithm code.
// -- 4bytes: generate unix timestamp (version)
// -- 4bytes: index block start ptr
// -- 4bytes: index block end ptr
//
//
// 2. data block : region or whatever data info.
// 3. segment index block : binary index block.
// 4. vector index block  : fixed index info for block index search speed up.
// space structure table:
// -- 0   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 1   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- 2   -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
// -- ...
// -- 255 -> | 1rt super block | 2nd super block | 3rd super block | ... | 255th super block
//
//
// super block structure:
// +-----------------------+----------------------+
// | first index block ptr | last index block ptr |
// +-----------------------+----------------------+
//
// data entry structure:
// +--------------------+-----------------------+
// | 2bytes (for desc)	| dynamic length		|
// +--------------------+-----------------------+
//  data length   whatever in bytes
//
// index entry structure
// +------------+-----------+---------------+------------+
// | 4bytes		| 4bytes	| 2bytes		| 4 bytes    |
// +------------+-----------+---------------+------------+
//  start ip 	  end ip	  data length     data ptr

using System.Text;

namespace IP2RegionMaker.XDB
{
    public class Maker
    {
        const ushort VersionNo = 2;
        const int HeaderInfoLength = 256;
        const int VectorIndexRows = 256;
        const int VectorIndexCols = 256;
        const int VectorIndexSize = 8;
        const int SegmentIndexSize = 14;
        const int VectorIndexLength = VectorIndexRows * VectorIndexCols * VectorIndexSize;

        private readonly Stream _srcHandle;
        private readonly Stream _dstHandle;
        private readonly IndexPolicy _indexPolicy;
        private readonly List<Segment> _segments;
        private readonly Dictionary<string, uint> _regionPool;
        private readonly byte[] _vectorIndex;

        public Maker(IndexPolicy indexPolicy,string srcFile, string dstFile)
        {
            _indexPolicy = indexPolicy;

            _srcHandle = File.Open(@srcFile, FileMode.Open);
            _dstHandle = File.Open(@dstFile, FileMode.Create);
            _segments = new List<Segment>();
            _regionPool = new Dictionary<string, uint>();
            _vectorIndex = new byte[VectorIndexLength];
        }

        ~Maker()
        {
            _srcHandle.Close();
            _dstHandle.Close();
        }

        private void InitDbHeader()
        {
            _srcHandle.Seek(0, SeekOrigin.Begin);

            var header = new byte[HeaderInfoLength];
            BitConverter.GetBytes(VersionNo).CopyTo(header, 0);
            BitConverter.GetBytes((ushort)_indexPolicy).CopyTo(header, 2);
            BitConverter.GetBytes(DateTimeOffset.UtcNow.ToUnixTimeSeconds()).CopyTo(header, 4);
            BitConverter.GetBytes((uint)0).CopyTo(header, 8);
            BitConverter.GetBytes((uint)0).CopyTo(header, 12);

            using var writer = new BinaryWriter(_dstHandle, Encoding.UTF8, true);
            writer.Write(header);
        }

        private void LoadSegments()
        {
            Console.WriteLine("try to load the segments ... ");


            using var reader = new StreamReader(_srcHandle);
            while (true)
            {
                var line = reader.ReadLine();


                if (line == null) break;

                var seg=Util.GetSegment(line);

                _segments?.Add(seg);
            }

            if (_segments!=null)
            {
                Util.CheckSegments(_segments);
            }

            Console.WriteLine($"all segments loaded, length: {_segments?.Count}");
        }


        public void Init()
        {
            InitDbHeader();
            LoadSegments();
        }

        public void Build()
        {
            _dstHandle.Seek(HeaderInfoLength + VectorIndexLength, SeekOrigin.Begin);
            using var writer = new BinaryWriter(_dstHandle, Encoding.UTF8, false);

            Console.WriteLine("try to write the data block ... ");

            foreach (var seg in _segments)
            {
                Console.WriteLine($"try to write region {seg.Region}");

                if (_regionPool.ContainsKey(seg.Region))
                {
                    Console.WriteLine($"--[Cached] with ptr={_regionPool[seg.Region]}");
                    continue;
                }

                var region = Encoding.UTF8.GetBytes(seg.Region);

                if (region.Length > 0xFFFF)
                {
                    throw new ArgumentException($"too long region info `{seg.Region}`: should be less than {0xFFFF} bytes");
                }

                var pos = _dstHandle.Seek(0, SeekOrigin.Current);
                writer.Write(region);

                _regionPool[seg.Region] = (uint)pos;
            }

            Console.WriteLine("try to write the segment index block ... ");

            var indexBuff = new byte[SegmentIndexSize];
            var counter = 0;
            long startPtr = -1;
            long endPtr = -1;
            foreach (var seg in _segments)
            {
                var dataPtr = _regionPool[seg.Region];
                if (!_regionPool.ContainsKey(seg.Region))
                {
                    throw new Exception($"missing ptr cache for region `{seg.Region}`");
                }

                var datalen = Encoding.UTF8.GetBytes(seg.Region).Length;

                if (datalen < 1)
                {
                    throw new ArgumentNullException(nameof(seg.Region));
                }

                var segList = seg.Split();
                Console.WriteLine($"try to index segment({segList.Count}) {seg} ...");

                foreach (var item in segList)
                {
                    var pos = _dstHandle.Seek(0, SeekOrigin.Current);

                    BitConverter.GetBytes(item.StartIP).CopyTo(indexBuff, 0);
                    BitConverter.GetBytes(item.EndIP).CopyTo(indexBuff, 4);
                    BitConverter.GetBytes((ushort)datalen).CopyTo(indexBuff, 8);
                    BitConverter.GetBytes(dataPtr).CopyTo(indexBuff, 10);

                    writer.Write(indexBuff);

                    Console.WriteLine($"|-segment index: {counter}, ptr: {pos}, segment: {seg}");
                    SetVectorIndex(item.StartIP, (uint)pos);

                    counter++;

                    if (startPtr == -1)
                    {
                        startPtr = pos;
                    }

                    endPtr = pos;
                }
            }

            Console.WriteLine($"try to write the vector index block ... ");

            _dstHandle.Seek(HeaderInfoLength, SeekOrigin.Begin);
            writer.Write(_vectorIndex);


            Console.WriteLine("try to write the segment index ptr ... ");
            BitConverter.GetBytes((uint)startPtr).CopyTo(indexBuff, 0);
            BitConverter.GetBytes((uint)endPtr).CopyTo(indexBuff, 4);
            _dstHandle.Seek(0, SeekOrigin.Begin);

            writer.Write(indexBuff[..8]);

            Console.WriteLine($"write done, dataBlocks: {_regionPool.Count}, indexBlocks: ({_segments.Count}, {counter}), indexPtr: ({startPtr}, {endPtr})");
        }

        private void SetVectorIndex(uint ip, uint ptr)
        {
            var il0 = (ip >> 24) & 0xFF;
            var il1 = (ip >> 16) & 0xFF;
            var idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;

            ArraySegment<byte> bytes = new(_vectorIndex, (int)idx, _vectorIndex.Length - 1 - (int)idx);
            var sPtr = BitConverter.ToUInt32(bytes);

            if (sPtr == 0)
            {
                BitConverter.GetBytes(ptr).CopyTo(_vectorIndex, idx);
                BitConverter.GetBytes(ptr + SegmentIndexSize).CopyTo(_vectorIndex, idx + 4);
            }
            else
            {
                BitConverter.GetBytes(ptr + SegmentIndexSize).CopyTo(_vectorIndex, idx + 4);
            }
        }


    }

}
