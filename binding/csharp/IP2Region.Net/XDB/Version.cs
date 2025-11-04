using System.Buffers.Binary;

namespace IP2Region.Net.XDB;

public struct Version
{
    public int IndexSize { get; set; }

    public int Length { get; set; }

    public uint GetVectorIndexStartPos(ReadOnlyMemory<byte> buffer)
    {
        return BinaryPrimitives.ReadUInt32LittleEndian(buffer.Span);
    }

    public uint GetVectorIndexEndPos(ReadOnlyMemory<byte> buffer)
    {
        return BinaryPrimitives.ReadUInt32LittleEndian(buffer.Slice(Length).Span);
    }
}
