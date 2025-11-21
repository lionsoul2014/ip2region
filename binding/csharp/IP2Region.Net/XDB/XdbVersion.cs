namespace IP2Region.Net.XDB;

/// <summary>
/// XdbVersion 结构体
/// </summary>
public struct XdbVersion
{
    /// <summary>
    /// 获得/设置 版本号
    /// </summary>
    public ushort Ver { get; set; }

    /// <summary>
    /// 获得/设置 缓存策略
    /// </summary>
    public ushort CachePolice { get; set; }

    /// <summary>
    /// 获得/设置 文件生成时间
    /// </summary>
    public DateTimeOffset CreatedTime { get; set; }

    /// <summary>
    /// 获得/设置 索引起始地址
    /// </summary>
    public uint StartIndex { get; set; }

    /// <summary>
    /// 获得/设置 索引结束地址
    /// </summary>
    public uint EndIndex { get; set; }

    /// <summary>
    /// 获得/设置 IP版本
    /// </summary>
    public ushort IPVer { get; set; }

    /// <summary>
    /// 获得/设置 指针字节数
    /// </summary>
    public ushort BytesCount { get; set; }
}
