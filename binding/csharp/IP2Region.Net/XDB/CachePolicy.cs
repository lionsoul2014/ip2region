namespace IP2Region.Net.XDB;

/// <summary>
/// 缓存策略枚举
/// </summary>
public enum CachePolicy
{
    /// <summary>
    /// no cache 
    /// </summary>
    File,

    /// <summary>
    /// cache vector index , reduce the number of IO operations
    /// </summary>
    VectorIndex,

    /// <summary>
    /// default cache policy , cache whole xdb file
    /// </summary>
    Content
}
