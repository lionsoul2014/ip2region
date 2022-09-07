namespace IP2Region.Net.XDB;

public enum CachePolicy
{
    /// <summary>
    /// no cache , not thread safe!
    /// </summary>
    File,
    /// <summary>
    /// cache vector index , reduce the number of IO operations , not thread safe!
    /// </summary>
    VectorIndex,
    /// <summary>
    /// default cache policy , cache whole xdb file , thread safe 
    /// </summary>
    Content
}