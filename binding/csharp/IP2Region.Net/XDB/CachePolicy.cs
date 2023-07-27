namespace IP2Region.Net.XDB;

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