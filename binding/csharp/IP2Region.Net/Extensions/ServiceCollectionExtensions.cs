using IP2Region.Net.Abstractions;
using IP2Region.Net.XDB;
using Microsoft.Extensions.DependencyInjection.Extensions;

namespace Microsoft.Extensions.DependencyInjection;

/// <summary>
/// IP2Region 服务扩展类
/// </summary>
public static class IP2RegionExtensions
{
    /// <summary>
    /// 添加 IP2RegionService 服务。
    /// </summary>
    /// <param name="services"><see cref="IServiceCollection"/> 集合</param>
    /// <param name="path">IP2Region 数据库文件的路径。</param>
    /// <param name="cachePolicy">缓存策略，默认为 <see cref="CachePolicy.Content"/>。</param>
    public static IServiceCollection AddIP2RegionService(this IServiceCollection services, string path, CachePolicy cachePolicy = CachePolicy.Content)
    {
        services.TryAddSingleton<ISearcher>(provider =>
        {
            return new Searcher(cachePolicy, path);
        });
#if NET8_0_OR_GREATER
        services.TryAddKeyedSingleton("IP2Region.Net", (provider, _) =>
        {
            return provider.GetRequiredService<ISearcher>();
        });
#endif

        return services;
    }
}
