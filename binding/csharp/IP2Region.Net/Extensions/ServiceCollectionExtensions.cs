using IP2Region.Net.Abstractions;
using IP2Region.Net.XDB;

namespace Microsoft.Extensions.DependencyInjection;

/// <summary>
/// IP2Region 服务扩展类
/// </summary>
public static class IP2RegionExtensions
{
    /// <summary>
    /// 添加 IP2RegionService 服务
    /// </summary>
    /// <param name="services"></param>
    public static IServiceCollection AddIP2RegionService(this IServiceCollection services, string path, CachePolicy cachePolicy = CachePolicy.Content)
    {
        services.AddSingleton<ISearcher>(provider =>
        {
            return new Searcher(cachePolicy, path);
        });
#if NET8_0_OR_GREATER
        services.AddKeyedSingleton("IP2Region.Net", (provider, _) =>
        {
            return provider.GetRequiredService<ISearcher>();
        });
#endif

        return services;
    }
}
