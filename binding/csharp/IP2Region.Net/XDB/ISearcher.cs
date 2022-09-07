using System.Net;

namespace IP2Region.Net.XDB;

public interface ISearcher
{
    string? Search(string ipStr);

    string? Search(IPAddress ipAddress);

    string? Search(uint ipAddress);

    int IoCount { get; }
}