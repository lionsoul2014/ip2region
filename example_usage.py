#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Example usage of the IP2Region Patch Override Tool

This demonstrates how to integrate the patch override system
into your IP geolocation service.
"""

from pathlib import Path
from patch_override import load_patch_cache, find_patch_for_ip


def example_basic_usage():
    """Basic example: Load cache and lookup IPs."""
    print("=" * 60)
    print("Example: Basic Patch Lookup")
    print("=" * 60)
    
    # Load the patch cache
    cache_file = Path("patches_cache.json")
    cache = load_patch_cache(cache_file)
    
    if not cache:
        print("No cache file found. Run patch_override.py first!")
        return
    
    print(f"Loaded {cache.get('total_patches', 0)} patches from cache")
    print(f"Last updated: {cache.get('last_updated', 'unknown')}")
    
    # Test IPs
    test_ips = [
        "39.144.0.1",      # Should match patch
        "39.144.10.5",    # Should match patch
        "39.144.177.100", # Should match patch
        "8.8.8.8",        # Should not match (no patch)
    ]
    
    print("\nTesting IP lookups:")
    for ip in test_ips:
        result = find_patch_for_ip(ip, cache)
        if result:
            print(f"  {ip:15} -> {result['province']}, {result['city']} (from patch)")
        else:
            print(f"  {ip:15} -> No patch found (use main database)")


def example_integration():
    """Example: Integration with IP geolocation service."""
    print("\n" + "=" * 60)
    print("Example: Service Integration")
    print("=" * 60)
    
    class SimpleIPGeolocation:
        """Simple example IP geolocation service."""
        
        def __init__(self, cache_file: Path):
            """Initialize with patch cache."""
            self.patch_cache = load_patch_cache(cache_file)
            print(f"Service initialized with {self.patch_cache.get('total_patches', 0)} patches")
        
        def get_location(self, ip: str):
            """
            Get location for an IP address.
            Checks patches first, then falls back to main database.
            """
            # Check patches first (patches take priority)
            patch_result = find_patch_for_ip(ip, self.patch_cache)
            if patch_result:
                return {
                    **patch_result,
                    'source': 'patch'
                }
            
            # In a real implementation, you would query the main database here
            # For this example, we'll just return None
            return {
                'source': 'main_database',
                'note': 'Would query main ip2region database here'
            }
    
    # Initialize service
    service = SimpleIPGeolocation(Path("patches_cache.json"))
    
    # Test lookups
    test_ip = "39.144.0.1"
    result = service.get_location(test_ip)
    
    print(f"\nLookup result for {test_ip}:")
    print(f"  Source: {result.get('source')}")
    if result.get('source') == 'patch':
        print(f"  Province: {result.get('province')}")
        print(f"  City: {result.get('city')}")
        print(f"  Country: {result.get('country')}")


if __name__ == "__main__":
    print("IP2Region Patch Override - Example Usage\n")
    
    # Run examples
    example_basic_usage()
    example_integration()
    
    print("\n" + "=" * 60)
    print("Examples complete!")
    print("=" * 60)

