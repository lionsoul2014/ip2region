#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
IP2Region Patch Override Tool

A lightweight Python tool for applying ip2region database patches without
modifying the original xdb database files. This creates a JSON cache that
acts as an override layer, checking patches before querying the main database.

Unlike the maker tools (Java/Golang/C++) which modify xdb files directly,
this tool provides a non-destructive patch application method.

Based on patch format from:
https://github.com/lionsoul2014/ip2region/tree/master/data/fix

Usage:
    python patch_override.py [--patches-dir DIR] [--output FILE]

Author: Community Contribution
License: Apache 2.0 (same as ip2region project)
"""

import json
import ipaddress
import sys
import argparse
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime

# Configure stdout for UTF-8 on Windows
if sys.platform == 'win32':
    try:
        if hasattr(sys.stdout, 'reconfigure'):
            sys.stdout.reconfigure(encoding='utf-8', errors='replace')
        else:
            import io
            sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
    except:
        pass


def ip_to_int(ip: str) -> int:
    """Convert IP address to integer."""
    try:
        return int(ipaddress.IPv4Address(ip))
    except:
        return 0


def parse_patch_file(patch_path: Path) -> List[Dict]:
    """
    Parse patch file format: start_ip|end_ip|Country|Province|City|ISP
    
    Returns list of patch entries with IP ranges converted to integers.
    """
    patches = []
    try:
        # Try UTF-8 first, fallback to GBK/GB2312 for Windows compatibility
        encodings = ['utf-8', 'gbk', 'gb2312', 'utf-8-sig']
        content = None
        encoding_used = None
        
        # Read as binary first to avoid any encoding issues
        with open(patch_path, 'rb') as f:
            raw_bytes = f.read()
        
        for enc in encodings:
            try:
                content = raw_bytes.decode(enc)
                encoding_used = enc
                # Verify Chinese characters decode correctly
                if '中国' in content[:500] or '北京' in content[:500]:
                    break
            except UnicodeDecodeError:
                continue
        
        if content is None:
            print(f"Warning: Could not decode {patch_path} with any encoding")
            return patches
        
        # Process lines
        for line_num, line in enumerate(content.splitlines(), 1):
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            
            # Format: start_ip|end_ip|Country|Province|City|ISP
            parts = line.split('|')
            if len(parts) >= 6:
                start_ip = parts[0].strip()
                end_ip = parts[1].strip()
                country = parts[2].strip()
                province = parts[3].strip()
                city = parts[4].strip()
                isp = parts[5].strip()
                
                # Convert IPs to integers for range checking
                try:
                    start_int = ip_to_int(start_ip)
                    end_int = ip_to_int(end_ip)
                    
                    if start_int > 0 and end_int > 0:
                        patches.append({
                            'start_ip': start_ip,
                            'end_ip': end_ip,
                            'start_int': start_int,
                            'end_int': end_int,
                            'country': country,
                            'province': province,
                            'city': city,
                            'isp': isp,
                            'source': patch_path.name,
                            'line': line_num
                        })
                except Exception as e:
                    print(f"Warning: Invalid IP range in {patch_path.name} line {line_num}: {e}")
                    continue
        
        if encoding_used and encoding_used != 'utf-8':
            print(f"  Note: File read with {encoding_used} encoding")
                        
    except Exception as e:
        print(f"Error parsing {patch_path}: {e}")
        import traceback
        traceback.print_exc()
    
    return patches


def build_patch_cache(patches_dir: Path, output_file: Path) -> Dict:
    """
    Build a cache of all patches for fast lookup.
    
    Args:
        patches_dir: Directory containing .fix patch files
        output_file: Path to save the JSON cache file
    
    Returns:
        Dictionary with patch cache data
    """
    if not patches_dir.exists():
        print(f"Patch directory not found: {patches_dir}")
        return {}
    
    # Find all patch files
    patch_files = list(patches_dir.glob("*.fix"))
    if not patch_files:
        print(f"No patch files found in {patches_dir}")
        return {}
    
    print(f"Found {len(patch_files)} patch file(s):")
    for pf in patch_files:
        print(f"  - {pf.name}")
    
    # Parse all patches
    all_patches = []
    for patch_file in patch_files:
        patches = parse_patch_file(patch_file)
        print(f"  Parsed {len(patches)} entries from {patch_file.name}")
        all_patches.extend(patches)
    
    print(f"\nTotal patches: {len(all_patches)}")
    
    if not all_patches:
        return {}
    
    # Build cache structure: list of patches sorted by start_int for binary search
    patches_sorted = sorted(all_patches, key=lambda x: x['start_int'])
    
    # Save to cache file with proper UTF-8 encoding
    cache_data = {
        'patches': patches_sorted,
        'total_patches': len(patches_sorted),
        'last_updated': datetime.now().isoformat(),
        'patch_files': [pf.name for pf in patch_files]
    }
    
    # Ensure output directory exists
    output_file.parent.mkdir(parents=True, exist_ok=True)
    
    # Save with UTF-8 encoding
    with open(output_file, 'w', encoding='utf-8', newline='\n') as f:
        json.dump(cache_data, f, ensure_ascii=False, indent=2)
    
    print(f"\nPatch cache saved to: {output_file}")
    print(f"Total patches cached: {len(patches_sorted)}")
    
    return cache_data


def load_patch_cache(cache_file: Path) -> Dict:
    """Load patch cache from file."""
    if not cache_file.exists():
        return {}
    
    try:
        # Try UTF-8 first, with fallback encodings
        encodings = ['utf-8', 'utf-8-sig', 'gbk', 'gb2312']
        cache_data = None
        
        for enc in encodings:
            try:
                with open(cache_file, 'r', encoding=enc) as f:
                    cache_data = json.load(f)
                    break
            except UnicodeDecodeError:
                continue
        
        if cache_data is None:
            print(f"Warning: Could not decode cache file with any encoding")
            return {}
        
        return cache_data
    except Exception as e:
        print(f"Error loading patch cache: {e}")
        return {}


def find_patch_for_ip(ip: str, cache: Dict) -> Optional[Dict]:
    """
    Find patch entry for a given IP address using binary search.
    
    Args:
        ip: IP address string
        cache: Patch cache dictionary
    
    Returns:
        Patch data dict if IP falls within any patch range, None otherwise
    """
    patches = cache.get('patches', [])
    if not patches:
        return None
    
    try:
        ip_int = ip_to_int(ip)
        if ip_int == 0:
            return None
        
        # Binary search for matching range
        left, right = 0, len(patches) - 1
        
        while left <= right:
            mid = (left + right) // 2
            patch = patches[mid]
            
            start_int = patch.get('start_int', 0)
            end_int = patch.get('end_int', 0)
            
            if start_int <= ip_int <= end_int:
                # Found matching range - return location data
                return {
                    'province': patch.get('province', ''),
                    'city': patch.get('city', ''),
                    'country': patch.get('country', '中国'),
                    'isp': patch.get('isp', '')
                }
            elif ip_int < start_int:
                right = mid - 1
            else:
                left = mid + 1
        
        return None
        
    except Exception as e:
        print(f"Error finding patch for IP {ip}: {e}")
        import traceback
        traceback.print_exc()
        return None


def main():
    """Main function to build and test patch cache."""
    parser = argparse.ArgumentParser(
        description='IP2Region Patch Override Tool - Create JSON cache from patch files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Use default paths (patches/ and patches_cache.json)
  python patch_override.py
  
  # Specify custom directories
  python patch_override.py --patches-dir ./data/fix --output ./cache.json
  
  # Test with specific IPs
  python patch_override.py --test-ip 39.144.0.1 --test-ip 39.144.10.5
        """
    )
    
    parser.add_argument(
        '--patches-dir',
        type=str,
        default='patches',
        help='Directory containing .fix patch files (default: patches)'
    )
    
    parser.add_argument(
        '--output',
        type=str,
        default='patches_cache.json',
        help='Output JSON cache file path (default: patches_cache.json)'
    )
    
    parser.add_argument(
        '--test-ip',
        action='append',
        dest='test_ips',
        help='Test IP addresses to verify patch lookup (can be specified multiple times)'
    )
    
    args = parser.parse_args()
    
    patches_dir = Path(args.patches_dir)
    output_file = Path(args.output)
    
    print("=" * 60)
    print("IP2Region Patch Override Tool")
    print("=" * 60)
    
    # Build patch cache
    print(f"\n[1/2] Building patch cache...")
    print(f"  Patches directory: {patches_dir}")
    print(f"  Output file: {output_file}")
    
    cache = build_patch_cache(patches_dir, output_file)
    
    if not cache:
        print("\nNo patches found. Exiting.")
        return
    
    # Test patch lookup if IPs provided
    if args.test_ips:
        print(f"\n[2/2] Testing patch lookup...")
        for test_ip in args.test_ips:
            patch = find_patch_for_ip(test_ip, cache)
            if patch:
                province = patch.get('province', '')
                city = patch.get('city', '')
                print(f"  {test_ip} -> {province}, {city} (from patch)")
            else:
                print(f"  {test_ip} -> No patch found (will use main database)")
    else:
        # Default test IPs
        print(f"\n[2/2] Testing patch lookup...")
        test_ips = ['39.144.0.1', '39.144.10.5', '39.144.177.100']
        for test_ip in test_ips:
            patch = find_patch_for_ip(test_ip, cache)
            if patch:
                province = patch.get('province', '')
                city = patch.get('city', '')
                print(f"  {test_ip} -> {province}, {city} (from patch)")
            else:
                print(f"  {test_ip} -> No patch found (will use main database)")
    
    print("\n" + "=" * 60)
    print("Patch cache creation complete!")
    print("\nNext steps:")
    print("1. Use the cache file in your IP geolocation service")
    print("2. Check patches before querying the main xdb database")
    print("3. Patches take priority over database results")
    print("=" * 60)


if __name__ == "__main__":
    main()

