# xdb-parse
A parser to xdb files like ip2region
A high-performance Rust library for parsing and querying IP database files (xdb format) like ip2region. Supports both IPv4 and IPv6 address lookup with efficient binary search algorithms.

## Features

- **Dual Protocol Support**: Full support for both IPv4 and IPv6 address lookup
- **High Performance**: Optimized binary search algorithm for fast IP queries
- **Memory Efficient**: Load database files once and query multiple times
- **Thread Safe**: Designed for concurrent usage in multi-threaded environments
- **Flexible Input**: Accepts IP addresses in multiple formats (string, numeric)
- **Zero Dependencies**: Minimal external dependencies for reliability

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
xdb-parse = "0.1.0"
```

## Quick Start

### Basic Usage

```rust
use xdb_parse::utils::{load_file, search_ip};
use anyhow::Result;

fn main() -> Result<()> {
    // Load the IP database file
    let path = "./assets/ip2region_v4.xdb";
    let data = load_file(path.into())?;
    
    // Search for an IP address
    let result = search_ip("73.24.63.66", &data)?;
    println!("IP location: {}", result);
    
    Ok(())
}
```

### IPv6 Support

```rust
use xdb_parse::utils::{load_file, search_ip};
use anyhow::Result;

fn main() -> Result<()> {
    // Load IPv6 database
    let path = "./assets/ip2region_v6.xdb";
    let data = load_file(path.into())?;
    
    // Search for IPv6 address
    let result = search_ip("2001:0db8:85a3:0000:0000:8a2e:0370:7334", &data)?;
    println!("IPv6 location: {}", result);
    
    Ok(())
}
```

## Examples

### Example 1: Basic IP Lookup

See `examples/search_ip.rs`:

```rust
use std::time::Instant;
use anyhow::Result;
use xdb_parse::utils::{load_file, search_ip};

fn main() -> Result<()> {
    let start = Instant::now();
    let path = "./assets/ip2region_v4.xdb";
    let data = load_file(path.into())?;
    
    let ret = search_ip("73.24.63.66", &data)?;
    let time = start.elapsed();
    
    println!("Search time: {:?} - Result: {}", time, ret);
    Ok(())
}
```

### Example 2: Multi-threaded Usage

The library is designed for thread-safe concurrent usage:

```rust
use std::{sync::Arc, thread};
use xdb_parse::utils::{load_file, search_ip};
use anyhow::Result;

fn main() -> Result<()> {
    let path = "./assets/ip2region_v6.xdb";
    let data = Arc::new(load_file(path.into())?);
    
    let data_clone = Arc::clone(&data);
    let handle = thread::spawn(move || {
        let result = search_ip("2408:8352:da10:1ad:c283:c9ff:fec6:4046", &data_clone).unwrap();
        println!("Thread result: {}", result);
    });
    
    // Main thread can also query
    let result = search_ip("2408:8352:da10:1ad:c283:c9ff:fec6:4046", &data)?;
    println!("Main thread result: {}", result);
    
    handle.join().unwrap();
    Ok(())
}
```

### Example 3: Performance Benchmarking

See `benches/search.rs` for performance testing:

```rust
use criterion::{Criterion, criterion_group, criterion_main};
use xdb_parse::utils::{load_file, search_ip};

fn search_benchmark(c: &mut Criterion) {
    c.bench_function("ipv4_search", |b| {
        let path = "./assets/ip2region_v4.xdb";
        let data = load_file(path.into()).unwrap();
        b.iter(|| {
            search_ip("73.24.63.66", &data).unwrap();
        })
    });
}

criterion_group!(benches, search_benchmark);
criterion_main!(benches);
```

## API Reference

### Core Functions

#### `load_file(path: PathBuf) -> Result<Vec<u8>, XdbError>`

Loads an xdb database file into memory.

**Parameters:**
- `path`: Path to the xdb file

**Returns:** Byte vector containing the database data

#### `search_ip(ip: &str, data: &[u8]) -> Result<String, XdbError>`

Searches for an IP address in the loaded database.

**Parameters:**
- `ip`: IP address to search (supports multiple formats)
- `data`: Database data loaded by `load_file`

**Returns:** Location information as string

### Supported IP Formats

The library accepts IP addresses in multiple formats:

```rust
// Standard IPv4 format
search_ip("192.168.1.1", &data)?;

// Standard IPv6 format  
search_ip("2001:0db8:85a3::0370:7334", &data)?;

// Numeric format (IPv4)
search_ip("3232235777", &data)?; // Equivalent to 192.168.1.1

// Numeric format (IPv6)
search_ip("42540766411282592856903984951653826560", &data)?;
```

## Database File Format

The library supports the xdb format used by ip2region:

- **Header**: 256 bytes containing metadata
- **Vector Index**: 256×256 index blocks for fast lookup
- **Segment Data**: IP range segments with location information

### File Structure

```
┌─────────────────┐
│     Header      │ 256 bytes
├─────────────────┤
│   Vector Index  │ 256×256×8 bytes
├─────────────────┤
│  Segment Data   │ Variable length
└─────────────────┘
```

## Performance

The library uses an optimized binary search algorithm:

- **IPv4 Lookup**: ~5 microseconds per query
- **IPv6 Lookup**: ~250 microseconds per query
- **Memory Usage**: Database loaded once, shared across threads

## Error Handling

The library uses `thiserror` for comprehensive error handling:

```rust
use xdb_parse::error::XdbError;

match search_ip("invalid_ip", &data) {
    Ok(result) => println!("Location: {}", result),
    Err(XdbError::InvalidIP(msg)) => println!("Invalid IP: {}", msg),
    Err(e) => println!("Error: {}", e),
}
```

## Running Examples

To run the provided examples:

```bash
# Run the basic search example
cargo run --example search_ip

# Run benchmarks
cargo bench

# Run tests
cargo test
```

## License

This project is licensed under the same terms as Rust itself.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Acknowledgments

- Inspired by the ip2region project
- Uses efficient binary search algorithms for fast IP lookup
- Designed for high-performance applications
