# ts-ip2region

TypeScript node client library for IP2Region

## Installation

Install the package with NPM

```bash
npm install ts-ip2region
# or
yarn add ts-ip2region
```

## Usage

```typescript
import { Searcher, CachePolicy, ISearcher } from 'ts-ip2region';

const dbPath = './ip2region.xdb'; 
const searcher: ISearcher = new Searcher(CachePolicy.Content, dbPath);

const ip = '8.8.8.8';
const regionInfo = await searcher.search(ip);
console.log(`IP: ${ip}, Region: ${regionInfo}`);

await searcher.close();
```
### Cache Policy Description
| Cache Policy            | Description                                                                                                | Thread Safe |
|-------------------------|------------------------------------------------------------------------------------------------------------|-------------|
| CachePolicy.Content     | Cache the entire `xdb` data.                                                                               | Yes         |
| CachePolicy.VectorIndex | Cache `vecotorIndex` to speed up queries and reduce system io pressure by reducing one fixed IO operation. | Yes         |
| CachePolicy.File        | Completely file-based queries                                                                              | Yes         |
### XDB File Description
Generate using [maker](https://github.com/lionsoul2014/ip2region/tree/master/maker/csharp), or [download](https://github.com/lionsoul2014/ip2region/blob/master/data/ip2region.xdb) pre-generated xdb files

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[Apache License 2.0](https://github.com/lionsoul2014/ip2region/blob/master/LICENSE.md)