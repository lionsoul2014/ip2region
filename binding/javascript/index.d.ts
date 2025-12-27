// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.

// Type definitions for ip2region
// @Author Lion <chenxin619315@gmail.com>

export declare class Header {
    version: number;
    indexPolicy: number;
    createdAt: number;
    startIndexPtr: number;
    endIndexPtr: number;
    ipVersion: number;
    runtimePtrBytes: number;
    buff: Buffer;
    
    constructor(buff: Buffer);
    toString(): string;
}

export declare class Version {
    id: number;
    name: string;
    bytes: number;
    indexSize: number;
    ipCompareFunc: (ip1: Buffer, ip2: Buffer, offset: number) => number;
    
    constructor(id: number, name: string, bytes: number, indexSize: number, ipCompareFunc: (ip1: Buffer, ip2: Buffer, offset: number) => number);
    ipCompare(ip1: Buffer, ip2: Buffer): number;
    ipSubCompare(ip1: Buffer, ip2: Buffer, offset: number): number;
    toString(): string;
}

export declare class Searcher {
    ioCount: number;
    version: Version;
    handle: number | null;
    vectorIndex: Buffer | null;
    cBuffer: Buffer | null;
    
    constructor(version: Version, dbPath: string | null, vectorIndex: Buffer | null, cBuffer: Buffer | null);
    getIPVersion(): Version;
    getIOCount(): number;
    search(ip: string | Buffer): string;
    read(offset: number, buff: Buffer, stats?: any): void;
    close(): void;
}

// Constants
export declare const XdbStructure20: 2;
export declare const XdbStructure30: 3;
export declare const XdbIPv4Id: 4;
export declare const XdbIPv6Id: 6;
export declare const HeaderInfoLength: 256;
export declare const VectorIndexRows: 256;
export declare const VectorIndexCols: 256;
export declare const VectorIndexSize: 8;

// Version instances
export declare const IPv4: Version;
export declare const IPv6: Version;

// Utility functions
export declare function parseIP(ipString: string): Buffer;
export declare function ipToString(ipBytes: Buffer, compress?: boolean): string;
export declare function ipBytesString(ipBytes: Buffer): string;
export declare function ipSubCompare(ip1: Buffer, buff: Buffer, offset: number): number;
export declare function ipCompare(ip1: Buffer, ip2: Buffer): number;
export declare function versionFromName(name: string): Version | null;
export declare function versionFromHeader(h: Header): Version | null;

// File operations
export declare function loadHeader(fd: number): Header;
export declare function loadHeaderFromFile(dbPath: string): Header;
export declare function loadVectorIndex(fd: number): Buffer;
export declare function loadVectorIndexFromFile(dbPath: string): Buffer;
export declare function loadContent(fd: number): Buffer;
export declare function loadContentFromFile(dbPath: string): Buffer;

// Searcher factory functions
export declare function newWithFileOnly(version: Version, dbPath: string): Searcher;
export declare function newWithVectorIndex(version: Version, dbPath: string, vectorIndex: Buffer): Searcher;
export declare function newWithBuffer(version: Version, cBuffer: Buffer): Searcher;