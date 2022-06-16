const Benchmark = require('benchmark');

const suite = new Benchmark.Suite();
const searcher = require('../../ip2region').create('../../data/ip2region.db');
const testDatas = require('../utils/testData');
const asyncFor = require('../utils/asyncFor');

suite.add("MemorySearchSync", () => {
    for (let i = 0; i < testDatas.length; ++i) {
        searcher.memorySearchSync(testDatas[i]);
    }
})
    .add("BinarySearchSync", () => {
        for (let i = 0; i < testDatas.length; ++i) {
            searcher.binarySearchSync(testDatas[i]);
        }
    })
    .add("BtreeSearchSync", () => {
        for (let i = 0; i < testDatas.length; ++i) {
            searcher.btreeSearchSync(testDatas[i]);
        }
    })
    .add("MemorySearch", {
        defer: true,
        fn: function (completeCallBack) {
            asyncFor(testDatas,
                (v, c) => {
                    searcher.memorySearch(v, () => {
                        c();
                    });
                },
                () => {
                    completeCallBack.resolve();
                });
        }
    })
    .add("BinarySearch", {
        defer: true,
        fn: function (completeCallBack) {
            asyncFor(testDatas,
                (v, c) => {
                    searcher.binarySearch(v, () => {
                        c();
                    });
                },
                () => {
                    completeCallBack.resolve();
                });
        }
    })
    .add("BtreeSearch", {
        defer: true,
        fn: function (completeCallBack) {
            asyncFor(testDatas,
                (v, c) => {
                    searcher.btreeSearch(v, () => {
                        c();
                    });
                },
                () => {
                    completeCallBack.resolve();
                });
        }
    })
    .on('cycle', function (event) {
        console.log(String(event.target));
    })
    .on('complete', function () {

        let results = new Array();

        for (let prop in this) {
            if (!isNaN(prop)) {
                const eachResult = {
                    name: this[prop].name,
                    mean: this[prop].stats.mean * 1000,  //second => millisecond
                    moe: this[prop].stats.moe,
                    rme: this[prop].stats.rme,
                    sem: this[prop].stats.sem
                }
                results.push(eachResult);
            }
        }

        results = results.sort((a, b) => { return a.mean - b.mean });

        console.log(`Rand\t${'Name'.padEnd(20)}Time (in milliseconds)`);
        let id = 1;

        for (let r of results) {
            console.log(`${id++}\t${r.name.padEnd(20)}${r.mean.toFixed(3)}`);
        }
    })
    .run({ async: true });