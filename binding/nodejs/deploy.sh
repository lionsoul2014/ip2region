#!/bin/sh
echo 'create dir'
if [ ! -d "data" ]; then
  mkdir data
fi

echo "copy db"
cp '../../data/ip2region.db' './data/'

echo "npm publish"
npm publish
