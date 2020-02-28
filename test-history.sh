#!/bin/bash -e

mkdir tmp
cd tmp

git clone git@github.com:1ml-prime/1ml.git
cd 1ml

while [ -e ./travis.sh ]; do
  ./travis.sh
  git reset --hard HEAD~1
done

cd ../..
rm -rf tmp
