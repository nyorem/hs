#! /usr/bin/env bash
# Clean Haskell executables and objects

rm *.hi *.o
find . -type f -perm +111 -delete

