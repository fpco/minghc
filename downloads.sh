#!/bin/bash -ex

if [ ! -f cabal-1.20.0.3-i386-unknown-mingw32.tar.gz ]
then
    wget https://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-mingw32.tar.gz
fi

rm -rf cabal-1.20
tar zxfv cabal-1.20.0.3-i386-unknown-mingw32.tar.gz
mkdir -p cabal-1.20/bin
mv cabal.exe cabal-1.20/bin

if [ ! -f ghc-7.8.3-i386-unknown-mingw32.tar.xz ]
then
    wget https://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-i386-unknown-mingw32.tar.xz
fi
rm -rf ghc-7.8.3
tar xf ghc-7.8.3-i386-unknown-mingw32.tar.xz

rm -rf msys-1.0
unzip msys-1.0.zip
