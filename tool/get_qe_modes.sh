#!/bin/sh

if [ -d ../lisp/qe-modes ]; then
  echo "qe-modes exist under ../lisp, skip"
  exit 1
fi
qemode_ver=7.4
tarball=QE-modes-${qemode_ver}.tar.gz

if [ ! -e $tarball ]; then
  wget https://pwtk.ijs.si/download/${tarball}
fi

tar -zxf $tarball
mv QE-modes-$qemode_ver/qe-modes ../lisp
rm -rf QE-modes-${qemode_ver}
