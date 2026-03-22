#!/bin/sh
# Build liberime with librime from the package manager on Linux
# Tested for Doom Emacs, liberime version 0.0.6
librime_install_dir=/usr/lib64
EMACSVER=$(emacs --version | awk 'FNR == 1 {print $3}')
liberime_dir=${liberime_dir:=$HOME/.config/emacs/.local/straight/build-$EMACSVER/liberime}

SED=sed

cd "$liberime_dir" || { echo "fail to open $liberime_dir" && exit 1; }
if [ -f src/liberime-core.so ]; then
  echo "src/liberime-core.so is already built"
else
  RIME_PATH="$librime_install_dir" emacs -Q -batch -eval "(add-to-list 'load-path \"$(pwd)\")" -load liberime.el -f liberime-build
  # remove the Release library dir in rpath to avoid run time error
  # this might be redundant if one builds librime from source
  $SED -i "s|:/opt/librime/build/lib/Release\(/\)\{0,1\}||g" Makefile-liberime-build
  make -f Makefile-liberime-build
fi

# check if liberime is workable
emacs -Q -batch -eval "(add-to-list 'load-path \"$(pwd)\")" -load liberime.el \
  -eval "(if (liberime-workable-p) (message \"liberime workable\") (message \"liberime not workable\"))"
