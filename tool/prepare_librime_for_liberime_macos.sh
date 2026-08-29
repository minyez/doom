#!/bin/sh
# Install librime with Homebrew and build liberime
set -eu

liberime_dir=${liberime_dir:=$HOME/.config/emacs/.local/straight/repos/liberime}

command -v brew >/dev/null 2>&1 || {
  echo "Homebrew is required to install librime" >&2
  exit 1
}
brew list --versions librime >/dev/null 2>&1 || brew install librime
librime_install_dir=$(brew --prefix librime)

cd "$liberime_dir" || { echo "fail to open $liberime_dir" && exit 1; }
make -B \
  "RIME_CFLAGS=-I$librime_install_dir/include" \
  "RIME_LDFLAGS=-L$librime_install_dir/lib -Wl,-rpath,$librime_install_dir/lib -lrime"

# check if liberime is workable
emacs -Q -batch -eval "(add-to-list 'load-path \"$(pwd)\")" -load liberime.el \
  -eval "(if (liberime-workable-p) (message \"liberime workable\") (message \"liberime not workable\"))"
