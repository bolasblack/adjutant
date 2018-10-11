#!/bin/bash

[ ! -d node_modules ] && exit

LIBRARYIES=`cat <<EOF
lodash.startcase
lodash.kebabcase
lodash.lowerfirst
lodash.upperfirst
EOF`

VENDOR_PATH=src/js-vendors

cd $(git rev-parse --show-toplevel)
mkdir -p "$VENDOR_PATH"
for L in $LIBRARYIES; do
  rm -rf "$VENDOR_PATH/$L"
  cp -r "node_modules/$L" "$VENDOR_PATH/$L"
done
