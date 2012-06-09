#!/bin/bash

# basenames of distribution files
dist_files="nav ack"

dist_dir=emacs-nav-$(grep 'Version:' nav.el | awk '{print $3}')
rm -rf $dist_dir
mkdir -p $dist_dir
for file in $dist_files; do
    emacs -batch -f batch-byte-compile $file.el
    cp $file.el $dist_dir
    mv $file.elc $dist_dir
done
for file in ack README.md LICENSE CHANGES; do
    cp $file $dist_dir
done

tar czvf $dist_dir.tar.gz $dist_dir
rm -rf $dist_dir
