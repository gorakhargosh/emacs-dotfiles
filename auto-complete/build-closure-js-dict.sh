#!/bin/sh
#
# Copyright 2012 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: yesudeep@google.com (Yesudeep Mangalapilly)

if [ -e /tmp/closure-library ]; then
    pushd /tmp/closure-library
    git pull
else
    git clone git://github.com/jarib/google-closure-library.git /tmp/closure-library
    pushd /tmp/closure-library
fi

cat ~/.emacs.d/vendor/auto-complete/dict/javascript-mode > ~/.emacs.d/auto-complete/dict/javascript-mode
git grep -h     -e "^goog\.[A-Za-z_0-9.]*.*\=[ ]" --and --not -e ".*prototype.*" --and --not -e "^goog\.[A-za-z_0-9.]*_.*\=[ ]" | sed  "s/^\(goog\.[A-Za-z_0-9.]*.*\)=.*/\1/g" | sort -u >> ~/.emacs.d/auto-complete/dict/javascript-mode

popd
