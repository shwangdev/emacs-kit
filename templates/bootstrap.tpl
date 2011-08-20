#!/usr/bin/env bash

# Time-stamp: <2010-04-13 13:30:12 Tuesday by ahei>

set -x
aclocal                                         \
&& autoconf                                     \
&& libtoolize --copy --force --automake         \
&& automake --foreign --add-missing
