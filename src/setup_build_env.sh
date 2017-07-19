#
# Usage:
# ln -s packages/debian .
# . ./setup_build_env.sh
# fakeroot debian/rules clean binary
#
# I have multipe guile versions installed and so must do this
# prior to building right now to get a working build.
#

if ! echo $PATH | grep -q ccache; then
    export PATH=/usr/lib/ccache:$PATH
fi

#
# For run-from-source so that .scm can be modified in source directories rather than out in installed system locations, and then a
# restart of texmacs will pick up the changed scm. Modifications to C++ or whatever can be picked up quickly by a rebuild,
# especially with ccache in use, speeding builds tremendously.
#
export TEXMACS_PATH="/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-1.8/src/TeXmacs"
export TEXMACS_BIN_PATH="/home/karlheg/src/TeXmacs/texmacs-git-svn-guile-1.8/src/debian/texmacs/usr/lib/texmacs/TeXmacs"


## export GUILE_CFLAGS="`pkg-config --static --cflags guile-1.8`"
## export GUILE_LDFLAGS="`pkg-config --static --libs guile-1.8`"
## export GUILE_DATA_PATH="`pkg-config --variable=datadir guile-1.8`"
## export GUILE_VERSION="`pkg-config --modversion guile-1.8`"

#export CCACHE_PREFIX=/usr/bin/distcc
#export DISTCC_HOSTS="localhost/8 192.168.1.64/4"
#export DEB_BUILD_OPTIONS=parallel=12

export DEB_BUILD_OPTIONS="parallel=8 nostrip" #noautodbgsym
