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
    PATH=/usr/lib/ccache:$PATH
    export PATH
fi

export GUILE_CFLAGS="`pkg-config --static --cflags guile-1.8`"
export GUILE_LDFLAGS="`pkg-config --static --libs guile-1.8`"
export GUILE_DATA_PATH="`pkg-config --variable=datadir guile-1.8`"
export GUILE_VERSION="`pkg-config --modversion guile-1.8`"

# export CCACHE_PREFIX=/usr/bin/distcc
# export DISTCC_HOSTS="localhost/8 192.168.1.64/4"
# export DEB_BUILD_OPTIONS=parallel=12

export DEB_BUILD_OPTIONS="parallel=8 nostrip noautodbgsym"
