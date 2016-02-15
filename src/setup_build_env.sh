export GUILE_CFLAGS="`pkg-config --static --cflags guile-1.8`"
export GUILE_LDFLAGS="`pkg-config --static --libs guile-1.8`"
export GUILE_DATA_PATH="`pkg-config --variable=datadir guile-1.8`"
export GUILE_VERSION="`pkg-config --modversion guile-1.8`"

export CCACHE_PREFIX=/usr/bin/distcc
export DISTCC_HOSTS="localhost/8 192.168.1.64/4"
export DEB_BUILD_OPTIONS=parallel=12
