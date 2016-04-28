# Mirror of GNU TeXmacs svn repository #

This repository is a mirror of the GNU TeXmacs svn repository created using `git svn`. The default branch here is called `oaktree`, and the upstream branch is called `svn-trunk`. My branch contains a few changes that I will try and send upstream. It can be used to produce a deb package:

    cd src
    ln -s packages/debian .
    . ./setup_build_env.sh
    dpkg-checkbuildeps
    [ install what you need ]
    fakeroot debian/rules binary

You will need to have git-svn installed for the version number thing to work right.

Karl M. Hegbloom <karl.hegbloom@gmail.com>
