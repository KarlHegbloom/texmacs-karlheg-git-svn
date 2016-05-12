# Mirror of GNU TeXmacs svn repository #

This repository is a mirror of the GNU TeXmacs svn repository created using `git svn`. The default branch here is called `oaktree`, and the upstream branch is called `svn-trunk`. My branch contains a few changes that I will try and send upstream. It can be used to produce a deb package:

    cd src
    ln -s packages/debian .
    . ./setup_build_env.sh
    dpkg-checkbuildeps
    [ install what you need ]
    fakeroot debian/rules binary

You will need to have git-svn installed for the version number thing to work right.

**NO WARRANTY:** I will put binary build releases here:

[https://github.com/KarlHegbloom/texmacs/releases](https://github.com/KarlHegbloom/texmacs/releases)

*They are development snapshot builds.* I use these binaries myself
and they tend to be reasonably usable. If one is especially broken I
will take care to remove it.


Karl M. Hegbloom <karl.hegbloom@gmail.com>
