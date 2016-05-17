# Mirror of GNU TeXmacs svn repository #

I apologize for the difficulties I may have caused by having used `git rebase` on my syrinx branch. I did not yet understand what kind of difficulties that can cause for people. Now I do. From now on, here in my local repository at home, I will:

    git stash                  # if needed
    git checkout svn-trunk
    git svn rebase
    git push github svn-trunk
    git checkout syrinx
    git merge svn-trunk
    git pull -r github syrinx 
    git push github syrinx
    git stash pop              # if needed
    
That will ensure that the `svn-trunk` branch is a straight mirror of the TeXmacs `svn` repository, and that I'm not continually rebasing my own working branch, but instead merging the new things from the upstream "vendor tracking" branch on top of my own changes, so that a clone of my branch will function as expected. I'm tempted to try and fix-up the `syrinx` branch one time, just to put the commits into the correct chronological order. I suppose that if I do that, I'll need to create a new branch, and switch to it as the default branch. If I do this, I'll leave notice of it here.

This repository is a mirror of the GNU TeXmacs svn repository created using `git svn`. The default branch here on GitHub is called `syrinx`, and the upstream branch is called `svn-trunk`. My branch contains a few changes that I will try and send upstream. It can be used to produce a deb package:

    git clone https://github.com/KarlHegbloom/texmacs.git
    cd texmacs
    git checkout syrinx
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
