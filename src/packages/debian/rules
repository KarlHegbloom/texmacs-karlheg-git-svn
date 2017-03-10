#!/usr/bin/make -f
# -*- makefile -*-
# Sample debian/rules that uses debhelper.
# This file was originally written by Joey Hess and Craig Small.
# As a special exception, when this file is copied by dh-make into a
# dh-make output file, you may use that output file without restriction.
# This special exception was added by Craig Small in version 0.37 of dh-make.

# Uncomment this to turn on verbose mode.
export DH_VERBOSE := 1

BUILD_DIR := obj-$(shell dpkg-architecture -qDEB_BUILD_GNU_TYPE)

CMAKE_OPTS :=					\
  -DCMAKE_EXPORT_COMPILE_COMMANDS=1		\
  -DCMAKE_INSTALL_PREFIX=/usr			\
  -DCMAKE_VERBOSE_MAKEFILE=ON			\
  -DCMAKE_INSTALL_SYSCONFDIR=/etc		\
  -DCMAKE_INSTALL_LOCALSTATEDIR=/var		\
  -DTEXMACS_GUI=Qt5

# -DCMAKE_BUILD_TYPE=RelWithDebInfo

%: cmake debian/changelog debian/control
	dh --parallel --buildsystem=cmake $@

cmake:
	(mkdir ${BUILD_DIR}; \
	cd ${BUILD_DIR};     \
	cmake ${CMAKE_OPTS} ..)


debian/changelog:
	cp $@.in $@
	touch $@.in

debian/control:
	cp $@.in $@
	touch $@.in


override_dh_update_autotools_config:
	:

override_dh_auto_configure: cmake
	:

override_dh_strip:
	:

.PHONY: cmake
