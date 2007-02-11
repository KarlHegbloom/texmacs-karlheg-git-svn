Summary: A structured wysiwyg scientific text editor
Name: TeXmacs
Version: 1.0.6.9
Release: 1
Url: http://www.texmacs.org
Source: ftp://ftp.texmacs.org/pub/TeXmacs/TeXmacs-1.0.6.9-src.tar.gz
License: GNU GPL 2.0
Packager: Joris van der Hoeven <vdhoeven@texmacs.org>
Distribution: GNU/Linux
Vendor: Jo the ripper software
Group: Applications/Editors
Requires: tetex
Buildrequires: guile-devel
BuildRoot: %{_tmppath}/TeXmacs-1.0.6.9-root

%description

GNU TeXmacs is a free wysiwyw (what you see is what you want) editing
platform with special features for scientists. The software aims to provide
a unified and user friendly framework for editing structured documents with
different types of content (text, graphics, mathematics, interactive content,
etc.). The rendering engine uses high-quality typesetting algorithms so as to
produce professionally looking documents, which can either be printed out
or presented from a laptop.

The software includes a text editor with support for mathematical formulas,
a small technical picture editor and a tool for making presentations from
a laptop. Moreover, TeXmacs can be used as an interface for many external
systems for computer algebra, numerical analysis, statistics, etc.
New presentation styles can be written by the user and new features can be
added to the editor using the Scheme extension language. A native spreadsheet
and tools for collaborative authoring are planned for later.

TeXmacs runs on all major Unix platforms and Windows. Documents can be
saved in TeXmacs, Xml or Scheme format and printed as Postscript or
Pdf files. Converters exist for TeX/LaTeX and Html/Mathml.

%prep
%setup -q -n TeXmacs-1.0.6.9-src

%build
%configure
make STATIC_TEXMACS

%install
make DESTDIR=$RPM_BUILD_ROOT install
export GUILE_DATA_PATH=`guile-config info pkgdatadir`
export GUILE_LOAD_PATH=`find $GUILE_DATA_PATH -type d | grep ice-9`
cp -r -f $GUILE_LOAD_PATH $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs
chmod -f 644 $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs/ice-9/*
chmod -f 755 $RPM_BUILD_ROOT%{_datadir}/TeXmacs/progs/ice-9
mkdir -p $RPM_BUILD_ROOT/etc/X11/applnk/Applications
mkdir -p $RPM_BUILD_ROOT/usr/share/application-registry
mkdir -p $RPM_BUILD_ROOT/usr/share/mime-info
mkdir -p $RPM_BUILD_ROOT/usr/share/pixmaps
cp -f $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.desktop $RPM_BUILD_ROOT/etc/X11/applnk/Applications
cp -f $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.applications $RPM_BUILD_ROOT/usr/share/application-registry
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.keys $RPM_BUILD_ROOT/usr/share/mime-info
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/mime/texmacs.mime $RPM_BUILD_ROOT/usr/share/mime-info
cp $RPM_BUILD_ROOT%{_datadir}/TeXmacs/misc/pixmaps/TeXmacs.xpm $RPM_BUILD_ROOT/usr/share/pixmaps

%files
%{_bindir}/fig2ps
%{_bindir}/texmacs
%{_includedir}/TeXmacs.h
%doc %{_mandir}/*/*
%{_libexecdir}/TeXmacs
%{_datadir}/TeXmacs
/etc/X11/applnk/Applications/texmacs.desktop
/usr/share/application-registry/texmacs.applications
/usr/share/mime-info/texmacs.keys
/usr/share/mime-info/texmacs.mime
/usr/share/pixmaps/TeXmacs.xpm

%clean
rm -rf $RPM_BUILD_ROOT

%changelog

* Sun Oct 09 2005   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.5.10 Changed description

* Thu Aug 30 2004   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.4.2 correct name of tarball for release>1

* Thu Jan 22 2004   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.3.2 change VERSION -> DEVEL_VERSION, RELEASE -> DEVEL_RELEASE

* Mon Sep 22 2003   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.1.24 Release tag set by the configure script

* Sat Jun 28 2003   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.1.16 Moved "TeXmacs-%{version}" in root directory to "TeXmacs"

* Tue Jul 22 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 1.0.0.10 Migrated architecture dependent options to configure.in

* Thu Feb 07 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.14 Added -f for chmod

* Thu Feb 07 2002   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.11 Extra compilation options reduce size of executable

* Sun Nov 26 2001   Joris van der Hoeven <vdhoeven@texmacs.org>
- 0.3.5.7 TeXmacs mime types & pixmap

* Thu Jul 5 2001   Joris van der Hoeven & Christophe Merlet
- 0.3.4.3 Further corrections

* Sun Jun 3 2001   Bo Forslund  <bo.forslund@abc.se>
- 0.3.4.3 Some tiny adjustments in the spec
