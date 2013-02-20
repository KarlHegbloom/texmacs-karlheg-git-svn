<TeXmacs|1.0.7.18>

<style|tmweb>

<\body>
  <tmweb-current|Download|Windows><tmweb-title|Installing <TeXmacs> under
  <name|Windows>|<tmweb-download-links>>

  <section|Standard installation>

  In order to install the <name|Qt>-based version of <TeXmacs> on Windows,
  you should:

  <\enumerate>
    <item>Download <hlink|<verbatim|<merge|<TeXmacs-version-release|devel>|-installer.exe>>|<merge|http://www.texmacs.org/Download/ftp/tmftp/windows/|<TeXmacs-version-release|devel>|-installer.exe>>.

    <item>Execute the downloaded installer and follow the installation
    instructions.

    <item>Click on the <TeXmacs> icon on your desktop in order to launch
    <TeXmacs>.
  </enumerate>

  If you like the program, then please consider
  <hlink|donating|../contribute/donations.en.tm> money or services to us. Of
  course, you may also <hlink|contribute|../contribute/contribute.en.tm>
  yourself. In case of problems, please <hlink|subscribe|../home/ml.en.tm> to
  the <verbatim|texmacs-dev> or <verbatim|texmacs-users> mailing lists and
  ask your questions there. You may also directly
  <hlink|contact|../contact/contact.en.tm> us, but you might need to be more
  patient.

  <section|Notes>

  <\itemize>
    <item>If you are using <hlink|<name|Cygwin>|http://www.cygwin.com/>, then
    you may run <verbatim|setup.exe>, select the <verbatim|texmacs> package
    and install it.

    <item>From version 1.0.7.5 on, <TeXmacs> is completely independent from
    <TeX>/<LaTeX>. The last remaining dependency on <name|Bib><TeX> has been
    removed in this version: you may use <verbatim|tm-plain>,
    <verbatim|tm-alpha>, <abbr|etc.> for your bibliography styles. The native
    bibliography support is still a bit unstable though; it will be further
    improved in upcoming versions.

    <item>If you installed <hlink|<name|Mik><TeX>|http://www.miktex.org/> in
    addition to <TeXmacs>, then, due to the availability of some additional
    font templates, certain fonts may be generated automatically. This
    results in a<nbsp>significant slow-down of <TeXmacs>. Don't panic: the
    fonts have to be generated only once. Subsequent runs of <TeXmacs> will
    be fast.

    <item>The current <name|Windows> version of <TeXmacs> only supports a
    reduced set of plug-ins. As of version 1.0.7.18, <name|Maxima>,
    <name|Mathemagix>, <name|Octave>, <name|Pari> and <name|Python> should
    run out of the box after a standard installation.
  </itemize>

  <tmdoc-copyright|1999--2013|Joris van der Hoeven|David Michel|Denis Raux>

  <tmweb-license>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>