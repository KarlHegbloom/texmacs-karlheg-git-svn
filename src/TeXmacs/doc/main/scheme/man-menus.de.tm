<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Eigene dynamische Men�s schreiben>

  Sie Men�s oder Teile eines Men�s selbst erstellen oder �ndern, indem Sie
  f�r das Men� mit Namen <verbatim|name> \ den Befehl

  <\verbatim>
    \ \ \ \ (menu-bind name . prog)
  </verbatim>

  benutzen. Sie k�nnen neue Eintr�ge an ein existierendes Men� mit dem Namen
  <verbatim|name> mit dem Befehl

  <\verbatim>
    \ \ \ \ (menu-extend name . prog)
  </verbatim>

  anh�ngen. <verbatim|prog> ist hier ein Programm, das von dem Men�eintrag
  repr�sentiert wird. Sehen Sie sich die Dateien im Verzeichnis

  <\verbatim>
    \ \ \ \ progs/menu
  </verbatim>

  um zu sehen, wie die Standard <TeXmacs>-Men�s definiert sind.

  Genauer gesagt, ist das Programm <verbatim|prog> in <verbatim|menu-set>
  oder <verbatim|menu-append> eine Liste von Eintr�gen einer der folgenden
  Formen:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<next-line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<next-line>
    \ \ \ ("entry" action)<next-line> \ \ \ ---<next-line> \ \ \ (if
    condition menu-definition)<next-line> \ \ \ (link variable)
  </verbatim>

  Die Konstruktoren <verbatim|=\<gtr\>> und <verbatim|-\<gtr\>> werden
  benutzt, um Pulldown- bzw. Pullright-Men�s zu erzeugen.
  <verbatim|menu-definition> enth�lt ein Programm, das ein Unter-Men�
  erstellt. Der Konstruktor <verbatim|("entry" action)> erzeugt einen
  normalen Eintrag, der <verbatim|action> kompiliert und ausf�hrt, wenn man
  auf den Eintrag <verbatim|entry> klickt. Die Eintr�ge k�nnen mit
  <verbatim|---> getrennt werden. Der Konstruktor <verbatim|if> wird benutzt,
  wenn ein Eintrag nur erfolgen soll, wenn eine bestimmte Bedingung
  <verbatim|condition> erf�llt ist, z.B. dass man im mathematischen Modus
  ist.

  Wenn sie ein Men� mit Namen <verbatim|name> definiert haben, k�nnen Sie
  dieses Men� indirekt mit dem <verbatim|link> Konstruktor verwenden. Das hat
  f�r Untermen�s zwei Vorteile:

  <\itemize>
    <item>Ein \Rindirektes'' Untermen� kann zu beliebig vielen Men�s gelinkt
    werden

    <item>Neue Eintr�ge k�nnen <with|font-shape|italic|a posteriori>
    \Rindirekten`` Men�s zugef�gt werden, indem man <verbatim|menu-append>
    benutzt.
  </itemize>

  Die Haupt-<TeXmacs>-Men�s sind: \ <verbatim|texmacs-menu>,
  <verbatim|texmacs-popup-menu>, <verbatim|texmacs-main-icons>,
  <verbatim|texmacs-context-icons> und <verbatim|texmacs-extra-icons>. Andere
  indirekte Standard-Men�s sind: <verbatim|file-menu>, <verbatim|edit-menu>,
  <verbatim|insert-menu>, <verbatim|text-menu>, <verbatim|paragraph-menu>,
  <verbatim|document-menu>, <verbatim|options-menu> und <verbatim|help-menu>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
  </collection>
</initial>