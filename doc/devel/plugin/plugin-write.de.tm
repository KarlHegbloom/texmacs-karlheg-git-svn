<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Eigene Plugins schreiben>

  Um ein Plugin <verbatim|<em|myplugin>> zuschreiben, sollten Sie zuerst ein
  Verzeichnis erstellen

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>
  </verbatim>

  das Sie alle notwendigen Dateien enthalten wird. Denken Sie bitte daran,
  dass der <verbatim|$TEXMACS_HOME_PATH> gem�� Vorgabe
  <verbatim|$HOME/.TeXmacs> ist. Zus�tzlich k�nnen Sie die folgenden
  Verzeichnisse erzeugen, falls Sie sie brauchen sollten:

  <\description-dash>
    <item*|<verbatim|bin>>F�r Bin�rdateien.

    <item*|<verbatim|doc>>F�r die Dokumentation (noch nicht unterst�tzt).

    <item*|<verbatim|langs>>F�r Sprach-Dateien wie z.B. W�rterb�cher (noch
    nicht unterst�tzt).

    <item*|<verbatim|lib>>F�r Bibliotheken.

    <item*|<verbatim|packages>>F�r Stil-Pakete.

    <item*|<verbatim|progs>>F�r <value|scheme>-Programme.

    <item*|<verbatim|src>>F�r Quellcode.

    <item*|<verbatim|styles>>F�r Stil-Definitionen.
  </description-dash>

  Generell gilt, Dateien, die sich in diesen Verzeichnissen befinden, werden
  automatisch erkannt, wenn <TeXmacs> startet. Wenn z.B. ein <verbatim|bin>
  Unterverzeichnis existiert, dann wird

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/bin
  </verbatim>

  automatisch zu der <verbatim|PATH>-Kontext-Variablen hinzugef�gt. Beachten
  Sie, dass die Verzeichnisstruktur eines Plugins derjenigen von
  <verbatim|$TEXMACS_PATH> �hnelt.

  <\example>
    Der simpelste Typ von Plugins besteht nur aus Daten-Dateien, wie z.B.
    einer Sammlung von Stil-Definitionen und Stil-Paketen. Dazu gen�gt es,
    die Verzeichnisse

    <\verbatim>
      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/styles

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/packages
    </verbatim>

    herzustellen und die Dateien in die entsprechenden Verzeichnisse zu
    kopieren. Danach werden diese automatisch nach einem Neu-Start von
    <TeXmacs>, in den Men�s <menu|Document|Style> bzw. <menu|Document|Use
    package> erscheinen.
  </example>

  Komplexere Plugins, wie z.B. Plugins mit zus�tzlichem <value|scheme> oder
  <value|cpp> Code muss man meist noch eine
  <value|scheme>-Konfigurations-Datei erstellen

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/progs/init-<em|myplugin>.scm
  </verbatim>

  Diese Konfigurations-Datei sollte eine Anweisung der folgenden Form

  <\scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scheme-fragment>

  enthalten. Darin beschreiben <verbatim|<em|configuration-options>> die
  T�tigkeiten, die beim Start durchzuf�hren sind, einschlie�lich der Frage,
  ob der Code in Ordnung ist. In den nachfolgenden Abschnitten werden wir
  anhand von einfachen Beispielen dir Arbeitsweise und die Programmierung von
  Plugins erl�utern. Viele weitere Beispiele finden Sie unter

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins

    \ \ \ \ $TEXMACS_PATH/plugins
  </verbatim>

  Einige werden eingehender im Kapitel �ber
  <hyper-link|Schnittstellen|../interface/interface.de.tm> beschrieben.

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
    <associate|preamble|false>
  </collection>
</initial>