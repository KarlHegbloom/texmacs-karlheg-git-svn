<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Das Plugin in die <TeXmacs>-Distribution einbinden>

  Nehmen wir einmal an, dass Sie erfolgreich, wie im vorgehenden Abschnitt
  beschrieben, \ eine Schnittstelle zu <TeXmacs> in einem Anwenderprogramm
  angelegt haben. Dann wird es Zeit die Unterst�tzung f�r das Programm in die
  Standard-<TeXmacs>-Distribution zu �bertragen. Weitere Verbesserungen
  bleiben danach m�glich.

  Seit der <TeXmacs>Version 1.0.1.5 ist es sehr leicht, eine Schnittstelle so
  anzupassen, dass Sie direkt in <TeXmacs> integriert wird. Sie brauchen nur
  ein Verzeichnis zu erzeugen, das den Namen Ihres Plugins,
  <verbatim|myplugin>, tr�gt:\ 

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim>

  <verbatim|$TEXMACS_HOME_PATH> ist vorgabem��ig <verbatim|~/.TeXmacs>. In
  einem Unter-Verzeichnis, <verbatim|plugins>, Ihres <TeXmacs>-Verzeichnisses
  k�nnen Sie alle Standard Plugins finden, die mit Ihrer
  <TeXmacs>-Distribution. ausgeliefert werden. Diese bieten gute Beispiele
  zur Nach�ffung.

  Das <verbatim|myplugin>-Verzeichnis sollte eine �hnliche
  Unterverzeichnis-Struktur aufweisen, wie Ihr <TeXmacs>-Verzeichnis.
  Allerdings sollte es nur die Verzeichnisse enthalten, die Sie wirklich
  brauchen. In jedem Fall brauchen Sie aber eine Datei
  <verbatim|progs/init-myplugin.scm>, die definiert, wie Ihr Plugin zu
  initialisieren ist. Normalerweise ist dies ein \ <name|Scheme>-Befehl der
  folgenden Form:\ 

  <\verbatim>
    \ \ \ \ (plugin-configure myplugin<next-line> \ \ \ \ \ (:require
    (file-in-path "myplugin"))<next-line> \ \ \ \ \ (:launch
    "shell-cmd")<next-line> \ \ \ \ \ (:format "input-format"
    "output-format")<next-line> \ \ \ \ \ (:session "Myplugin"))
  </verbatim>

  Der erste Teil des Befehls ist ein Pr�dikat, dass �berpr�ft, ob das Plugin
  auf diesen bestimmten System �berhaupt verwendet werden kann. Meist
  �berpr�ft es, ob ein bestimmtes Programm in einem bestimmten Pfad vorhanden
  ist. Nur dann, wenn dies erf�llt ist, wird der zweite Teil ausgef�hrt. Der
  Befehl <verbatim|:launch> bedeutet hier, dass das Plugin mit
  <verbatim|shell-cmd>. The command <verbatim|shell-cmd> aus Befehl der
  System-Umgebung gestartet werden soll: meist in der Form:
  <verbatim|myplugin --texmacs>. Der <verbatim|:format>-Befehl erkl�rt ,
  welche Formate f�r Eingabe und Ausgabe benutzt werden sollen. Normalerweise
  ist das Eingabe-Format <verbatim|verbatim>, <localize|verbatim>, und das
  Ausgabe-Format <verbatim|<localize|generic>> . Andere m�gliche Formate sind
  <verbatim|scheme>, <verbatim|latex>, <verbatim|html> und <verbatim|ps>. Die
  Option <verbatim|:session> macht nimmt den Befehl in das Men�
  <menu|Insert|Session|Myplugin> auf.

  Wenn alles gut arbeitet und Sie Ihr System anderen innerhalb der
  offiziellen <TeXmacs>-Distribution zur Verf�gung stellen wollen, dann
  kontaktieren Sie mich unter <verbatim|vdhoeven@texmacs.org>.

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