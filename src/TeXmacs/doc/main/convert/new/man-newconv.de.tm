<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Konvertierer und neue Datenformate erstellen>

  Mir der <name|Guile>/<name|Scheme>-Sprache kann man, neue Datenformate und
  Konvertierer zu <TeXmacs> als Module hinzuf�gen. Normalerweise werden die
  zus�tzlichen Formate und Konvertierer in ihrer pers�nlichen
  Initialisierungs-Datei <verbatim|~/.TeXmacs/progs/my-init-texmacs.scm>
  definiert oder in einem speziellen Plugin. Einige Beispiele finden Sie in
  einem Unterverzeichnis <verbatim|progs/convert> ihres
  <TeXmacs>-Verzeichnisses, wie z.B. <hlink|init-html.scm|$TEXMACS_PATH/progs/convert/html/init-html.scm>.

  <paragraph*|neue formate definieren>

  Ein neues Format kann mit dem Befehl

  <\scheme-fragment>
    (define-format <em|format>

    \ \ (:name <em|format-name>)

    \ \ <em|options>)
  </scheme-fragment>

  erzeugt werden. <verbatim|<em|format>> ist ein Symbol, dass f�r das Format
  steht und <verbatim|<em|format-name>> eine Zeichenkette, die in Men�s
  benutzt werden kann. Tats�chlich gibt es ein Datenformat meist in mehreren
  Varianten: ein Format <verbatim|<em|format>-file> f�r Dateien, ein Format
  <verbatim|<em|format>-document> f�r ganze Dokumente und ein Format
  <verbatim|<em|format>-snippet> f�r kurze Zeichenketten, wie z.B. Auswahlen
  (selections), und schlie�lich <verbatim|<em|format>-object> f�r die
  bevorzugte interne <name|Scheme>-Repr�sentation bei Konversionen (dies ist
  Parser-Variante des Formats). Konvertierer von <verbatim|<em|format>-file>
  nach <verbatim|<em|format>-document> und umgekehrt werden automatisch
  erzeugt.

  Der Anwender kann zus�tzliche Optionen zur automatischen Erkennung von
  Formaten mit Hilfe von Datei-Suffixen oder -Inhalten spezifizieren. Z.B.
  k�nnen die erlaubten Suffixe eines Daten-Formats mit der Voreinstellung als
  erstem durch

  <\scheme-fragment>
    (:suffix <em|default-suffix> <em|other-suffix-1> ... <em|other-suffix-n>)
  </scheme-fragment>

  angegeben werden. Eine (heuristische) Routine, um festzustellen, ob ein
  Dokument zu einem bestimmten Format geh�rt, kann mit

  <\scheme-fragment>
    (:recognize <em|predicate>)

    (:must-recognize <em|predicate>)
  </scheme-fragment>

  erreicht werden. Im ersten Fall hat die Suffix-Erkennung Vorrang vor der
  heuristischen Erkennung. Im zweiten Fall ist nur die heuristische Erkennung
  durch das Pr�dikat, predicate, ma�geblich.

  <paragraph*|neue konvertierer erzeugen>

  Neue Konvertierer k�nnen mit

  <\scheme-fragment>
    (converter <em|from> <em|to>

    \ \ <em|options>)
  </scheme-fragment>

  erzeugt werden. Der eigentliche Konvertierer wird durch eine der folgenden
  Optionen spezifiziert:

  <\scheme-fragment>
    (:function <em|converter>)

    (:function-with-options <em|converter-with-options>)

    (:shell <em|prog> <em|prog-pre-args> from <em|progs-infix-args> to
    <em|prog-post-args>)
  </scheme-fragment>

  Im ersten Fall ist der Konvertierer <verbatim|<em|converter>> eine Routine
  die ein Objekt im Daten-Format <verbatim|<em|from>> \ �bernimmt und ein
  Objekt im Daten-Format <verbatim|<em|to>> zur�ckgibt. Im zweiten Fall
  �bernimmt der <verbatim|<em|converter>> eine assoziative Liste als zweites
  Argument und Optionen f�r den Konvertierer. Im letzten Fall wird ein
  <em|shell>-Befehl angegeben, der die Konvertierung zwischen den beiden
  Datei-Formaten durchf�hrt. Der Konvertierer wird dann und nur dann
  aktiviert, wenn das Programm <verbatim|<em|prog>> gefunden wird.
  Hilfsdateien k�nnen automatisch erzeugt und wieder gel�scht werden.

  <TeXmacs> berechnet automatisch die transitive \RClosure'' aller
  Konvertierer, indem einen \Rk�rzesten Pfad Algorithmus'' benutzt. Mit
  anderen Worten, wenn es einen Konvertierer f�r <with|mode|math|x> nach
  <with|mode|math|y> gibt und einen Konvertierer von <with|mode|math|y> nach
  <with|mode|math|z>, dann hat man auch einen von <with|mode|math|x> nach
  <with|mode|math|z>. Es f�r jede Konvertierung kann eine \RStrafe/Abstand
  zwischen den Formaten`` mit

  <\scheme-fragment>
    (:penalty <em|floating-point-distance>)
  </scheme-fragment>

  angeben werden, um so Hinweise zum automatischen Finden eines optimalen
  Weges f�r die Konvertierung zu geben.

  Weitere Optionen f�r Konvertierer sind:

  <\scheme-fragment>
    (:require <em|cond>)

    (:option <em|option> <em|default-value>)
  </scheme-fragment>

  Die erste Option spezifiziert eine Bedingung, die erf�llt sein muss, damit
  der Konvertierer benutzt werden kann. Diese Option sollte als erste oder
  zweite Option spezifiziert werden und immer nach der <verbatim|:penalty>
  Option. Die <verbatim|:option> Option spezifiziert eine Option f�r den
  Konvertierer, mit einer Voreinstellung. Diese Option wird automatisch an
  alle Konvertierer mit Optionen weitergegeben.

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