<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Makros und <TeXmacs>-Umgebungsvariable>

  Die wichtigsten Tastenkombinationen, die Sie kennen sollten, um
  Stildefinitionen selbst zu schreiben, sind die folgenden:

  <\description>
    <item*|<kbd-ia|=>>erzeugt eine neue Befehlsanweisung. Das erste Argument
    ist ein neuer Befehlsname und das zweite Argument ein Ausdruck.

    <item*|<kbd-ia|w>>erzeugt eine \RWith-Anweisung``. Diese erlaubt
    <TeXmacs>-Umgebungsvariablen lokal zu �ndern. Sie haben die Form
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>.
    Dabei sind die <with|mode|math|x<rsub|i>> die Namen der Variablen,
    <with|mode|math|a<rsub|i>> die lokalen Werte und b ein Text, auf den die
    Variablen angewendet werden.

    <item*|<kbd-ia|m>>erzeugt ein Makro. Argumente k�nnen mit der
    \ <key|Tab>-Taste eingef�gt werden.

    <item*|<kbd-ia|#>>hole den Wert eines Makroarguments.

    <item*|<kbd-ia|v>>hole den Wert einer <TeXmacs>-Umgebungsvariablen.

    <item*|<kbd-ia|e>>expandiert ein Makro mit keinem oder mehreren
    Argumenten.
  </description>

  Genauer, wenn ein mit <kbd-ia|e> expandiertes Makro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> ausgewertet wird,
  laufen folgende Vorg�nge ab:

  <\itemize>
    <item>Wen <with|mode|math|a> weder eine Zeichenkette noch ein Makro ist,
    wird a genau einmal evaluiert. Das Ergebnis ist entweder ein Makroname
    <with|mode|math|f> oder ein Makroausdruck <with|mode|math|f>.

    <item>Wenn es ein Makroname ist, wird <with|mode|math|f> durch den Wert
    der <TeXmacs>-Umgebungsvariablen <with|mode|math|f> ersetzt. Wenn danach
    <with|mode|math|f> immer noch kein Makroausdruck ist, wird
    <with|mode|math|f> zur�ckgegeben.

    <item>Wenn <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> die Argumente
    von <with|mode|math|f> sind und <with|mode|math|b> der Rumpf
    (�berfl�ssige Argumente werden ignoriert; fehlende Argumente erhalten den
    Vorgabewert, die leere Zeichenkette). Dann werden die
    <with|mode|math|y<rsub|i>> in <with|mode|math|b> durch die
    <with|mode|math|x<rsub|i>> ersetzt und das Resultat zur�ckgegeben.
  </itemize>

  Funktionen verhalten sich �hnlich wie Makros, nur sind die Argumente von
  Funktionen evaluiert und k�nnen nicht direkt ge�ndert werden. Man mu�
  zuerst die Funktion deaktivieren, dann die Argumente �ndern und schlie�lich
  wieder aktivieren. Au�erdem werden <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>>
  als lokale Variablen mit den Werten <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>
  betrachtet. Diese lokalen Variablen gehen verloren, wenn die Funktion, die
  sie benutzt, verlassen wird.

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