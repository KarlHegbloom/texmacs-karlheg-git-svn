<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Tabellen-Layout>

  Die Kontextvariablen f�r Tabellen k�nnen in zwei Klassen eingeteilt werden.
  Solche die die ganze Tabelle betreffen, die beginnen mit dem Pr�fix
  \ <src-var|table->, und solche die nur eine einzelne Zelle betreffen, diese
  beginnen mit dem Pr�fix <src-var|cell->. W�hrend normale Kontextvariablen
  mit den Konstrukten <markup|assign> und <markup|with> gesetzt werden,
  benutzt man dazu bei den Tabellenvariablen das <hyper-link|Konstrukt
  <markup|tformat>|../regular/prim-table.de.tm>. Mit diesem Konstrukt k�nnen
  bestimmte Vorgaben auf alle rechteckigen Unter-Tabellen �bertragen werden.
  Das gilt vor allem f�r Reihen und Spalten. Mehr Informationen finden Sie
  \ <hyper-link|hier|../regular/prim-table.de.tm#table-twith> f�r die
  Konstrukte <markup|twith> und <markup|cwith>.

  <paragraph|Layout der ganzen Tabelle>

  <\explain>
    <var-val|table-width|>

    <var-val|table-height|><explain-synopsis|Minimale Tabellendimensionen>
  <|explain>
    Diese Parameter geben einen Hinweis auf die ungef�hren Dimensionen einer
    Tabelle. Die Parameter <src-var|table-hmode> und <src-var|table-vmode>
    bestimmen, wie sie auszuwerten sind.
  </explain>

  <\explain>
    <var-val|table-hmode|>

    <var-val|table-vmode|><explain-synopsis|Berechnungsweise der
    Tabellendimensionen>
  <|explain>
    <src-var|table-hmode> und <src-var|table-vmode> werden zur Zeit
    ignoriert. Momentan werden <src-var|table-width> und
    <src-var|table-height> als minimale Tabellendimensionen interpretiert. In
    Zukunft sollen <src-var|table-hmode> und <src-var|table-vmode> aber
    steuern wie genau <src-var|table-width> und <src-var|table-height>
    auszuwerten sind.
  </explain>

  <\explain>
    <var-val|table-halign|l>

    <var-val|table-valign|f><explain-synopsis|Einpassung in den Text>
  <|explain>
    Die vorstehenden Parameter bestimmen, wie die Tabelle in den umgebenden
    Text eingef�gt werden soll. M�gliche Werte f�r <src-var|table-halign>
    sind <verbatim|l> (<translate|left|english|german>), <verbatim|c>
    (<translate|center|english|german>) und <verbatim|r>
    (<translate|right|english|german>); m�gliche Werte f�r
    <src-var|table-valign> sind <verbatim|t>
    (<translate|top|english|german>), <verbatim|f> (<translate|centered at
    fraction bar height|english|german>), <verbatim|c>
    (<translate|center|english|german>) und <verbatim|b>
    (<translate|bottom|english|german>).

    Au�er den oben genannten Werten sind noch weitere m�glich, die die
    Tabelle in Bezug zu der Basislinie bestimmter Zellen positioniert. F�r
    <src-var|table-halign> sind dies <verbatim|L> (nach der linken Spalte
    ausrichten), <verbatim|C> (nach der mittleren Spalte ausrichten),
    <verbatim|R> (nach der rechten Spalte ausrichten) und <verbatim|O> (nach
    der Spalte der privilegierten Zelle <src-var|table-col-origin> ausrichten
    , s.u.). Entsprechend kann <src-var|table-valign> die zus�tzlichen Werte
    <verbatim|T> (nach der obersten Zeile ausrichten), <verbatim|C> (Nach der
    Mittelzeile ausrichten), <verbatim|B> (nach der untersten Zeile
    ausrichten) und <verbatim|O> (nach der Zeile der privilegierten Zelle
    <src-var|table-row-origin> ausrichten, s.u.).
  </explain>

  <\explain>
    <var-val|table-row-origin|0>

    <var-val|table-col-origin|0><explain-synopsis|privilegierte Zelle>
  <|explain>
    Die Tabellen-Koordinaten einer privilegierten Zelle, origin cell, die zur
    Ausrichtung im umgebenden Text dienen kann (s.o.).
  </explain>

  <\explain>
    <var-val|table-lsep|0fn>

    <var-val|table-rsep|0fn>

    <var-val|table-bsep|0fn>

    <var-val|table-tsep|0fn><explain-synopsis|Padding um eine Tabelle>
  <|explain>
    Der kontextabh�ngige Leerraum, Padding, um eine Tabelle (zus�tzlich zum
    Padding um die einzelnen Zellen).
  </explain>

  <\explain>
    <var-val|table-lborder|0ln>

    <var-val|table-rborder|0ln>

    <var-val|table-bborder|0ln>

    <var-val|table-tborder|0ln><explain-synopsis|Tabellenr�nder>
  <|explain>
    Breite der Tabellenr�nder zus�tzlich zur Breite der R�nder um die
    einzelnen Zellen.
  </explain>

  <\explain>
    <var-val|table-hyphen|n><explain-synopsis|Seitenumbruch erlaubt?>
  <|explain>
    Boolesche Variable, die angibt ob ein Seitenumbruch innerhalb der Tabelle
    erlaubt ist. Wenn <src-var|table-hyphen> auf y gesetzt ist, kann ein
    Seitenumbruch erfolgen aber nur dann, wenn

    <\enumerate>
      <item> Die Tabelle innerhalb des selben Absatzes nicht von weiteren
      Kontexten oder speziellen Hervorhebungen gesteuert wird.

      <item>Die Zeilen zwischen denen der Seitenumbruch erfolgt, haben kein
      Gitter.
    </enumerate>

    Ein Beispiel, dass f�r eine Tabelle mit erlaubtem Seitenumbruch ist
    <markup|eqnarray*>.
  </explain>

  <\explain>
    <var-val|table-min-rows|>

    <var-val|table-min-cols|>

    <var-val|table-max-rows|>

    <var-val|table-max-cols|><explain-synopsis|Minimum bzw. Maximum der
    Tabellendimensionen>
  <|explain>
    Man kann die minimale und die maximale Anzahl sowohl von Zeilen wie von
    Spalten festlegen. Das begrenzt die M�glichkeiten des Editors beim
    Einf�gen und L�schen von Zeilen und Spalten. Dies ist besonders n�tzlich
    bei Tabellenmakros. So sind f�r <markup|eqnarray*>
    <src-var|table-min-columns> und <src-var|table-max-columns> fest auf 3
    gesetzt.
  </explain>

  <paragraph|Layout individueller Zellen>

  <\explain>
    <var-val|cell-background|><explain-synopsis|Hintergrundfarbe>
  <|explain>
    Die Hintergrundfarbe einer Zelle.
  </explain>

  <\explain>
    <var-val|cell-width|>

    <var-val|cell-height|><explain-synopsis|Ungef�hre Zelldimensionen>
  <|explain>
    Diese Variablen sind Vorgaben f�r die Festlegung von Zellh�he und
    -Breite, die au�erdem von den Variablen \ <src-var|cell-hmode> und
    <src-var|cell-vmode> sowie <src-var|cell-hpart> und <src-var|cell-vpart>
    abh�ngt.\ 
  </explain>

  <\explain>
    <var-val|cell-hpart|>

    <var-val|cell-vpart|><explain-synopsis|zus�tzlicher Paddinganteil einer
    Zelle>
  <|explain>
    Wenn die Summe <with|mode|math|s> der Breiten aller Spalten kleiner ist
    als die Breite der Tabelle \ <with|mode|math|w>, dann muss man vorgeben,
    was mit dem leeren Platz geschehen soll. Der Parameter
    <src-var|cell-hpart> legt fest wie viel Platz eine einzelne Zelle von dem
    leeren Platz bekommen soll. Der horizontale Anteil einer ganzen Spalte
    ist das Maximum der Anteile aller Zellen der Spalte.
    <with|mode|math|p<rsub|i>> sei der so definierte Anteil jeder Spalte
    <with|mode|math|i>, (<with|mode|math|i\<in\>{1,\<ldots\>,n}>). Der
    verbleibende Anteil wird dann verteilt, in dem jeder Spalte Dann wird
    jeder Spalte <with|mode|math|i> der Anteil
    <with|mode|math|p<rsub|i>*(w-s)/(p<rsub|1>+\<cdots\>+p<rsub|n>)>
    zugeteilt wird. Ganz entsprechend wird in jeder Zeile verfahren.
  </explain>

  <\explain>
    <var-val|cell-hmode|exact>

    <var-val|cell-vmode|exact><explain-synopsis|Berechnungsweise der
    Zelldimensionen>
  <|explain>
    Diese Parameter legen fest, auf welche Weise Breite und H�he der Zelle zu
    berechnen sind. Wenn <src-var|cell-hmode> auf <verbatim|exact> gesetzt
    ist, dann ist die Breite <src-var|cell-width>. Wenn <src-var|cell-hmode>
    auf <verbatim|min> oder auf <verbatim|max> gesetzt ist, dann ist die
    wirkliche Breite das Minimum von <src-var|cell-width> und der Breite des
    Zellinhalts bzw. das entsprechende Maximum. Die Festlegung der Zellh�he
    geschieht analog.
  </explain>

  <\explain>
    <var-val|cell-halign|l>

    <var-val|cell-valign|B><explain-synopsis|Zellausrichtung>
  <|explain>
    Diese Parameter bestimmen die Zellausrichtung. M�gliche Werte f�r
    <src-var|cell-halign> sind <verbatim|l>
    (<translate|left|english|german>), <verbatim|c>
    (<translate|center|english|german>), <verbatim|r>
    (<translate|right|english|german>), <verbatim|.> (<translate|decimal
    dot|english|german>), <verbatim|,> (<translate|decimal
    comma|english|german>) und <verbatim|R> (<translate|vertical
    baseline|english|german>). M�gliche Werte f�r <src-var|cell-valign> sind
    <verbatim|t> (<translate|top|english|german>), <verbatim|c>
    (<translate|center|english|german>), <verbatim|b>
    (<translate|bottom|english|german>) und <verbatim|B>
    (<translate|baseline|english|german>).
  </explain>

  <\explain>
    <var-val|cell-lsep|0fn>

    <var-val|cell-rsep|0fn>

    <var-val|cell-bsep|0fn>

    <var-val|cell-tsep|0fn><explain-synopsis|Zellpadding>
  <|explain>
    Padding einer Zelle (<translate|left|english|german>,
    <translate|right|english|german>, <translate|bottom|english|german> und
    <translate|top|english|german>).
  </explain>

  <\explain>
    <var-val|cell-lborder|0ln>

    <var-val|cell-rborder|0ln>

    <var-val|cell-bborder|0ln>

    <var-val|cell-tborder|0ln><explain-synopsis|Zellgitter>
  <|explain>
    Die Dicke der Linien, die als R�nder die Zelle begrenzen
    (<translate|left|english|german>, <translate|right|english|german>,
    <translate|bottom|english|german> und <translate|top|english|german>).
    Die Breite der Randlinie zwischen den Zellen <with|mode|math|T<rsub|i,j>>
    und <with|mode|math|T<rsub|i,j+1>> an den Positionen
    <with|mode|math|(i,j)> und <with|mode|math|(i,j+1)> ist das Maximum der
    rechten Randlinie von <with|mode|math|T<rsub|i,j>> und der linken von
    <with|mode|math|T<rsub|i,j+1>>. Analog werden die oberen und unteren
    Randlinien-Breiten bestimmt.
  </explain>

  <\explain>
    <var-val|cell-vcorrect|a><explain-synopsis|vertikale Korrektur von Text>
  <|explain>
    Wie bereits oben beschrieben k�nnen die Dimensionen und die Ausrichtung
    der Zellen vom Inhalt abh�ngen. Wenn die Zellen Boxen enthalten dann kann
    die H�he der Boxen von deren Inhalt abh�ngen, denn z.B. hat der Buchstabe
    \Rk`` eine gr��ere Oberl�nge als \Ry``, das daf�r eine gr��ere Unterl�nge
    besitzt. Solche Unterschiede f�hren manchmal zu unerw�nschten
    Ungleichm��igkeiten im Satzbild. Die vertikale Zellenkorrektur
    <src-var|cell-vcorrect> verbessert die Gleich-m��igkeit von Text in einer
    einzigen Schriftart, indem Boxen nach oben oder unten verschoben werden
    in einer Weise, die nur von der gew�hlten Schriftart abh�ngt. M�gliche
    Werte f�r \ <src-var|cell-vcorrect> sind <verbatim|n> (<translate|no
    vertical correction|english|german>), <verbatim|b> (<translate|vertical
    correction of the bottom|english|german>), <verbatim|t>
    (<translate|vertical correction of the top|english|german>), <verbatim|a>
    (<translate|vertical correction of bottom and the top|english|german>).
  </explain>

  <\explain>
    <var-val|cell-hyphen|n><explain-synopsis|Trennung innerhalb einer Zelle
    erlaubt?>
  <|explain>
    Normalerweise wird innerhalb von Zellen nicht getrennt. Durch setzen der
    Option <menu|Table|Special cell properties|Hyphenation|Multi-paragraph>
    wird die Zelle f�r die Aufnahme mehrerer Abs�tze eingerichtet. In diesem
    Fall kann mit <src-var|cell-hyphen> die Art und Weise des Zeilenumbruch
    bzw. der Trennung bestimmt werden. M�gliche Werte sind <verbatim|n>
    (<translate|disable line breaking|english|german>), \ <verbatim|b>
    (<translate|enable line breaking and align at the
    bottom|english|german>), <verbatim|c> \ (<translate|enable line breaking
    and align at the center|english|german>) und <verbatim|t>
    (<translate|enable line breaking and align at the top
    line|english|german>).
  </explain>

  <\explain>
    <var-val|cell-row-span|1>

    <var-val|cell-col-span|1><explain-synopsis|Mehrfachzellen>
  <|explain>
    Man kann Zellen so konfigurieren, das diese den Platz von mehreren Zellen
    belegen, die entweder rechts neben ihnen oder unter ihnen liegen. Mit
    solchen Mehrfachzellen k�nnen z.B. �berschriften, die sich �ber mehrere
    Zellen erstrecken und sie zusammenfassen, in die Tabelle eingef�gt
    werden. <src-var|cell-row-span> und <src-var|cell-col-span> legen fest,
    wie breit bzw. wie hoch eine solche Zelle in Anzahl der Nachbarzellen
    ist.
  </explain>

  <\explain>
    <var-val|cell-decoration|><explain-synopsis|Dekorierende Tabelle f�r eine
    Zelle>
  <|explain>
    Diese Kontextvariable kann eine \Rdekorierende'' Zelle enthalten. Diese
    f�gt der die urspr�nglichen Tabelle weitere Spalten und Zeilen au�en
    hinzu. Das Konstrukt <markup|tmarker> \ definiert den Ort der
    urspr�nglichen Zelle. Ihre Umgebung wird in der vergr��erten Tabelle mit
    Dekorationen aufgef�llt. Zelldekorationen werden kaum gebraucht und
    verschwinden vielleicht in zuk�nftigen Versionen von <TeXmacs>.
  </explain>

  <\explain>
    <var-val|cell-orientation|portrait><explain-synopsis|Zellorientierung>
  <|explain>
    Bisher ist nur das L�ngsformat, <verbatim|portrait>, implementiert.
  </explain>

  <\explain>
    <var-val|cell-row-nr|1>

    <var-val|cell-col-nr|1><explain-synopsis|Aktuelle Zellposition>
  <|explain>
    Das ist noch nicht implementiert. In Zukunft sollen diese Variablen die
    aktuelle Zellposition w�hrend des Schriftsetzens enthalten.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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