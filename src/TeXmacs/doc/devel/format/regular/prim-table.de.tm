<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Tabellen-Konstrukte>

  Tabellen sind in allen Dokumenten vorhanden, die <markup|tformat>-Argumente
  akzeptieren. Alle fundamentalen Tabellen-Konstrukte haben R�nder, die nicht
  erreichbar sind. Das grundlegende Konstrukt ist <markup|tabular>.

  <\explain>
    <explain-macro|tformat|with-1|<with|mode|math|\<cdots\>>|with-n|table><explain-synopsis|Container
    zur Tabellen-Formatierung>
  <|explain>
    Jede Tabellen-Struktur in einem Dokument hat einen <markup|tformat>-Tag.

    <explain-macro|tformat|table> bedeutet, dass die Tabellen- und
    Zell-Kontextvariablen unmodifizierte Vorgaben sind. Das Argument,
    <src-arg|table>, kann eine Tabelle, <markup|table>, oder ein
    verschachtelter <markup|tformat>-Konstrukt sein. Letzterer erscheint
    nicht in den Dokumenten, wird aber w�hrend der Evaluierung des obersten
    Konstrukts automatisch erzeugt.

    <explain-macro|tformat|with-1|<with|mode|math|\<cdots\>>|with-n|table>
    wird benutzt, wenn die Tabelle, <src-arg|table>, \ spezielle
    Formatierungs-Optionen ben�tigt. Die <src-arg|with-1> bis
    <src-arg|with-n> Argumente m�ssen alle <markup|twith> oder
    <markup|cwith>-Konstrukte sein.
  </explain>

  <\explain>
    <label|table-twith><explain-macro|twith|var|val><explain-synopsis|eine
    Tabellenvariable setzen>
  <|explain>
    Die Formatierung einer Tabelle als Ganzes wird von einer Anzahl von
    <em|Tabellenvariablen> gesteuert, die nur intern benutzt werden und nicht
    im Kontext erscheinen wie die normalen den Schriftsatz steuernden
    Kontextvariablen.

    Das <markup|twith>-Konstrukt setzt die Variable <src-arg|var>
    (Zeichenfolge) auf den Wert <src-arg|val> (nach Evaluierung).
  </explain>

  <\explain>
    <label|table-cwith><explain-macro|cwith|top-row|bot-row|left-col|right-col|var|val><explain-synopsis|Zellvariablen
    f�r einen Zellbereich setzen>
  <|explain>
    Die Formatierung von Zellen wird von einer Anzahl von <em|Zellvariablen>
    gesteuert, die nur intern benutzt werden und nicht im Kontext erscheinen
    wie die normalen den Schriftsatz steuernden Kontextvariablen. Zeilen,
    Spalten, generell jeder rechteckige Bereich kann mit Hilfe eines einzigen
    <markup|cwith>-Konstrukts mit einer Zellvariablen assoziiert werden.

    Das <markup|cwith>-Konstrukt setzt die Zellvariable, <src-arg|var>
    (Zeichenfolge) auf de Wert <src-arg|val> (nach Evaluierung) f�r den
    Zeilen-Bereich <src-arg|top-row> bis <src-arg|bot-row> und
    Spalten-Bereich <src-arg|left-col> bis <src-arg|right-col> (Zahlzeichen
    ohne 0).

    Die Bereichs-Koordinaten sind ganzzahlige Werte, au�er 0, positive Werte
    werden von links nach rechts und von oben nach unten gez�hlt, negative
    Werte entsprechend von rechts nach links und von unten nach oben. 2
    bedeutet also die zweite Spalte rechts oder die zweite Reihe nach unten,
    -1 hei�t die Spalte links oder Zeile dar�ber.

    Typische Werte f�r <with|mode|math|(<with|mode|text|<src-arg|top-row>>,<with|mode|text|<src-arg|bot-row>>,<with|mode|text|<src-arg|left-col>>,<with|mode|text|<src-arg|right-col>>)>
    sind <with|mode|math|(r,r,<op|>1,<op|->1)> f�r \RZeile
    <with|mode|math|r>'', <with|mode|math|(<op|>1,<op|->1,c,c)> f�r \RSpalte
    <with|mode|math|c>'', und <with|mode|math|(r,r,c,c)> f�r \Rdie Zelle
    Reihe r, Spalte c''. Wenn neue Zellen eingef�gt werden, macht es einen
    Unterschied, ob die Reihen von oben oder von unten und ob die Spalten von
    links oder von rechts gez�hlt werden. Wenn <with|mode|math|m> die Anzahl
    der Zeilen und n die Anzahl de Spalten ist, dann repr�sentieren
    <with|mode|math|r> und <with|mode|math|r-m-1> dieselbe Zeile nur einmal
    von oben und einmal von unten gez�hlt. �hnlich entsprechen
    <with|mode|math|c> und <with|mode|math|c-n-1> dieselbe Spalte einmal von
    links andermal von rechts.
  </explain>

  <\explain>
    <explain-macro|table|row-1|<with|mode|math|\<cdots\>>|row-n><explain-synopsis|Zeilencontainer>
  <|explain>
    Der einzige Zweck des <markup|table>-Konstrukts ist es,
    <markup|row>-(Zeilen)-Konstrukte aufzunehmen. Die Anzahl der Zeilen ist
    die Anzahl der Unter-B�ume.
  </explain>

  <\explain>
    <explain-macro|row|cell-1|<with|mode|math|\<cdots\>>|cell-k><explain-synopsis|Zellencontainer>
  <|explain>
    Der einzige Zweck des <markup|row><markup|>-Konstrukts ist es,
    <markup|cell>-(Zellen)-Konstrukte aufzunehmen. Die Anzahl der Zeilen ist
    die Anzahl der Unter-B�ume. Alle <markup|row>-(Reihen)-Konstrukte in
    einer Tabelle, <markup|table>, m�ssen die genau so viele Unter-B�ume,
    <markup|cell>-(Zellen)-Konstrukte, haben, wie Spalten in der Tabelle
    vorhanden sind.
  </explain>

  <\explain>
    <explain-macro|cell|content><explain-synopsis|Zell-Datencontainer>
  <|explain>
    Die Zellen von Tabellen k�nnen jede Art von Dokument-Fragmenten
    enthalten. Eine Zelle, <markup|cell>, kann direkt Zeileninhalt oder ein
    <markup|concat>-Konstrukt enthalten, wenn Blockinhalt eingef�gt werden
    soll, muss es in Form eines <markup|document>-Baumes sein.

    Eine Zelle, <markup|cell>, deren Inhalt ein <markup|document> ist, ist
    eine <def-index|Multi-Absatz-Zelle>. Weil Tabellen im Zeilen-kontext
    erlaubt sind, ist dies das einzige Konstrukt, das indirekt die Einf�gung
    von Blockinhalten in Zeilen-kontext erlaubt. Man beachte, dass fast jeder
    Blockinhalt nur in Zellen, die umgebrochen werden k�nnen, korrekt gesetzt
    werden kann. Dies wird mit der Tabellenvariablen <src-var|cell-hyphen>
    eingestellt.
  </explain>

  <\explain>
    <explain-macro|subtable|table><explain-synopsis|Unter-Tabellen>
  <|explain>
    Zellen k�nnen Unter-Tabellen, <markup|><markup|subtable>, enthalten. Das
    Argument von <markup|subtable> ist ein <markup|tformat>-Baum, der
    normalen Tabellen-Inhalt enth�lt.

    Ein �hnlicher Effekt kann erreicht werden, wenn man das Zell-Padding in
    alle Richtungen auf 0 setzt. Eine Besonderheit von <markup|subtable> ist
    aber, dass ihre R�nder nicht erreichbar sind.
  </explain>

  <\explain>
    <explain-macro|tmarker|table><explain-synopsis|Markierung des
    Dekorationsurpsrungs>
  <|explain>
    Dieses Konstrukt wird bei der Definition von Zelldekorationen benutzt,
    siehe die Dokumentation zu <src-var|cell-decoration>.

    Es wird au�erdem au�erhalb von Tabellen zur Markierung der augenblicklich
    dargestellten Position im <markup|switch>-Konstrukt verwendet.
  </explain>

  <\explain>
    <explain-macro|tabular|table><explain-synopsis|fundamentales
    Tabellenmakro>
  <|explain>
    Diese Makro definiert links ausgerichtete Standard-Tabellen ohne Gitter.
    Obwohl <markup|tabular> in <TeXmacs> eingebaut ist, sollte es eigentlich
    nicht als ein fundamentales Konstrukt betrachtet werden. Allerdings
    geh�rt es auch nicht zu den Stil-Definitionen.\ 
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

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