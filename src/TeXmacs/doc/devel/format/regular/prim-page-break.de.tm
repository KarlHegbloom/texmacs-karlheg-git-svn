<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Seitenumbruch-Konstrukte>

  Ein Dokument wird in �hnlicher Weise in Seiten umgebrochen wie die Abs�tze
  in Zeilen. Der Seitenumbruch-Algorithmus erzeugt <def-index|Seitenf�llung>,
  das �hnelt dem Blocksatz, er versucht Seiten gleichm��ig mit Text zu
  versehen, so dass der Text bis zum Seitenende l�uft. Er versucht auch
  sogenannte <em|Weisenkinder> und <em|Witwen> zu vermeiden. Das sind ein
  oder zwei Zeilen, die vom Rest ihres Absatzes durch einen Seitenumbruch
  getrennt wurden. Wenn es keine bessere L�sung gibt, k�nnen diese dennoch
  entstehen.

  <\explain>
    <explain-macro|no-page-break><explain-synopsis|einen automatischen
    Seitenumbruch nach dieser Zeile verhindern>
  <|explain>
    Dies verhindert einen automatischen Seitenumbruch direkt hinter dieser
    Zeile. Dies setzt die Seitenumbruch-Strafe f�r diese Zeile auf unendlich,
    ganz �hnlich wie <markup|no-break>.

    Verbotene Seitenumbruch-Punkte werden durch \Rnew page'' und \Rpage
    break'' aufgehoben.
  </explain>

  <\explain>
    <explain-macro|no-page-break*><explain-synopsis|einen automatischen
    Seitenumbruch vor dieser Zeile verhindern>
  <|explain>
    �hnlich wie <markup|no-page-break>. Es setzt aber die Strafe in der
    vorgehenden Zeile.
  </explain>

  <\explain>
    <explain-macro|new-page><explain-synopsis|beginne eine neue Seite nach
    dieser Zeile>
  <|explain>
    Sorgt daf�r, dass die n�chste Zeile auf einer neuen Seite erscheint, ohne
    das die Seite gef�llt wird. Der Seitenumbruch-Algorithmus versucht also
    nicht die aktuelle Zeile ganz unten auf der Seite zu setzen.\ 
  </explain>

  <\explain>
    <explain-macro|new-page*><explain-synopsis|beginne eine neue Seite vor
    dieser Zeile>
  <|explain>
    �hnlich zu <markup|new-page>. Aber der Seitenumbruch erfolgt vor der
    aktuellen Zeile, so dass die aktuelle Zeile auf der neuen Seite
    erscheint. Dies ist f�r Kapitel-�berschriften geeignet.
  </explain>

  <\explain>
    <explain-macro|page-break><explain-synopsis|einen Seitenumbruch nach
    dieser Zeile erzwingen>
  <|explain>
    Dies erzwingt einen Zeilenumbruch hinter der aktuellen Zeile. Im
    Gegensatz zu new-page wird die Seite gef�llt. \ Seitenumbruch-Algorithmus
    versucht die aktuelle Zeile ganz unten auf die Seite zu setzen.\ 

    Das sollte man nur benutzen, um den automatischen Seitenumbruch
    vorsichtig anzupassen. Idealerweise sollte das eigentlich wie
    <markup|line-break> wie ein Hinweis arbeiten, da es aber als Befehl
    implementiert ist, sollte man es mit �u�erster Vorsicht einsetzen.
  </explain>

  <\explain>
    <explain-macro|page-break*><explain-synopsis|einen Seitenumbruch nach
    dieser Zeile erzwingen>
  <|explain>
    �hnlich wie <markup|page-break>, bezieht aber auf die vorg�ngige Zeile.
  </explain>

  Wenn mehrere \Rnew page'' und \Rpage break'' Befehle sich auf die selbe
  Stelle beziehen, dann wird nur der erste ber�cksichtigt. \ Jedes
  <markup|new-page> oder <markup|page-break> nach dem ersten wird ignoriert.
  Jedes <markup|new-page> oder <markup|page-break> in einer Zeile wird
  <markup|new-page*> oder <markup|page-break*> in der folgenden Zeile
  vorgezogen. Jedes <markup|new-page*> oder <markup|page-break*> nach dem
  ersten Auftreten wird ignoriert.

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