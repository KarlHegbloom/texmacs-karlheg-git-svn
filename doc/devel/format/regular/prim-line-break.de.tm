<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Zeilenumbruch-Konstrukte>

  Ein einfaches Dokument ist eine Sequenz von <em|logischen Abs�tzen>, eine
  f�r jeden Unter-Baum eines \ <markup|document>- oder
  <markup|paragraph>-Knoten. Abs�tze, die den verf�gbaren horizontalen Raum
  �berschreiten, m�ssen in <em|physikalische Zeilen> umgebrochen werden.
  Umgebrochene Zeilen werden als Vorgabe im Blocksatz gesetzt, dabei kann
  horizontaler Leerraum gedehnt oder gestaucht werden, um ein gutes
  Schriftbild zu erzeugen.

  <\explain>
    <explain-macro|new-line><explain-synopsis|Beginn eines neuen Absatzes>
  <|explain>
    Dieses Konstrukt ist �berholt. Es dient dazu, einen logischen Absatz in
    mehrere logische Abs�tze zu unterteilen, ohne explizit neue Unter-B�ume
    f�r alle Abs�tze zu erzeugen.

    Wir erinnern daran, dass logische Abs�tze wichtige Strukturen im
    Schriftsatz-Prozess sind. Viele Konstrukte und Kontextvariablen
    (Vertikaler Abstand, Absatz-Stil, Zeileneinzug, Seitenumbruch usw.)
    operieren mit ganzen Abs�tzen oder an den Grenzen des umgebenden
    Absatzes.
  </explain>

  <\explain>
    <explain-macro|next-line><explain-synopsis|Beginn einer neuen Zeile>
  <|explain>
    Dieses Konstrukt wird �berholt sein, wenn das
    <markup|paragraph>-(Absatz)-Konstrukt korrekt implementiert ist. Sein
    Gebrauch �hnelt <markup|new-line>, nur dass hier eine neue logische
    Absatzeinheit (Zeile) anstelle eines logischen Absatzes erzeugt wird.

    Zur Zeit kann <markup|next-line> benutzt werden, um einen Zeilenumbruch
    zu erzwingen, bei dem die Zeile vor dem Umbruch genau an dieser Stelle
    umgebrochen wird und auch nicht in Blocksatz gesetzt wird.
  </explain>

  <\explain>
    <explain-macro|line-break><explain-synopsis|Bedingter Trennstrich>
  <|explain>
    Es wird unsichtbarer Abstand mit Breite 0 und eine Trennstrafe 0. Die
    verschiedenen Wort-Trennungen besitzen unterschiedliche Trennstrafen. Der
    Trennungs-Algorithmus sucht nach einem Satz von Trennpunkten, indem er
    die Summe der Trennstrafen minimiert. Deshalb ist die Trennung an der
    Stelle des <markup|line-break> wahrscheinlicher als irgendwo in seiner
    N�he.

    Im Gegensatz zu <markup|next-line> erzwingt <markup|line-break> keinen
    Umbruch. Es ist ein bedingter Trennstrich.
  </explain>

  <\explain>
    <explain-macro|no-break><explain-synopsis|Trennung an dieser Stelle
    verhindern>
  <|explain>
    Setze einen Trennpunkt mit einer unendlich hohen Trennstrafe, verbiete
    also die Trennung an dieser Stelle. Das ist manchmal sehr n�tzlich. Eine
    andere M�glichkeit unerw�nschte Trennungen zu verhindern, ist das
    <markup|group>-Konstrukt.
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