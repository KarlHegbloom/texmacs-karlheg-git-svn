<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Text-Operatoren>

  <\explain>
    <explain-macro|length|expr><explain-synopsis|L�nge einer Zeichenkette>
  <|explain>
    Wenn <src-arg|expr> eine Zeichenkette ist wird deren L�nge zur�ckgegeben.
    \ <inactive*|<length|Hello>> evaluiert zu <length|Hello>.
  </explain>

  <\explain>
    <explain-macro|range|expr|start|end><explain-synopsis|Extrahiere eine
    Teilkette>
  <|explain>
    Gebe den Teil der Zeichenkette <src-arg|expr> zur�ck, der an der Position
    <src-arg|start> beginnt und an <src-arg|end> aufh�rt. Die Endposition ist
    nicht dabei. <inactive*|<range|hottentottententententoonstelling|9|15>>
    evaluiert zu <range|hottentottententententoonstelling|9|15>.
  </explain>

  <\explain>
    <explain-macro|merge|expr-1|<with|mode|math|\<cdots\>>|expr-n><explain-synopsis|Verkette
    Zeichenketten>
  <|explain>
    Damit werden mehrer Zeichenketten <src-arg|expr-1> bis <src-arg|expr-n>
    zu einer neuen Zeichenkette zusammengef�gt.
    <inactive*|<merge|Hello|World>> gibt <merge|Hello|World>.
  </explain>

  <\explain>
    <explain-macro|number|number|render-as><explain-synopsis|unterschiedliche
    Darstellungen von Zahlen>
  <|explain>
    Stelle eine Zahl <src-arg|number> in einer bestimmten Weise dar. M�gliche
    Werte f�r <src-arg|render-as> sind

    <\description>
      <item*|roman>kleine r�mische Zahlen: <inactive*|<number|18|roman>>
      <with|mode|math|\<longrightarrow\>> <number|18|roman>.

      <item*|Roman>Gro�e r�mische Zahlen: <inactive*|<number|18|Roman>>
      <with|mode|math|\<longrightarrow\>> <number|18|Roman>.

      <item*|alpha>kleine Buchstaben: <inactive*|<number|18|alpha>>
      <with|mode|math|\<longrightarrow\>> <number|18|alpha>.

      <item*|Alpha>Gro�buchstaben: <inactive*|<number|18|Alpha>>
      <with|mode|math|\<longrightarrow\>> <number|18|Alpha>.
    </description>
  </explain>

  <\explain>
    <explain-macro|date>

    <explain-macro|date|format>

    <explain-macro|date|format|language><explain-synopsis|aktuelles Datum>
  <|explain>
    gibt das aktuelle Datum in einer bestimmten landesspezifischen
    Darstellung <src-arg|format> zur�ck, wenn dieses Feld leer gelassen wird,
    wird die landesspezifische Voreinstellung benutzt, und, wenn angegeben,
    in einer bestimmten Sprache <src-arg|language> ansonsten Englisch. Das
    Format �hnelt der Ausgabe des <name|Unix> <verbatim|date>-Befehls.
    \ <inactive*|<date>> evaluiert zu \R<date>'', <inactive*|<date||french>>
    zu \R<date||french>'' und <inactive*|<date|%d %B om %k:%M|dutch>> zu
    \R<date|%d %B om %k:%M|dutch>''.
  </explain>

  <\explain>
    <explain-macro|translate|what|from|into><explain-synopsis|�bersetzung>
  <|explain>
    Gibt die �bersetzung der Zeichenkette <src-arg|what> aus der Sprache
    <src-arg|from> in die Sprache <src-arg|into> zur�ck. Dabei wird das
    <TeXmacs>-W�rterbuch verwendet. Die Sprachen sind in kleinen Buchstaben
    in englischer Sprache anzugeben. <inactive*|<translate|File|english|french>>
    liefert \R<translate|File|english|french>''.

    Eine Liste der verf�gbaren Sprachen findet sich im Men�
    <menu|Document|Language>. Die englische Schreibweise kann man leicht
    ermitteln, indem man vor�bergehend die Sprach unter
    <menu|Edit|Preferences|Language> auf Englisch einstellt. Die eingebauten
    <TeXmacs>-W�rterb�cher findet man in

    <verbatim| \ \ \ $TEXMACS_PATH/languages/natural/dic>

    Wenn man versucht ein nicht vorhandenes W�rterbuch zu benutzen, kann das
    Programm abst�rzen. Meistens ist es einfacher das Makro <markup|localize>
    zu benutzen, das eine Zeichenkette aus dem Englischen in die aktuelle
    Sprache umsetzt.<verbatim|>
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
  </collection>
</initial>