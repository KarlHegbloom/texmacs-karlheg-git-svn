<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Konstrukte f�r Boxen>

  <\explain>
    <explain-macro|move|content|delta-x|delta-y><explain-synopsis|Position
    verschieben>
  <|explain>
    Dieses Konstrukt verschiebt die Box mit dem Inhalt <src-arg|content> um
    <src-arg|delta-x> nach rechts und <src-arg|delta-y> nach oben.\ 
  </explain>

  <\explain>
    <explain-macro|resize|content|left-lim|bot-lim|right-lim|top-lim><explain-synopsis|Gr��enanpassung>
  <|explain>
    Ver�ndere die Gr��e der Box mit dem Inhalt <src-arg|content> zu neuen
    Grenzen links, unten, oben und oben: <src-arg|left-lim>,
    <src-arg|bot-lim>, <src-arg|right-lim> und <src-arg|top-lim>. Die Grenzen
    k�nnen leere Zeichenketten sein, in diesem Fall werden die alten Grenzen
    verwendet, absolute Koordinaten oder Grenzen, die aus den alten berechnet
    werden.

    In diesem Fall sollten die Grenzen die Form
    <verbatim|\<less\>pos\<gtr\>\<less\>op\<gtr\>\<less\>len\<gtr\>> haben.
    Das erste Zeichen <verbatim|\<less\>pos\<gtr\>> verweist auf die Position
    der urspr�nglichen Box und ist entweder <verbatim|l> (links),
    <verbatim|b> (unten), <verbatim|c> (Mitte), <verbatim|r> (rechts) oder
    <verbatim|t> (oben). Der zweite Buchstabe <verbatim|\<less\>op\<gtr\>>
    verweist auf die Operation, die an dieser Position durchgef�hrt werden
    soll, und der letzte Buchstabe auf eine L�ngenangabe, die auf die
    Position mit der gegebenen Operation angewendet werden soll. M�gliche
    Operationen sind <verbatim|+>, <verbatim|->, <verbatim|[> und
    <verbatim|]>. Die Klammern <verbatim|[> und <verbatim|]> stehen f�r
    \RMinimum'' und \RMaximum''. Beispielsweise verbreitert

    <\tm-fragment>
      <inactive*|(<resize|Hopsa|l-5mm||r+5mm||>)>
    </tm-fragment>

    die Box \RHopsa'' um <verbatim|5mm> an jeder Seite:

    <\tm-fragment>
      (<resize|Hopsa|l-5mm||r+5mm||>)
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|if*|condition|content><explain-synopsis|bedingtes
    Erscheinen einer Box>
  <|explain>
    Die Box mit dem Inhalt <src-arg|content> wird normal angezeigt, wenn
    <src-arg|condition> wahr ist, sonst als Leerraum. Dieses Konstrukt wird
    vor allem zur Definition des <markup|phantom>-Makro verwendet.
    \ Beispielsweise wird der Leerraum \R<if*|false|phantom>'' erzeugt mit
    <inactive*|<if*|false|phantom>>.
  </explain>

  <\explain>
    <explain-macro|repeat|content|pattern><explain-synopsis|�berschreibe>
  <|explain>
    Dieses Konstrukt kann dazu benutzt werden, den Inhalt mit
    <src-arg|content> mit einen bestimmten Schriftzug <src-arg|pattern> zu
    �berschreiben. Beispielsweise erzeugt der Code

    <\tm-fragment>
      <inactive*|<assign|wipe-out|<macro|x|<repeat|<arg|x>|<with|color|red|/>>>>>
    </tm-fragment>

    <inactive*|<wipe-out|obsolete>> das Folgende
    <with|wipe-out|<macro|x|<repeat|<arg|x>|<with|color|red|/>>>|<wipe-out|obsolete>>.
    Das <markup|repeat>-Konstrukt kann au�erdem dazu benutzt werden die
    aktuelle Zeile mit einem bestimmten Inhalt zu f�llen, wie z.B. die Punkte
    in Inhaltsverzeichnissen.\ 
  </explain>

  <\explain>
    <explain-macro|datoms|foo|content>

    <explain-macro|dlines|foo|content>

    <explain-macro|dpages|foo|content><explain-synopsis|Dekorationen>
  <|explain>
    Diese Konstrukte sind daf�r gedacht, Zeilen eines Absatzes, Zeilen eines
    Dokuments bzw. Zeilen einer Seite nachtr�glich zu dekorieren. Derzeit
    sind nur Dekorationen f�r atomare Zeilenelemente eines Absatzes
    implementiert.

    Das erste Argument <src-arg|foo> ist ein Makro, dass auf alle Boxen in
    einer Zeile angewendet wird und das zweite Argument <src-arg|content> ist
    der Teil des Absatzes auf die die Dekoration angewendet wird. Beispiel
    kann

    <\tm-fragment>
      <inactive*|<style-with|src-compact|none|<datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|<arg|body>>>>
    </tm-fragment>

    benutzt werden, um die Boxen in einem Absatz zu zeigen:

    <\quote-env>
      <datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. ><datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Dies
      ist ein gen�gend langer Absatz. >
    </quote-env>

    Wenn man dies in Kombination mit <markup|repeat> benutzt, kann man die
    punktierten Linien der Inhaltsverzeichnisse reproduzieren

    <\tm-fragment>
      <inactive*|<style-with|src-compact|none|<assign|toc-dots|<macro|<style-with|src-compact|none|<datoms|<macro|x|<repeat|<arg|x>|<space|0.2fn>.<space|0.2fn>>>|<htab|5mm>>>>>>>
    </tm-fragment>

    Man beachte, das <markup|datoms> sehr empfindlich ist, da das
    <src-arg|foo>-Makro keinen Zugriff auf den Kontext hat, in dem
    <src-arg|content> gesetzt wird.
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