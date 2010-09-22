<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematische Symbole eingeben>

  Die griechischen Buchstaben k�nnen in <TeXmacs> eingegeben werden, indem
  die <em|Hyper-Modifiertaste> <key|H-> mit einem Buchstaben kombiniert wird.
  Beispielsweise ergibt <key|H-a> \ <with|mode|math|\<alpha\>> und <key|H-G>
  gibt <with|mode|math|\<Gamma\>>. <hyper-link|Erinnern Sie
  sich|../../start/man-conventions.de.tm>, dass <shortcut|math:greek> zu <key|H->
  �quivalent ist, so dass man <with|mode|math|\<rho\>> mit <key|F5 r>
  erhalten kann.

  In �hnlicher Weise k�nnen <shortcut|math:bold>, <shortcut|math:cal>, <shortcut|math:frak> und <shortcut|math:bbb>
  benutzt werden, um in dieser Reihenfolge,
  <with|mode|math|\<b-f\>\<b-e\>\<b-t\>\<b-t\>\<b-e\>\<b-n\>
  \<b-T\>\<b-e\>\<b-x\>\<b-t\>>, <with|mode|math|\<cal-K\>\<cal-A\>\<cal-L\>\<cal-L\>\<cal-I\>\<cal-G\>\<cal-R\>\<cal-A\>\<cal-F\>\<cal-I\>\<cal-S\>\<cal-C\>\<cal-H\>\<cal-E\>\<cal-N\>
  \ \ \ \ \<cal-T\>\<cal-E\>\<cal-X\>\<cal-T\>>,
  <with|mode|math|\<frak-F\>\<frak-r\>\<frak-a\>\<frak-k\>\<frak-t\>\<frak-u\>\<frak-r\>>
  und <with|mode|math|\<bbb-F\>\<bbb-E\>\<bbb-T\>\<bbb-T\>\<bbb-E\>
  \ \ \ \ \ \<bbb-T\>\<bbb-A\>\<bbb-F\>\<bbb-E\>\<bbb-L\>\<bbb-S\>\<bbb-C\>\<bbb-H\>\<bbb-R\>\<bbb-F\>\<bbb-T\>>
  zu erzeugen. \ <shortcut|\<frak-m\>> gibt <with|mode|math|\<frak-m\>>, <key|S-F6 R>
  gibt <no-break><with|mode|math|\<bbb-R\>> und <shortcut|\<b-calZ\>> ergibt
  <with|mode|math|\<b-cal-Z\>>.

  Griechische Buchstaben k�nnen auch als \RVarianten`` von lateinischen
  Buchstaben mit der \ <key|<key-variant>>-Taste erzeugt werden.
  Beispielsweise liefert <key|p <key-variant>> im Mathematik-Modus
  <with|mode|math|\<pi\>>. Die <key|<key-variant>>-Taste kann auch dazu
  gebraucht werden, Varianten von griechischen Buchstaben zu erzeugen. Z.B.
  <key|H-p tab <key-variant>> und <key|p <key-variant> <key-variant> tab>
  ergeben <with|mode|math|\<varpi\> >, <key|H-p <key-variant>> und <key|p
  <key-variant> tab> \ <with|mode|math|\<mathpi\>>, \ <key|H-p> und <key|p
  tab> \ <with|mode|math|\<pi\>>.

  \;

  Viele andere Buchstaben erh�lt man durch \Rnahe liegende``
  Tastenkombinationen. Beispielsweise ergibt <key|- \<gtr\>>
  \ <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>>
  \ <with|mode|math|\<longrightarrow\>> und <key|\<gtr\> =>
  \ <with|mode|math|\<geqslant\>>. Oder <key|\| -> ergibt
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>>
  \ <with|mode|math|\<mapsto\>> und <key|- \<gtr\> \<less\> ->
  \ <with|mode|math|\<rightleftarrows\>>.\ 

  Es gibt einige Regeln:

  <\description>
    <item*|<key|tab>>ist die wichtigste Taste zur Erzeugung von Varianten.
    Z.B. <key|\<gtr\> => yields <with|mode|math|\<geqslant\>>, aber
    <key|\<gtr\> = <key-variant>> erzeugt <with|mode|math|\<geq\>>.
    \ <key|\<less\> <key-variant>> gibt <with|mode|math|\<prec\>>,
    <key|\<less\> <key-variant> => gibt <with|mode|math|\<preccurlyeq\>> und
    <key|\<less\> <key-variant> = <key-variant>> gibt
    <with|mode|math|\<preceq\>>. Also, <key|P <key-variant> tab> erzeugt
    <with|mode|math|\<wp\>> und <key|e <key-variant> tab> ergiebt die
    Konstante <with|mode|math|\<mathe\>=exp(1)>. Sie k�nne mit
    <key|S-<key-variant>> wieder zur�ckgehen.

    <item*|<key|@>>liefert Zeichen in Kreisen und Quadraten. Z.B. <key|@ +>
    \ ergibt <with|mode|math|\<oplus\>>, <key|@ x>
    \ <with|mode|math|\<otimes\>>und <key|@ <key-variant> +> gibt
    <with|mode|math|\<boxplus\>>.

    <item*|<key|/>>wird f�r Negatioen benutzt. <key|= /> ergibt
    <with|mode|math|\<neq\>> und <key|\<less\> = />
    \ <with|mode|math|<neg|\<leqslant\>>>. Beachte, dass \ <key|\<less\> =
    <key-variant> <key-variant> /> <with|mode|math|\<nleqq\>> ergibt, w�hrend
    <key|\<less\> = <key-variant> <key-variant> / <key-variant>>
    \ <with|mode|math|\<lneqq\>> erzeugt.

    <item*|<key|!>>wird nach Pfeilen benutzt, um untere und obere Indices
    direkt ober- bzw. unterhalb der Pfeile zu plazieren: \ <key|- - \<gtr\> ^
    x> ergibt <with|mode|math|\<longrightarrow\><rsup|x> >, aber <key|- -
    \<gtr\> ! ^ x> ergibt <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Einige andere Symbole, die nicht durch \Rnahe liegende``
  Tastenkombinationen erzeugt werden k�nnen, werden mit dem Pr�fix <shortcut|symbol>
  eingegeben:

  <big-table|<descriptive-table|<tformat|<cwith|1|-1|2|2|cell-halign|c>|<cwith|1|-1|4|4|cell-halign|c>|<cwith|1|-1|2|2|cell-rborder|1ln>|<table|<row|<cell|Kurzbefehl>|<cell|Symbol>|<cell|Kurzbefehl>|<cell|Symbol>>|<row|<cell|<kbd-symb|a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<kbd-symb|n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<kbd-symb|u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<kbd-symb|v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<kbd-symb|w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Einige
  Symbole, die nicht auf naheliegende Weise mit Tastenkombinationen erzeugt
  werden k�nnen.>

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