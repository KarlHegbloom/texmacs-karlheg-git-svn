<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Tecleando s�mbolos matem�ticos>

  Los caracteres griegos son obtenidos en <apply|TeXmacs> por la combinaci�n
  de la tecla modificadora <prefix|H-> con una letra. Por ejemplo, <key|H-a>
  produce <with|mode|math|\<alpha\>> y <key|H-G> produce
  <with|mode|math|\<Gamma\>>. <apply|hyper-link|Recuerde|../../start/man-conventions.es.tm>
  que la tecla <prefix|math:greek> es equivalente a <prefix|H->, as� que
  <with|mode|math|\<rho\>> puede tambi�n ser obtenido tecleando
  <with|mode|math|\<rho\>>. Similarmente, <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> y
  <prefix|math:bbb> pueden ser usados a fin de teclear caract�res resaltados,
  caligr�ficos, fraktur y blackboard. Por ejmplo <shortcut|\<frak-m\>> produce
  <with|mode|math|\<frak-m\>>, <key|S-F6 R> produce
  <with|mode|math|\<bbb-R\>> y <shortcut|\<b-calZ\>> produce
  <with|mode|math|\<b-cal-Z\>>.

  Los caracteres griegos pueden tambi�n ser obtenidos como ``variantes'' de
  caracteres latinos usando la tecla <key|tab>. Por ejemplo, <key|p tab>
  produce <with|mode|math|\<pi\>>. La tecla <key|tab> es tamb�en usada para
  obtener variantes de las letras griegas en s� mismas. Por ejemplo, tanto
  <key|H-p tab> como <key|p tab tab> producen <with|mode|math|\<varpi\>>.

  Muchos otros s�mbolos matem�ticos son obtenidos por combinaciones
  ``naturales'' de teclas. Por ejemplo \ <key|- \<gtr\>> produce
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> produce
  <with|mode|math|\<longrightarrow\>> y <key|\<gtr\> => produce
  <with|mode|math|\<geqslant\>>. Similarmente, <key|\| -> produce
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> produce
  <with|mode|math|\<mapsto\>> y <key|- \<gtr\> \<less\> -> produce
  <with|mode|math|\<rightleftarrows\>>. Algunas reglas generales se mantienen
  a fin de obtener variantes de s�mbolos:

  <\description>
    <expand|item*|<key|tab>>es la tecla principal para obtener variantes. Por
    ejemplo, <key|\<gtr\> => produce <with|mode|math|\<geqslant\>>, pero
    <key|\<gtr\> = tab> yields <format|no line
    break><with|mode|math|\<geq\>>. Similarmente, <key|\<less\> tab> produce
    <with|mode|math|\<prec\>>, <key|\<less\> tab => produce
    <with|mode|math|\<preccurlyeq\>> y <key|\<less\> tab = tab> produce
    <with|mode|math|\<preceq\>>. Tambi�n, <key|P tab> produce
    <with|mode|math|\<wp\>> y <key|e tab> produce la constante
    <with|mode|math|\<mathe\>=exp(1)>. Usted puede ``realizar el ciclo hacia
    atr�s'' using <key|S-tab>.

    <expand|item*|<key|@>>es usada para poner s�mbolos dentro de
    circunferencias o cuadrados. Por ejemplo, <key|@ +> produce
    <with|mode|math|\<oplus\>> y <key|@ x> yields
    <with|mode|math|\<otimes\>>. An�logamente, <key|@ tab +> produce
    <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>es usado para las negaciones. Por ejemplo, <key|=
    /> produce <with|mode|math|\<neq\>> and <key|\<less\> = /> produce
    <with|mode|math|<neg|\<leqslant\>>>. Note que <key|\<less\> = tab tab />
    produce <with|mode|math|\<nleqq\>>, mientras <key|\<less\> = tab tab /
    tab> produce <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>es usado despu�s de las flechas a fin de forzar los
    super�ndice a ser colocados arriba o abajo de la flecha. Por ejemplo,
    <key|- - \<gtr\> ^ x> produce <with|mode|math|\<longrightarrow\><rsup|x>
    >, pero <key|- - \<gtr\> ! ^ x> produce
    <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Varios otros s�mbolos que no pueden ser ingresados manualmente en la forma
  anterior son obtenidos usando el prefijo <prefix|symbol>. A continuaci�n se
  muestra una peque�a tabla de tales s�mbolos:

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Atajo>|<cell|S�mbolo>|<cell|Atajo>|<cell|S�mbolo>>|<row|<cell|<expand|kbd-symb|a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<expand|kbd-symb|n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<expand|kbd-symb|u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<expand|kbd-symb|v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<expand|kbd-symb|w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Algunos
  s�mboloss que no pueden ser obtenidos usando reglas generales en una forma
  natural.>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas|�lvaro Cantero Tejero|Pablo Ruiz M�zquiz|David Moriano Garcia>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Algunos s�mboloss que no pueden ser obtenidos usando
      reglas generales en una forma natural.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
