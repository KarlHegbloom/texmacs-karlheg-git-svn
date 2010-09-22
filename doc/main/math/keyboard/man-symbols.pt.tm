<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Digitando s�mbolos matem�ticos>

  Letras gregas s�o inseridas no <apply|TeXmacs> atrav�s da combina��o da
  tecla modificadora hiper <prefix|H-> com uma letra. Por exemplo, <key|H-a>
  gera <with|mode|math|\<alpha\>> e <key|H-G> produz
  <with|mode|math|\<Gamma\>>. <apply|hyper-link|Lembre-se|../../start/man-conventions.pt.tm>
  que a tecla <prefix|math:greek> � equivalente a <prefix|H->, de forma que
  <with|mode|math|\<rho\>> pode ser obtido tamb�m com <key|F5 r>. De forma
  an�loga, <prefix|math:bold>, <prefix|math:cal>, <prefix|math:frak> e <prefix|math:bbb> podem ser usados para
  inserir caracteres em negrito, caligr�ficos, fraktur e blackboard bold. Por
  exemplo, <shortcut|\<frak-m\>> produz <with|mode|math|\<frak-m\>>, <key|S-F6 R> produz
  <format|no line break><with|mode|math|\<bbb-R\>> e <shortcut|\<b-cal-Z\>> produz
  <with|mode|math|\<b-cal-Z\>>.

  Letras gregas tamb�m podem ser obtidas como \ ``varia��es'' das letras
  latinas, atrav�s da tecla <key|var>. Por exemplo, <key|p
  var> gera <with|mode|math|\<pi\>>. A tecla
  <key|var> tamb�m � usada para obter varia��es das pr�prias
  letras gregas. Por exemplo, tanto <key|H-p var> quanto
  <key|p var var> produzem
  <with|mode|math|\<varpi\>>.

  Muitos outros s�mbolos matem�ticos s�o inseridos com combina��es
  ``naturais'' de teclas, por exemplo, \ <key|- \<gtr\>> produz
  <with|mode|math|\<rightarrow\>>, <key|- - \<gtr\>> produz
  <with|mode|math|\<longrightarrow\>> e <key|\<gtr\> => produz
  <with|mode|math|\<geqslant\>>. Analogamente, <key|\| -> produz
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> \ produz
  <with|mode|math|\<mapsto\>> and <key|- \<gtr\> \<less\> -> produz
  <with|mode|math|\<rightleftarrows\>>. Algumas regras gerais s�o v�lidas
  para obter outras varia��es de s�mbolos:

  <\description>
    <expand|item*|<key|var>>� a principal tecla para
    obten��o de varia��es. Por exemplo, <key|\<gtr\> => gera
    <with|mode|math|\<geqslant\>>, mas <key|\<gtr\> = var>
    gera <format|no line break><with|mode|math|\<geq\>>. Da mesma forma,
    <key|\<less\> var> produz <with|mode|math|\<prec\>>,
    <key|\<less\> var => produz
    <with|mode|math|\<preccurlyeq\>> e <key|\<less\> var =
    var> produz <with|mode|math|\<preceq\>>. Al�m disso,
    <key|P var> gera <with|mode|math|\<wp\>> e <key|e
    var> gera a constante <with|mode|math|\<mathe\>=exp(1)>.
    Voc� pode ``circular de volta'' usando <key|S-var>.

    <expand|item*|<key|@>>� usado para colocar s�mbolos dentro de caixas, por
    exemplo <key|@ +> gera <with|mode|math|\<oplus\>> e <key|@ x> gera
    <with|mode|math|\<otimes\>>. Similarmente, <key|@ var +>
    gera <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>� usada para nega��es. Por exemplo, <key|= />
    gera<with|mode|math|\<neq\>>and <key|\<less\> = /> gera
    <with|mode|math|<neg|\<leqslant\>>>. Note que \ <key|\<less\> =
    var var /> gera
    <with|mode|math|\<nleqq\>>, enquanto <key|\<less\> = var
    var / var> gera
    <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>� usado ap�s setas, para for�ar com que super e
    subscritos apare�am sobre e sob as setas. Por exemplo, <key|- - \<gtr\> ^
    x> gera <with|mode|math|\<longrightarrow\><rsup|x> >, mas <key|- -
    \<gtr\> ! ^ x> gera <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  V�rios outros s�mbolos que n�o podem ser inseridos naturalmente como
  descrito acima, podem ser obtidos usando o prefixo <prefix|symbol>. A tabela
  abaixo mostra alguns destes s�mbolos:

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Atalho>|<cell|S�mbolo>|<cell|Atalho>|<cell|S�mbolo>>|<row|<cell|<key|symbol a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<key|symbol n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<key|symbol u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<key|symbol v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<key|symbol w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Alguns
  s�mbolos que n�o podem ser obtidos usando-se as regras gerais.>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
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
    <associate|language|portuguese>
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
      <tuple|normal|Alguns s�mbolos que n�o podem ser obtidos usando-se as
      regras gerais.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
