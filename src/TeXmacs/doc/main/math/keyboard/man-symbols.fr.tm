<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Saisie de symboles math�matiques>

  Dans <apply|TeXmacs>, on acc�de aux caract�res grecs en combinant la touche
  <key|H-> avec une lettre. Par exemple, <key|H-a> produit le caract�re
  <with|mode|math|\<alpha\>> et <key|H-G> le caract�re
  <with|mode|math|\<Gamma\>>. <apply|hyper-link|Souvenez-vous|../../start/man-conventions.fr.tm>
  que la touche <key|F5> est �quivalente � <key|H-> ; vous pouvez donc aussi
  utiliser <key|F5 r> pour obtenir <with|mode|math|\<rho\>>. De m�me,
  <key|F6>, <key|F7>, <key|F8> et <key|S-F6> permettent d'obtenir des
  caract�res gras, calligraphiques, gothiques ou des onciales. Par exemple,
  <key|F8 m> donne <with|mode|math|\<frak-m\>>, <key|S-F6 R> donne <format|no
  line break><with|mode|math|\<bbb-R\>> et <key|F6 F7 Z> donne
  <with|mode|math|\<b-cal-Z\>>.

  Les caract�res grecs peuvent aussi �tre obtenus en tant que
  <space|0.2spc>variantes<space|0.2spc> de caract�res latins en utilisant
  la touche <key|tab>. Par exemple, <key|p tab> donne
  <with|mode|math|\<pi\>>. La touche <key|tab> sert aussi � produire des
  variantes de lettres grecques. Par exemple, <key|H-p tab tab> et <key|p tab
  tab tab> donnent toutes les deux <with|mode|math|\<varpi\>>.

  On peut obtenir de nombreux autres symboles math�matiques par des
  combinaisons de touches <space|0.2spc>�videntes<space|0.2spc>. Par
  exemple, \ <key|- \<gtr\>> donne <with|mode|math|\<rightarrow\>>, <key|- -
  \<gtr\>> donne <with|mode|math|\<longrightarrow\>> et <key|\<gtr\> => donne
  <with|mode|math|\<geqslant\>>. De m�me, <key|\| -> donne
  <with|mode|math|\<vdash\>>, <key|\| - \<gtr\>> donne
  <with|mode|math|\<mapsto\>> et <key|- \<gtr\> \<less\> -> donne
  <with|mode|math|\<rightleftarrows\>>. Voici quelques r�gles g�n�rales
  permettant d'obtenir des variantes de symboles :

  <\description>
    <expand|item*|<key|tab>>est la touche principale pour obtenir des
    variantes. Par exemple, <key|\<gtr\> => donne
    <with|mode|math|\<geqslant\>>, mais <key|\<gtr\> = tab> donne <format|no
    line break><with|mode|math|\<geq\>>. De m�me, <key|\<less\> tab> donne
    <with|mode|math|\<prec\>>, <key|\<less\> tab => donne
    <with|mode|math|\<preccurlyeq\>> et <key|\<less\> tab = tab> donne
    <with|mode|math|\<preceq\>>. <key|P tab tab> donne
    <with|mode|math|\<wp\>> et <key|e tab tab> donne la constante d'Euler. On
    peut utiliser <key|S-tab> pour passer en revue les diff�rents caract�res
    produits par les touches <key|tab> successives.

    <expand|item*|<key|@>>(symbole @) est utilis�e pour inscrire un symbole
    dans un cercle ou un carr�. Par exemple, <key|@ +> donne
    <with|mode|math|\<oplus\>> et <key|@ x> donne
    <with|mode|math|\<otimes\>>. De m�me, <key|@ tab +> donne
    <with|mode|math|\<boxplus\>>.

    <expand|item*|<key|/>>est utilis�e pour les n�gations. Par exemple,
    <key|= /> donne <with|mode|math|\<neq\>> et <key|\<less\> = /> donne
    <with|mode|math|<neg|\<leqslant\>>>. Notez que <key|\<less\> = tab tab />
    donne <with|mode|math|\<nleqq\>>, tandis que <key|\<less\> = tab tab /
    tab> donne <with|mode|math|\<lneqq\>>.

    <expand|item*|<key|!>>est utilis�e apr�s une fl�che pour forcer le
    caract�re suivant � s'inscrire au-dessus ou au-dessous d'elle. Par
    exemple, <key|- - \<gtr\> ^ x> donne <with|mode|math|\<longrightarrow\><rsup|x>
    >, mais <key|- - \<gtr\> ! ^ x> donne
    <with|mode|math|\<longrightarrowlim\><rsup|x>>.
  </description>

  Certains symboles ne peuvent �tre obtenus comme ci-dessus, il faut alors
  utiliser le pr�fixe <key|S-F5>. En voici le tableau :

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|1|-1|2|2|cell
  halign|c>|<cwith|1|-1|4|4|cell halign|c>|<cwith|1|-1|2|2|cell
  rborder|1ln>|<table|<row|<cell|Raccourcis>|<cell|Symboles>|<cell|Raccourcis>|<cell|Symboles>>|<row|<cell|<expand|kbd-symb|a>>|<cell|<with|mode|math|\<amalg\>>>|<cell|>|<cell|>>|<row|<cell|<expand|kbd-symb|n>>|<cell|<with|mode|math|\<cap\>>>|<cell|<expand|kbd-symb|u>>|<cell|<with|mode|math|\<cup\>>>>|<row|<cell|<expand|kbd-symb|v>>|<cell|<with|mode|math|\<vee\>>>|<cell|<expand|kbd-symb|w>>|<cell|<with|mode|math|\<wedge\>>>>>>>|Symboles
  ne pouvant �tre obtenus par les r�gles g�n�rales �dict�es ci-dessus.>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Mich�le Garoche>

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
    <associate|language|french>
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
      <tuple|normal|Symboles ne pouvant �tre obtenus par les r�gles g�n�rales
      �dict�es ci-dessus.|<pageref|gly-1>>
    </associate>
  </collection>
</auxiliary>
