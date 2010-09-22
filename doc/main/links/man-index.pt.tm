<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Gerando um �ndice>

  Para criar um �ndice, voc� precisa inicialmente adicionar entradas no seu
  documento usando o menu <apply|menu|Insert|Link|Index entry>.
  Posteriormente, voc� deve colocar o cursor no local onde voc� deseja que o
  �ndice seja criado e clicar em <apply|menu|Text|Automatic|Index>. O �ndice
  ser� gerado de forma an�loga ao sum�rio.

  No menu <apply|menu|Insert|Link|Index entry> voc� encontra v�rios tipos de
  entradas para �ndice. As mais simples s�o ``main'', ``sub'', ``subsub'',
  que s�o macros com um, dois e tr�s argumentos, respectivamente. Entradas
  das formas ``sub'', ``subsub'', podem ser usadas para subordinar as
  entradas do �ndice com respeito a outras entradas.

  Uma entrada de �ndice completa necessita de quatro argumentos. O primeiro �
  a chave com a qual a entrada ser� ordenada e ele deve ser uma ``tupla''
  (criada com <key|inactive \<less\>>) na qual o primeiro componente � a
  categoria principal, o segundo uma subcategoria, etc. O segundo argumento
  de uma entrada complexa pode ser vazio ou ent�o ``strong'', indicando que
  esta entrada aparecer� em negrito no �ndice. O terceiro argumento
  normalmente � vazio, por�m se voc� criar duas entradas com o mesmo terceiro
  argumento n�o vazio, ent�o isto criar� uma ``faixa'' de n�meros de p�ginas.
  O quarto argumento, que � novamente uma ``tupla'', � a pr�pria entrada.

  Tamb�m � poss�vel criar uma linha no �ndice sem um n�mero de p�gina
  correspondente, usando ``interjei��o'' em <apply|menu|Insert|Link|Index
  entry>. O primeiro argumento deste macro � a chave para ordena��o da linha
  no �ndice e o segundo argumento cont�m o texto em si. Esta t�cnica pode ser
  �til para criar diferentes se��es ``A'', ``B'', etc. no seu documento.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      �ndice>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Autom�tico>|<with|font
      family|<quote|ss>|�ndice>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      �ndice>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Item do
      �ndice>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
