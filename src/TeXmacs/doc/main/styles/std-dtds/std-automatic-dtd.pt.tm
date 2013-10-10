<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Gera��o de conte�do autom�tica>

  A <abbr|d.t.d.> <tmdtd|std-automatic> cont�m defini��es para a gera��o de
  conte�do autom�tico como �ndices e bibliografia assim como para a
  apresenta��o deste material. As etiquetas abaixo s�o usadas para
  bibliografias:

  <\explain|<markup|cite>>
    Uma fun��o com um n�mero arbitr�rio de argumentos. Cada argumento � uma
    cita��o correspondente a um item em um arquivo BiB-<TeX>. As cita��es s�o
    mostradas da mesma forma em que s�o referenciadas na bibliografia e elas
    tamb�m fornecem hiperliga��es para as refer�ncias correspondentes. As
    cita��es s�o mostradas como pontos de interroga��o se voc� n�o gerou a
    bibliografia.
  </explain>

  <\explain|<markup|nocite*>>
    Semelhante a <markup|cite>, mas as cita��es n�o s�o exibidas no texto
    principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Uma fun��o que especifica como exibir um texto na bibliografia.
  </explain>

  As etiquetas abaixo s�o usadas para compilar um �ndice:

  <\explain|<markup|toc-main-1>>
    Uma fun��o com um argumento para criar uma entrada inicial no �ndice.
    Esta fun��o pode por exemplo ser usada quando um livro � composto de
    v�rias partes.
  </explain>

  <\explain|<markup|toc-main-2>>
    Uma fun��o com um argumento para criar uma entrada principal no �ndice.
    Esta fun��o normalmente � usada para cap�tulos.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Uma fun��o com um argumento para criar uma entrada normal no �ndice. Esta
    fun��o normalmente � usada para sess�es.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Semelhante a <markup|toc-normal-2>, para entradas menos importantes como
    subsess�es.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Semelhante a <markup|toc-normal-3>, para entradas ainda menos
    importantes.
  </explain>

  <\explain|<markup|toc-small-1>>
    Usada para entradas pouco importantes, tais como par�grafos (podem ser
    ignoradas.)
  </explain>

  <\explain|<markup|toc-small-2>>
    Usada para entradas ainda menos importantes do que <markup|toc-small-1>,
    como subpar�grafos.
  </explain>

  <\explain|<markup|toc-dots>>
    A separa��o entre uma entrada do �ndice e o n�mero da p�gina
    correspondente. Normalmente, s�o usados pontos horizontais.
  </explain>

  As seguintes etiquetas s�o usadas para �ndices remiss�vos (no final do
  documento):

  <\explain|<markup|index>>
    Uma fun��o com um argumento <var|x>, que insere <var|x> no �ndice como
    uma entrada principal.
  </explain>

  <\explain|<markup|subindex>>
    Uma fun��o com dois argumentos, <var|x> e <var|y>, que insere <var|y> no
    �ndice como uma entrada subordinada a <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Uma fun��o com tr�s argumentos, <var|x>, <var|y> e <var|z>, que insere
    <var|z> no �ndice como uma entrada subordinada a <var|y>, que �
    subordinada a <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Uma fun��o com quatro argumentos, <var|key>, <var|how>, <var|range>,
    <var|entry>, que � documentada na sess�o sobre <hlink|gera��o de �ndices|
    ../../links/man-index.pt.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Esta fun��o aceita uma <var|chave> como argumento, que diz como ordenar a
    entrada, e a <var|entry> em si. N�o � gerado um n�mero de p�gina.
  </explain>

  <\explain|<markup|index-1>>
    Macro com uma entrada no �ndice e um n�mero de p�gina, que � usado para
    exibir uma entrada principal do �ndice.
  </explain>

  <\explain|<markup|index-1*>>
    Semelhante a <markup|index-1>, mas sem o n�mero da p�gina.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (com <math|n> entre <math|1> e <math|5>): macro com uma entrada no �ndice
    e um n�mero de p�gina, que � usada para apresenta��o de uma entrada do
    �ndice de n�vel <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Semelhante a <markup|index-<math|n>>, mas sem o n�mero da p�gina.
  </explain>

  <\explain|<markup|index-dots>>
    O macro que produz os pontos entre as entradas do �ndice e os n�meros de
    p�gina correspondentes.
  </explain>

  As etiquetas abaixo s�o usadas para gloss�rios:

  <\explain|<markup|glossary>>
    Uma fun��o que insere seu argumento �nico no gloss�rio.
  </explain>

  <\explain|<markup|glossary-dup>>
    Para criar um n�mero de p�gina adicional para uma entrada que j� tinha
    sido inserida no gloss�rio.
  </explain>

  <\explain|<markup|glossary-explain>>
    Uma fun��o para inserir uma entrada do gloss�rio com sua explica��o.
  </explain>

  <\explain|<markup|glossary-line>>
    Insere um entrada no gloss�rio sem um n�mero de p�gina.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro para apresenta��o de uma entrada do gloss�rio com o seu n�mero de
    p�gina correspondente.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro para apresenta��o de uma entrada do gloss�rio, sua explica��o, e
    seu n�mero de p�gina.
  </explain>

  <\explain|<markup|glossary-dots>>
    O macro que produz os pontos entre a entrada no gloss�rio e seu n�mero de
    p�gina correspondente.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>