<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Gera��o de conte�do autom�tica>

  A <abbr|d.t.d.> <tmdtd|std-automatic> cont�m defini��es para a gera��o de
  conte�do autom�tico como �ndices e bibliografia assim como para a
  apresenta��o deste material. As etiquetas abaixo s�o usadas para
  bibliografias:

  <\description>
    <expand|item*|<markup|cite>>Uma fun��o com um n�mero arbitr�rio de
    argumentos. Cada argumento � uma cita��o correspondente a um item em um
    arquivo BiB-<TeX>. As cita��es s�o mostradas da mesma forma em que s�o
    referenciadas na bibliografia e elas tamb�m fornecem hiperliga��es para
    as refer�ncias correspondentes. As cita��es s�o mostradas como pontos de
    interroga��o se voc� n�o gerou a bibliografia.

    <expand|item*|<markup|nocite*>>Semelhante a <markup|cite>, mas as
    cita��es n�o s�o exibidas no texto principal.

    <expand|item*|<markup|bibitem*>>Uma fun��o que especifica como exibir um
    texto na bibliografia.
  </description>

  As etiquetas abaixo s�o usadas para compilar um �ndice:

  <\description>
    <expand|item*|<markup|toc-main-1>>Uma fun��o com um argumento para criar
    uma entrada inicial no �ndice. Esta fun��o pode por exemplo ser usada
    quando um livro � composto de v�rias partes.

    <expand|item*|<markup|toc-main-2>>Uma fun��o com um argumento para criar
    uma entrada principal no �ndice. Esta fun��o normalmente � usada para
    cap�tulos.

    <expand|item*|<markup|toc-normal-1>>Uma fun��o com um argumento para
    criar uma entrada normal no �ndice. Esta fun��o normalmente � usada para
    sess�es.

    <expand|item*|<markup|toc-normal-2>>Semelhante a <markup|toc-normal-2>,
    para entradas menos importantes como subsess�es.

    <expand|item*|<markup|toc-normal-3>>Semelhante a <markup|toc-normal-3>,
    para entradas ainda menos importantes.

    <expand|item*|<markup|toc-small-1>>Usada para entradas pouco importantes,
    tais como par�grafos (podem ser ignoradas.)

    <expand|item*|<markup|toc-small-2>>Usada para entradas ainda menos
    importantes do que <markup|toc-small-1>, como subpar�grafos.

    <expand|item*|<markup|toc-dots>>A separa��o entre uma entrada do �ndice e
    o n�mero da p�gina correspondente. Normalmente, s�o usados pontos
    horizontais.
  </description>

  As seguintes etiquetas s�o usadas para �ndices remiss�vos (no final do
  documento):

  <\description>
    <expand|item*|<markup|index>>Uma fun��o com um argumento <var|x>, que
    insere <var|x> no �ndice como uma entrada principal.

    <expand|item*|<markup|subindex>>Uma fun��o com dois argumentos, <var|x> e
    <var|y>, que insere <var|y> no �ndice como uma entrada subordinada a
    <var|x>.

    <expand|item*|<markup|subsubindex>>Uma fun��o com tr�s argumentos,
    <var|x>, <var|y> e <var|z>, que insere <var|z> no �ndice como uma entrada
    subordinada a <var|y>, que � subordinada a <var|x>.

    <expand|item*|<markup|index-complex>>Uma fun��o com quatro argumentos,
    <var|key>, <var|how>, <var|range>, <var|entry>, que � documentada na
    sess�o sobre <apply|hyper-link|gera��o de �ndices|
    ../../links/man-index.pt.tm>.

    <expand|item*|<markup|index-line>>Esta fun��o aceita uma <var|chave> como
    argumento, que diz como ordenar a entrada, e a <var|entry> em si. N�o �
    gerado um n�mero de p�gina.

    <expand|item*|<markup|index-1>>Macro com uma entrada no �ndice e um
    n�mero de p�gina, que � usado para exibir uma entrada principal do
    �ndice.

    <expand|item*|<markup|index-1*>>Semelhante a <markup|index-1>, mas sem o
    n�mero da p�gina.

    <expand|item*|<markup|index-<with|mode|math|n>>>(com <with|mode|math|n>
    entre <with|mode|math|1> e <with|mode|math|5>): macro com uma entrada no
    �ndice e um n�mero de p�gina, que � usada para apresenta��o de uma
    entrada do �ndice de n�vel <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Semelhante a
    <markup|index-<with|mode|math|n>>, mas sem o n�mero da p�gina.

    <expand|item*|<markup|index-dots>>O macro que produz os pontos entre as
    entradas do �ndice e os n�meros de p�gina correspondentes.
  </description>

  As etiquetas abaixo s�o usadas para gloss�rios:

  <\description>
    <expand|item*|<markup|glossary>>Uma fun��o que insere seu argumento �nico
    no gloss�rio.

    <expand|item*|<markup|glossary-dup>>Para criar um n�mero de p�gina
    adicional para uma entrada que j� tinha sido inserida no gloss�rio.

    <expand|item*|<markup|glossary-explain>>Uma fun��o para inserir uma
    entrada do gloss�rio com sua explica��o.

    <expand|item*|<markup|glossary-line>>Insere um entrada no gloss�rio sem
    um n�mero de p�gina.

    <expand|item*|<markup|glossary-1>>Macro para apresenta��o de uma entrada
    do gloss�rio com o seu n�mero de p�gina correspondente.

    <expand|item*|<markup|glossary-2>>Macro para apresenta��o de uma entrada
    do gloss�rio, sua explica��o, e seu n�mero de p�gina.

    <expand|item*|<markup|glossary-dots>>O macro que produz os pontos entre a
    entrada no gloss�rio e seu n�mero de p�gina correspondente.
  </description>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-automatic>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nocite*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|bibitem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-1>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-main-2>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-1>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-2>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-normal-3>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-2>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-small-1>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|toc-dots>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-35>>
    </associate>
  </collection>
</auxiliary>
