<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Defini��o de novos ambientes>

  A <abbr|d.t.d.> <tmdtd|env-manage> cont�m marca��o de alto n�vel que pode
  ser usada pelo usu�rio para definir novos ambientes para teoremas,
  coment�rios, exerc�cios e figuras:

  <\description>
    <expand|item*|<markup|newtheorem>>Define um ambiente similar a um
    teorema. Voc� deve especificar o nome para o ambiente (como
    ``experi�ncia'') e o texto correspondente (como ``Experi�ncia'').

    <expand|item*|<markup|newremark>>Semelhante a <markup|newtheorem>, mas
    para coment�rios.

    <expand|item*|<markup|newexercise>>Semelhante a <markup|newtheorem>, mas
    para exerc�cios.

    <expand|item*|<markup|newfigure>>Semelhante a <markup|newtheorem>, mas
    para figuras (em pares para figuras grandes e pequenas).
  </description>

  Esta <abbr|d.t.d.> tamb�m cont�m marca��o de baixo n�vel para a defini��o
  destes ambientes. Na verdade, a defini��o de novos ambientes para teoremas
  � feita em duas etapas. Na primeira, a etiqueta <markup|newtheorem> � usada
  para especificar qual ambiente deve ser definido. Na segunda etapa,
  (imediatamente antes do documento do usu�rio ser definido), os ambientes
  s�o efetivamente definidos. Este mecanismo torna poss�vel modifiar os
  ambientes com pacotes que s�o processados entre estas duas etapas. Por
  exemplo, a numera��o dos teoremas � modificada desta forma.

  <\warning>
    No momento, voc� deve usar <markup|newtheorem> e etiquetas similares
    apenas dentro de arquivos de estilo ou pacotes pessoais. Se voc� usar
    <markup|newtheorem> diretamente dentro de um documento, a numera��o
    poder� ser incorreta, devido ao esquema com duas etapas descrito acima.
    Esta limita��o ir� desaparecer t�o logo seja poss�vel especificar
    pre�mbulos limpos para documentos <TeXmacs>.
  </warning>

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
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-manage>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newremark>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newexercise>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newfigure>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
