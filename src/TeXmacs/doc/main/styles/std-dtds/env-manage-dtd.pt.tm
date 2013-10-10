<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Defini��o de novos ambientes>

  A <abbr|d.t.d.> <tmdtd|env-manage> cont�m marca��o de alto n�vel que pode
  ser usada pelo usu�rio para definir novos ambientes para teoremas,
  coment�rios, exerc�cios e figuras:

  <\explain|<markup|new-theorem>>
    Define um ambiente similar a um teorema. Voc� deve especificar o nome
    para o ambiente (como ``experi�ncia'') e o texto correspondente (como
    ``Experi�ncia'').
  </explain>

  <\explain|<markup|new-remark>>
    Semelhante a <markup|new-theorem>, mas para coment�rios.
  </explain>

  <\explain|<markup|new-exercise>>
    Semelhante a <markup|new-theorem>, mas para exerc�cios.
  </explain>

  <\explain|<markup|new-figure>>
    Semelhante a <markup|new-theorem>, mas para figuras (em pares para
    figuras grandes e pequenas).
  </explain>

  Esta <abbr|d.t.d.> tamb�m cont�m marca��o de baixo n�vel para a defini��o
  destes ambientes. Na verdade, a defini��o de novos ambientes para teoremas
  � feita em duas etapas. Na primeira, a etiqueta <markup|new-theorem> �
  usada para especificar qual ambiente deve ser definido. Na segunda etapa,
  (imediatamente antes do documento do usu�rio ser definido), os ambientes
  s�o efetivamente definidos. Este mecanismo torna poss�vel modifiar os
  ambientes com pacotes que s�o processados entre estas duas etapas. Por
  exemplo, a numera��o dos teoremas � modificada desta forma.

  <\warning>
    No momento, voc� deve usar <markup|new-theorem> e etiquetas similares
    apenas dentro de arquivos de estilo ou pacotes pessoais. Se voc� usar
    <markup|new-theorem> diretamente dentro de um documento, a numera��o
    poder� ser incorreta, devido ao esquema com duas etapas descrito acima.
    Esta limita��o ir� desaparecer t�o logo seja poss�vel especificar
    pre�mbulos limpos para documentos <TeXmacs>.
  </warning>

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