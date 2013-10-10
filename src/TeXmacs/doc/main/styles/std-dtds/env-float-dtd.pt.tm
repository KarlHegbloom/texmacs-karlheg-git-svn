<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambientes para objetos flutuantes>

  A <abbr|d.t.d.> <tmdtd|env-float> fornece marca��o para objetos flutuantes.
  A etiqueta seguinte � a �nica de alto n�vel.

  <\explain|<markup|footnote>>
    Cria uma nota de rodp�.
  </explain>

  As etiquetas de baixo n�vel a seguir podem ser usadas para a defini��o de
  ambientes de alto n�vel para figuras e tabelas, como <markup|big-figure>,
  <markup|small-figure>, <markup|big-table> e <markup|small-table>:

  <\explain|<markup|render-small-figure>>
    Um macro para exibi��o de uma figura pequena. Os argumento s�o um nome
    curto (como ``figura'' ou ``tabela'') para a lista de figuras, seu nome
    verdadeiro (como ```Figura 2.3'' ou ``Tabela <no-break>5''), a pr�pria
    figura e um texto para a legenda.
  </explain>

  <\explain|<markup|render-big-figure>>
    Uma varia��o de <markup|render-small-figure> para exibi��o de figuras
    grandes.
  </explain>

  As etiquetas abaixo podem ser usadas para modificar a apar�ncia do texto em
  torno das figuras, tabelas e notas de rodap�.

  <\explain|<markup|figure-name>>
    Um macro que controla a apar�ncia do texto``Figura''. O padr�o � usar
    negrito.
  </explain>

  <\explain|<markup|figure-sep>>
    O separador entre a figura e seu n�me e a legenda. O padr�o � um ponto
    seguido por um espa�o.
  </explain>

  <\explain|<markup|footnote-sep>>
    O separador entre o n�mero de uma nota de rodap� e o texto. O padr�o � um
    ponto seguido por um espa�o.
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