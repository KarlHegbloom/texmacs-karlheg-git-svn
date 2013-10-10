<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Ambiente matem�ticos>

  A <abbr|d.t.d.> <tmdtd|env-math> especifica os ambiente matem�tico que
  podem ser usados no modo texto. Em outras palavra, os ambientes devem ser
  usados no modo texto, por�m seus corpos cont�m f�rmulas matem�ticas ou
  tabelas de f�rmulas matem�ticas.

  <\explain|<markup|equation>>
    Uma equa��o numerada.
  </explain>

  <\explain|<markup|equation*>>
    Uma equa��o sem n�mero.
  </explain>

  <\explain|<markup|eqnarray>>
    Uma matriz de equa��es numeradas (n�o deve ser usada ainda).
  </explain>

  <\explain|<markup|eqnarray*>>
    Uma matriz de equa��es n�o numeradas.
  </explain>

  Dentro do ambiente <markup|eqnarray*>, voc� pode usar a etiqueta
  <markup|eq-number> para numerar a equa��o

  <\warning>
    A numera��o de equa��es dentro de tabelas ainda n�o � exatamente como
    deveria. Em particular, a etiqueta <markup|eqnarray> � equivalente a
    <markup|eqnarray*> no momento. Mais tarde, quando a etiqueta
    <markup|eqnarray> for implementada corretamente, voc� tamb�m ter� uma
    etiqueta <markup|no-number> para suprimir a numera��o de uma equa��o, e
    um arquivo de estilo para numerar as equa��es do lado esquerdo.
  </warning>

  <\warning>
    N�o h� ainda uma op��o para numerar as equa��es do lado esquerdo da
    p�gina. Ainda assim, voc� pode manualmente usar a etiqueta
    <markup|leq-number> para obter o mesmo efeito. Tamb�m existe a etiqueta
    <markup|next-number> que mostra o pr�ximo n�mero e incrementa o contador
    de equa��es.
  </warning>

  <\warning>
    N�s n�o encorajamos o uso dos ambientes do AMS-<TeX> <verbatim|align>,
    <verbatim|gather> e <verbatim|split>. Ainda assim, eles est�o dispon�veis
    sob os nomes de <markup|align>, <markup|gather>, <markup|eqsplit>,
    juntamente com suas variantes <markup|align*>, <markup|gather*> e
    <markup|eqsplit*>. N�s planejamos fornecer no futuro ambientes mais
    poderosos.
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