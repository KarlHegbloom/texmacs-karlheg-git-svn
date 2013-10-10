<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Listas padr�o>

  As listas padr�o do <TeXmacs> s�o definidas em <tmdtd|std-list>. As listas
  sem numera��o s�o:

  <\explain|<markup|itemize>>
    A marca antes de cada �tem depende do n�vel do aninhamento.
  </explain>

  <\explain|<markup|itemize-minus>>
    Usa <math|-> para a marca.
  </explain>

  <\explain|<markup|itemize-dot>>
    Usa <math|\<bullet\>> para a marca.
  </explain>

  <\explain|<markup|itemize-arrow>>
    Usa <math|\<rightarrow\>> para a marca.
  </explain>

  Listas numeradas correspondem aos seguintes ambientes:

  <\explain|<markup|enumerate>>
    O tipo do n�mero depende do n�vel de aninhamento.
  </explain>

  <\explain|<markup|enumerate-numeric>>
    Numera os itens com 1, 2, 3, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-roman>>
    Numera os itens com i, ii, iii, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-Roman>>
    Numera os itens com I, II, III, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-alpha>>
    Numera os itens com a), b), c), <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-Alpha>>
    Numera os itens com A, B, C, <abbr|etc.>
  </explain>

  Os ambientes abaixo s�o usados para listas descritivas.

  <\explain|<markup|description>>
    O ambiente padr�o para listas descritivas (normalmente
    <markup|description-compact>).
  </explain>

  <\explain|<markup|description-compact>>
    \ Alinha os itens da lista � esquerda e coloca as descri��es
    imediatamente depois deles.
  </explain>

  <\explain|<markup|description-dash>>
    Parecido com <markup|description-compact>, mas usa um \V para separar
    cada item de sua descri��o.
  </explain>

  <\explain|<markup|description-align>>
    Alinha as descri��es � esquerda, enquanto que os itens s�o alinhados �
    direita.
  </explain>

  <\explain|<markup|description-long>>
    Coloca os itens e suas descri��es em linhas diferentes.
  </explain>

  Novos itens em uma lista s�o indicados atrav�s da etiqueta <markup|item> ou
  da etiqueta un�ria <markup|item*>, no caso de descri��es. Desenvolvedores
  podem encontrar ainda outras macros em <tmdtd|std-list> para definir outras
  estruturas de listas, por�m estes macros n�o s�o considerados est�veis.

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