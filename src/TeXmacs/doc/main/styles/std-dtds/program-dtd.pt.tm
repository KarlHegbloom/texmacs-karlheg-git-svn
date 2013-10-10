<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Marca��o especial para programas e sess�es interativas>

  A <abbr|d.t.d.> <tmdtd|program> fornece principalmente os seguintes
  ambientes para �lgebra computacional:

  <\explain|<markup|session>>
    Macro com tr�s argumentos: a linguagem de �lgebra computacional, o nome
    da sess�oe e o pr�prio corpo da sess�o.
  </explain>

  <\explain|<markup|input>>
    Macro com dois argumentos: um prompt e a pr�pria entrada.
  </explain>

  <\explain|<markup|output>>
    Macro com o corpo da sa�da como seu argumento.
  </explain>

  Na verdade, estes ambientes est�o baseados em ambientes da forma
  <markup|<em|lan>-session>, <markup|<em|lan>-input> e
  <markup|<em|lan>-output> para cada linguagem <verbatim|<em|lan>>.

  A <abbr|d.t.d.> <tmdtd|program> tamb�m fornece alguma marca��o para
  diagrama��o de programas de computador. Estas etiquetas devem ser
  consideradas, no entanto, muito vol�teis, j� que pretendemos substitu�-las
  por um conjunto de etiquetas bem mais detalhado:

  <\explain|<markup|algorithm>>
    Macro com dois argumentos, o nome do algoritmo e o algoritmo em si,
    possivelmente acoplado � sua especifica��o.
  </explain>

  <\explain|<markup|body>>
    O corpo do algoritmo.
  </explain>

  <\explain|<markup|indent>>
    Para recuar parte do algoritmo.
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