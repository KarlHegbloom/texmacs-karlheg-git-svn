<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Standard lists>

  Las listas <TeXmacs> estandar est�n definidas en <tmdtd|std-list>. Las
  listas no numeradas son:

  <\explain|<markup|itemize>>
    La etiqueta antes de cada item depende de la profunidad de anidamiento.
  </explain>

  <\explain|<markup|itemize-minus>>
    Usa <math|-> para la etiqueta.
  </explain>

  <\explain|<markup|itemize-dot>>
    Usa <math|\<bullet\>> para la etiqueta.
  </explain>

  <\explain|<markup|itemize-arrow>>
    Usa <math|\<rightarrow\>> para la etiqueta.
  </explain>

  Las lista numeradas corresponden a los siguientes entornos:

  <\explain|<markup|enumerate>>
    La clase de n�mero antes de cada item depende de la profunidad de
    anidamiento.
  </explain>

  <\explain|<markup|enumerate-numeric>>
    Numera los items por 1, 2, 3, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-roman>>
    Numera los items por i, ii, iii, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-Roman>>
    Numera los items por I, II, III, <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-alpha>>
    Numera los items por a), b), c), <abbr|etc.>
  </explain>

  <\explain|<markup|enumerate-Alpha>>
    Numera los items por A, B, C, <abbr|etc.>
  </explain>

  Los siguients entornos puedes ser usadas para listas descriptivas.

  <\explain|<markup|description>>
    El entorno por defecto para listas descriptivas (usualmente
    <markup|description-compact>).
  </explain>

  <\explain|<markup|description-compact>>
    Alinea los lados izquierdos de los items en las listas y pone sus
    descripciones prontamente despu�s de estos.
  </explain>

  <\explain|<markup|description-dash>>
    Similar a <markup|description-compact>, pero usa un \V para seperar cada
    item de su descripci�n.
  </explain>

  <\explain|<markup|description-align>>
    Alinea los lados izquierdos de las descripciones, mientras al�nea los
    items a la derecha.
  </explain>

  <\explain|<markup|description-long>>
    Pone los items y sus descripciones en l�neas distintas.
  </explain>

  Los nuevos items en una lista son indicados a trav�s de la etiqueta
  <markup|item> o la etiqueta unaria <markup|item*> en el caso de las
  descripcioones. Los desarrolladores tambi�n encontrar�n unos pocos macros
  adicionales, pero inestables en <tmdtd|std-list> para definir estructuras
  de lista adicionales.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <tmdoc-license|El permiso est� garantizado para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>