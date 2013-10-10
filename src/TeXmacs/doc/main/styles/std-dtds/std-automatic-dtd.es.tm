<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Generaci�n autom�tica de contenido>

  El \ <abbr|d.t.d.> <tmdtd|std-automatic> se especifica para la generaci�n
  autom�tica de contenido auxiliar como tables de contenidos y bibliograf�as,
  as� como tambi�n para la presentaci�n de tal contenido auxiliar. Las
  siguientes etiquetas son usadas para las bibliograf�as:

  <\explain|<markup|cite>>
    Una funci�n con un n�mero arbitrario de argumentos. Cada argumento es una
    citaci�n correspondients a un item en un archivo BiB-<TeX>. Las
    citaciones son mostradas en la misma forma como ellas son referenciadas
    en la bibliograf�a y tambi�n proveen hiperenlaces a las referencias
    correspondientes. Las citaciones son mostradas como macas de pregunta si
    usted no ha generado la bibliograf�a.
  </explain>

  <\explain|<markup|nocite*>>
    Similar a <markup|cite>, pero las citaciones no son mostradas en el texto
    principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Una funci�n que especifica como mostrar un item en la bibliograf�a.
  </explain>

  Las siguientes etiquetas son usadas para compilar tablas de contenidos:

  <\explain|<markup|toc-main-1>>
    Una funci�n con un argumento para crear entradas primordiales en la tabla
    de contenidos. Esta funci�n puede por ejemplo ser usada cuando un libro
    consiste de varias partes.
  </explain>

  <\explain|<markup|toc-main-2>>
    Una funci�n con un argumento para crear una entrada principal en una
    tabla de contenidos. Esta funci�n es usada regularmente para cap�tulos.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Una funci�n con un argumento para crear una entrada normal en la tabla de
    contenidos. Esta funci�n es frecuentemente usada para secciones.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Similar a <markup|toc-normal-2> para entradas menos importantes como en
    subsecciones.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Similar a <markup|toc-normal-3> para entradas incluso menos importantes
    como subsubsecciones.
  </explain>

  <\explain|<markup|toc-small-1>>
    Usada para entradas no muy importantes tales como p�rrafos (puede ser
    ignorada).
  </explain>

  <\explain|<markup|toc-small-2>>
    Usada para entradas incluso menos importantes tales como subp�rrafos.
  </explain>

  <\explain|<markup|toc-dots>>
    La separaci�n entre una entrada en la tabla de contenidos y el
    correspondiente n�mero de p�gina. Por defecto, usamos puntos
    horizontales.
  </explain>

  Las siguientes etiquetas son usadas para �ndices:

  <\explain|<markup|index>>
    Una funci�n con un argumento <var|x>, que inserta <var|x> en el �ndice
    como una entrada principal.
  </explain>

  <\explain|<markup|subindex>>
    Una funci�n con dos argumentos <var|x> y <var|y>, que inserta <var|y> en
    el �ndice como una subentrada de <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Una funci�n con tres argumentos <var|x>, <var|y> y <var|z>, que inserta
    <var|z> en el �ndice como una subentrada de <var|y>, que es a su vez una
    subentrada de <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Una funci�n con cuatro argumentos <var|key>, <var|how>, <var|range>,
    <var|entry>, que est� documentada en la secci�n acerca de
    <hlink|generaci�n del �ndice|../../links/man-index.es.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Esta funci�n toma un argumento <var|key>, que dice como ordenar una
    entrada y el argumento actual <var|entry>. Ning�n n�mero de p�gina es
    generado.
  </explain>

  <\explain|<markup|index-1>>
    Macro con una entrada �ndice y un n�mero de p�gina, que es usada para
    visualizar una entrada principal del index en el �ndice.
  </explain>

  <\explain|<markup|index-1*>>
    Similar a <markup|index-1>, pero sin el n�mero de p�gina.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (con <math|n> entre <math|1> y <math|5>): macro con una entrada en el
    �ndice y un n�mero de p�gina, que es usado para visualizar una entrada en
    el �ndice de nivel <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Similar a <markup|index-<math|n>>, pero sin el n�mero de p�gina.
  </explain>

  <\explain|<markup|index-dots>>
    El macro que produce los puntos entre una entrada al �ndice y el (los)
    correspondiente(s) n�mero(s) de p�gina.
  </explain>

  Las siguientes etiquetas son usadas en los glosarios:

  <\explain|<markup|glossary>>
    Una funci�n que inserta su �nico argumento en el glosario.
  </explain>

  <\explain|<markup|glossary-dup>>
    Para crear un n�mero de p�gina adicional para una entrada que hab�a sido
    insertada antes.
  </explain>

  <\explain|<markup|glossary-explain>>
    Una funci�n para insertar una entrada en el glorario con su explicaci�n.
  </explain>

  <\explain|<markup|glossary-line>>
    Inserta una entrada de glosario sin n�mero de p�gina.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro for mostrar una entrada de glosario \ y su n�mero de p�gina
    correspondiente.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro para presentar una entrada de glosario, su explicaci�n y su n�mero
    de p�gina.
  </explain>

  <\explain|<markup|glossary-dots>>
    El macro que produce los putnos entre una entrada del glosario y el (los)
    correspondiente(s) n�mero(s) de p�gina.
  </explain>

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
    <associate|language|spanish>
  </collection>
</initial>