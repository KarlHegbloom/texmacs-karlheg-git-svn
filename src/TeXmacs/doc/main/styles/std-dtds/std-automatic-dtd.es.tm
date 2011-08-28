<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generaci�n autom�tica de contenido>

  El \ <abbr|d.t.d.> <tmdtd|std-automatic> se especifica para la generaci�n
  autom�tica de contenido auxiliar como tables de contenidos y bibliograf�as,
  as� como tambi�n para la presentaci�n de tal contenido auxiliar. Las
  siguientes etiquetas son usadas para las bibliograf�as:

  <\description>
    <expand|item*|<markup|cite>>Una funci�n con un n�mero arbitrario de
    argumentos. Cada argumento es una citaci�n correspondients a un item en
    un archivo BiB-<TeX>. Las citaciones son mostradas en la misma forma como
    ellas son referenciadas en la bibliograf�a y tambi�n proveen hiperenlaces
    a las referencias correspondientes. Las citaciones son mostradas como
    macas de pregunta si usted no ha generado la bibliograf�a.

    <expand|item*|<markup|nocite*>>Similar a <markup|cite>, pero las
    citaciones no son mostradas en el texto principal.

    <expand|item*|<markup|bibitem*>>Una funci�n que especifica como mostrar
    un item en la bibliograf�a.
  </description>

  Las siguientes etiquetas son usadas para compilar tablas de contenidos:

  <\description>
    <expand|item*|<markup|toc-main-1>>Una funci�n con un argumento para crear
    entradas primordiales en la tabla de contenidos. Esta funci�n puede por
    ejemplo ser usada cuando un libro consiste de varias partes.

    <expand|item*|<markup|toc-main-2>>Una funci�n con un argumento para crear
    una entrada principal en una tabla de contenidos. Esta funci�n es usada
    regularmente para cap�tulos.

    <expand|item*|<markup|toc-normal-1>>Una funci�n con un argumento para
    crear una entrada normal en la tabla de contenidos. Esta funci�n es
    frecuentemente usada para secciones.

    <expand|item*|<markup|toc-normal-2>>Similar a <markup|toc-normal-2> para
    entradas menos importantes como en subsecciones.

    <expand|item*|<markup|toc-normal-3>>Similar a <markup|toc-normal-3> para
    entradas incluso menos importantes como subsubsecciones.

    <expand|item*|<markup|toc-small-1>>Usada para entradas no muy importantes
    tales como p�rrafos (puede ser ignorada).

    <expand|item*|<markup|toc-small-2>>Usada para entradas incluso menos
    importantes tales como subp�rrafos.

    <expand|item*|<markup|toc-dots>>La separaci�n entre una entrada en la
    tabla de contenidos y el correspondiente n�mero de p�gina. Por defecto,
    usamos puntos horizontales.
  </description>

  Las siguientes etiquetas son usadas para �ndices:

  <\description>
    <expand|item*|<markup|index>>Una funci�n con un argumento <var|x>, que
    inserta <var|x> en el �ndice como una entrada principal.

    <expand|item*|<markup|subindex>>Una funci�n con dos argumentos <var|x> y
    <var|y>, que inserta <var|y> en el �ndice como una subentrada de <var|x>.

    <expand|item*|<markup|subsubindex>>Una funci�n con tres argumentos
    <var|x>, <var|y> y <var|z>, que inserta <var|z> en el �ndice como una
    subentrada de <var|y>, que es a su vez una subentrada de <var|x>.

    <expand|item*|<markup|index-complex>>Una funci�n con cuatro argumentos
    <var|key>, <var|how>, <var|range>, <var|entry>, que est� documentada en
    la secci�n acerca de <apply|hyper-link|generaci�n del
    �ndice|../../links/man-index.es.tm>.

    <expand|item*|<markup|index-line>>Esta funci�n toma un argumento
    <var|key>, que dice como ordenar una entrada y el argumento actual
    <var|entry>. Ning�n n�mero de p�gina es generado.

    <expand|item*|<markup|index-1>>Macro con una entrada �ndice y un n�mero
    de p�gina, que es usada para visualizar una entrada principal del index
    en el �ndice.

    <expand|item*|<markup|index-1*>>Similar a <markup|index-1>, pero sin el
    n�mero de p�gina.

    <expand|item*|<markup|index-<with|mode|math|n>>>(con <with|mode|math|n>
    entre <with|mode|math|1> y <with|mode|math|5>): macro con una entrada en
    el �ndice y un n�mero de p�gina, que es usado para visualizar una entrada
    en el �ndice de nivel <with|mode|math|n>.

    <expand|item*|<markup|index-<with|mode|math|n>*>>Similar a
    <markup|index-<with|mode|math|n>>, pero sin el n�mero de p�gina.

    <expand|item*|<markup|index-dots>>El macro que produce los puntos entre
    una entrada al �ndice y el (los) correspondiente(s) n�mero(s) de p�gina.
  </description>

  Las siguientes etiquetas son usadas en los glosarios:

  <\description>
    <expand|item*|<markup|glossary>>Una funci�n que inserta su �nico
    argumento en el glosario.

    <expand|item*|<markup|glossary-dup>>Para crear un n�mero de p�gina
    adicional para una entrada que hab�a sido insertada antes.

    <expand|item*|<markup|glossary-explain>>Una funci�n para insertar una
    entrada en el glorario con su explicaci�n.

    <expand|item*|<markup|glossary-line>>Inserta una entrada de glosario sin
    n�mero de p�gina.

    <expand|item*|<markup|glossary-1>>Macro for mostrar una entrada de
    glosario \ y su n�mero de p�gina correspondiente.

    <expand|item*|<markup|glossary-2>>Macro para presentar una entrada de
    glosario, su explicaci�n y su n�mero de p�gina.

    <expand|item*|<markup|glossary-dots>>El macro que produce los putnos
    entre una entrada del glosario y el (los) correspondiente(s) n�mero(s) de
    p�gina.
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <expand|tmdoc-license|El permiso est� garantizado para copiar, distribuir
  y/o modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

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
    <associate|language|spanish>
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
      green>|toc-dots>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subindex>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubindex>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-complex>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-line>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1*>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-1>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>*>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-<with|mode|<quote|math>|n>>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|index-dots>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dup>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-explain>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-line>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-1>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-2>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|glossary-dots>>|<pageref|idx-34>>
    </associate>
  </collection>
</auxiliary>
