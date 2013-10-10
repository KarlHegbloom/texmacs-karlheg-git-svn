<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Cabeceras est�ndard>

  El d.t.d. <tmdtd|header> provee etiquetas para personalizar las cabeceras y
  pies de p�gina. La personalizaci�n es basada en la idea de que podemos
  especificar un <em|texto de p�gina> para cada p�gina. Este texto de p�gina
  puede por ejemplo ser el t�tulo corriente o el nombre de la secci�n actual.
  El texto de la p�gina puede depender de la paridad de una p�gina y aparecer
  en una forma diferentes para p�ginas especiales como comienzos de nuevos
  cap�tulos. Las siguientes etiquetas controlan la disposici�n f�sica de los
  diferentes tipos de p�ginas:

  <\explain|<markup|start-page>>
    Esta etiqueta, con el texto de la p�gina como su �nico argumento,
    especifica la disposici�n de una primera p�gina de un cap�tulo o secci�n.
  </explain>

  <\explain|<markup|odd-page-text>>
    Similar a <markup|start-page>, pero para la disposici�n de p�ginas
    impares ordinarias.
  </explain>

  <\explain|<markup|even-page-text>>
    Similar a <markup|start-page>, pero para la disposici�n de p�ginas pares
    ordinarias.
  </explain>

  Las siguientes etiquetas controlan las acciones l�gicas relacionadas con la
  cabecera a ser tomadas, cuando se especifica un t�tulo, un autor, o cuando
  se empieza una nueva secci�n.

  <\explain|<markup|header-title>>
    Una etiqueta con el ``argumento t�tulo'' que es usado en la
    especificaci�n del t�tulo del documento.
  </explain>

  <\explain|<markup|header-author>>
    Una etiqueta con el ``argumento autor'' que es usado en la espeficicaci�n
    del autor del documento.
  </explain>

  <\explain|<markup|header-primary>>
    Una etiqueta con el ``argumento nombre de secci�n'' que es usado en el
    comienzo de cada nueva secci�n primaria. (<abbr|i.e.> <markup|chapter>
    para el estilo libro, o <markup|section> para el estilo art�culo).
  </explain>

  <\explain|<markup|header-secondary>>
    \ Una etiqueta con el ``argumento nombre de secci�n'' que es usado en el
    comienzo de cada nueva secci�n secundaria. (<abbr|i.e.> <markup|section>
    para el estilo libro, o <markup|subsection> para el estilo art�culo).
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
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