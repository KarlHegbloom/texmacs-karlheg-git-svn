<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Recorriendo la documentaci�n <TeXmacs>>

  Como una regla general, usted debe evitar el uso de comandos de secci�n
  dentro de la documentaci�n de <TeXmacs> y tratar de escribir peque�as
  p�ginas de ayuda sobre t�picos bien identificados. En una segunda etapa,
  usted debe escribir "meta archivos de ayuda" que indique como recorrer la
  documentaci�n en una forma autom�tica. Esto le permite el uso de una p�gina
  de ayuda para diferentes prop�sitos (un manual impreso, un tutorial
  orientado a la Web, etc.).

  El estilo <tmstyle|tmdoc> provee tres macros de etiquetamiento para indicar
  c�mo recorrer la documentaci�n. El macro <markup|traverse> es usado para
  encapsular regiones con informaci�n transversal. El macro <markup|branch>
  indica una p�gina de ayuda que debe ser considerada como una subsecci�n y
  el macro <markup|continue> indica una p�gina que sigue. Tanto el macro
  <markup|branch> como el <markup|continue> toman dos argumentos. El primer
  argumento describe el enlace y el segundo argumento da la direcci�n f�sica
  relativa del archivo enlazado.

  T�picamente, al final de un meta archivo de ayuda usted encontrar� varios
  macros <markup|branch> o <markup|continue>, dentro de un macro
  <markup|traverse>. En la parte superior del documento, usted debe
  espeficicar un t�tulo para su documento usando el macro
  <markup|tmdoc-title>. Cuanod se genera un manual impreso desde la
  documentaci�n, una estructura cap�tulo-secci�n-subsecci�n ser�
  autom�ticamente generada a partir de esta informaci�n y los t�tulos de los
  documentos. Alternaticamente, uno podr�a generar botones adicionales para
  la navegaci�n dentro de la documentaci�n usando un navegador.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir
  Luna C�rdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdoc-title>>|<pageref|idx-10>>
    </associate>
  </collection>
</auxiliary>
