<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Especificando el tama�o de la celda y la tabla>

  Usando <apply|menu|Table|Cell width|Set width> y <apply|menu|Table|Cell
  height|Set height>, respectivamente, puede especificar el ancho o el alto
  de una celda. De hecho hay tres modos de fijar el alto (o el ancho)
  especificados:

  <\description>
    <expand|item*|Modo m�nimo.>El ancho actual de la celda ser� el m�nimo
    entre el ancho espec�ficado y el ancho de la caja interior de la celda.

    <expand|item*|Modo exacto.>El ancho de la celda ser� exactamente el
    especificado.

    <expand|item*|Modo M�ximo.>El ancho actual de la celda ser� el m�ximo
    entre el ancho especificado y el ancho de la caja interior de la celda.
  </description>

  El ancho del borde y del acolchado de la celda (que explicaremos m�s abajo)
  son tomados en cuenta para el tama�o de la caja interior de la celda.

  Tambi�n puede especificar la anchura y la altura de la tabla entera en
  <apply|menu|Table|Special table properties>. En particular puede
  especificar que la tabla ocupe el ancho completo de un p�rrafo. Cuando se
  especifica una anchura (o altura) para la tabla completa, necesita indicar
  como el espacio no usado ser� distribuido sobre las celdas usando
  <apply|menu|Table|Special cell properties|Distribute unused space>. Por
  defecto este espacio no usado se distribuye equitativamente.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|�lvaro Cantero
  Tejero|Pablo Ruiz M�zquiz|David Moriano Garcia|Offray Vladimir Luna
  C�rdenas>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Anchura de la celda>|<with|font
      family|<quote|ss>|Fijar anchura>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Altura de la celda>|<with|font
      family|<quote|ss>|Fijar altura>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Propiedades especiales de tabla>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Propiedades especiales de celda>|<with|font
      family|<quote|ss>|Distribuir el espacio no utilizado>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
