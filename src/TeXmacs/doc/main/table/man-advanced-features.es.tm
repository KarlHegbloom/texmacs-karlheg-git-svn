<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Caracter�sticas avanzadas de la tabla>

  En los men�s puede encontrar algunas propiedades especiales para las
  tablas. Brevemente podemos comentar las siguientes:

  <\itemize>
    <item>Cambiar el ``alcance'' de una celda y hacer que ocupe el espacio de
    sus vecinas a la derecha y debajo.

    <item>Creaci�n de subtablas completas dentro de una celda.

    <item>Correcci�n de la profundidad y altura del texto para hacerlas
    coincidir las l�neas base.

    <item>Separaci�n ortogr�fica horizontal de los contenidos de la celda y
    separaci�n ortogr�fica vertical de toda la tabla.

    <item>Pegado de varias filas y/o columnas de forma que las celdas pegadas
    se conviertan en ``parte de los bordes'' de las celdas restantes.

    <item>Desactivaci�n de la tabla para poder ver su ``c�digo fuente''.

    <item>Fijado del ``centro de extensi�n'' de una tabla. A partir de ello,
    la propiedades de formato de esa celda ser�n usadas de patr�n para nuevas
    celdas creadas alrededor de este centro.

    <item>Especificaci�n del tama�o m�nimo y m�ximo de una tabla, que ser�
    respetado en toda edici�n posterior (esto es principalmente �til en la
    creaci�n de macros para tablas).
  </itemize>

  Normalmente, todas las tablas provienen de un entorno que puede ser
  <markup|tabular>, <markup|block>, <markup|matrix>, etc. Cuando cree sus
  propios macros para tablas, debe usar <apply|menu|Table|Special table
  properties|Extract format> para extraer el formato de una tabla dada.

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
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrix>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tabla>|<with|font
      family|<quote|ss>|Propiedades especiales de tabla>|<with|font
      family|<quote|ss>|Extraer formato>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
