<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Mejorar la implementaci�n actual >

  <with|color|red|[Deber�a ser actualizado]>

  <section|Administraci�n de memoria>

  Si tengo el corage, podr�a un dia escribir un recolector de basura para
  <apply|TeXmacs>.

  <section|Interface gr�fica>

  Deseamos movernos hacia <with|font shape|small-caps|Guile-Gtk> u otra
  interface gr�fica de usuario portable.\ 

  <section|Codificaciones y fuentes>

  <\itemize>
    <item>Algunos cambios tienen todav�a que ser hechos en la forma como las
    codificaciones de fuentes son manejadas. Esto deber�a hacer m�s f�cil
    mantener fuentes con caracteres de varias fuentes f�sicas, fuentes
    virtuales, caracteres especiales, etc.

    <item>Actualmente, la fuente l�gica actual es determinada desde un
    conjunto fijo de variables de enterno �nicamente. La composici�n
    tipogr�fica actual deber�a ser m�s bien un �rbol (en lugar de una
    cadena), el cual es evaluado (as� que las variables de entorno son
    reemplazadas) y entonces pasadas a <verbatim|find_font(display,tree)>. La
    fuente actual puede entonces ser una fuente junta y las futuras fuentes
    puede depender de variables de entorno del usuario (e.j. fuentes
    coloreadas, usar m�s de un color).
  </itemize>

  <section|Velocidad>

  A fin de hacer m�s r�pido el programa, hemos ya hecho un cambio importante
  y es que no todo el documento ser� compuesto tipogr�ficamente cuando se
  hagan cambios locales. Sin embargo varias otras optimizaciones deber�an a�n
  ser hechas:

  <\itemize>
    <item>Mejorar la velocidad para cargar (y salvar) archivos. Esto
    acelerar� la carga de fuentes.

    <item>Codificar las variables de entorno del sistema; esto acelerar�
    globalmente el programa.

    <item>La forma en que las variables de entorno de estilo son manejadas
    por los concatenantes del sistema de composici�n tipogr�fica y los
    p�rrafos est� lejos de lo �ptimo. Alg�n repensamiento serio deber�a ser
    emprendido aqu�.

    <item>La computaci�n del contexto actual en una posici�n dada del cursor
    toma mucho tiempo, especialmente la computaci�n del entorno. Esto hace
    lento el movimiento del cursor para textos complejos. usando fuentes
    <TeX> (se desplaza mucho m�s r�pido cuando se usan fuentes X).
  </itemize>

  <section|Cambios miscel�neos>

  Las siguientes implementaciones <em|ad hoc> deber�an ser cambiadas o hechas
  m�s robustas:

  <\itemize>
    <item>El movimiento del cursos dentro de l�neas de un p�rrafo (cuando se
    mueve a la derecha al final de la l�nea, el cursor deber�a saltar al
    comienzo de la pr�xima l�nea).

    <item>Anchos de la barra de fracci�n, la l�nea superior de la ra�z
    cuadrada y las negaciones.

    <item>Las cajas no deber�an tener or�genes, sus hijos deber�an tener
    posiciones en su lugar.

    <item>Separaci�n clara entre archivos de los cuales depende el sistem
    (e.j. <verbatim|fast_alloc.cpp>, <verbatim|file.hpp>, <verbatim|dir.hpp>) en
    alg�n directorio y los otros.
  </itemize>

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Administraci�n de
      memoria><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Interface
      gr�fica><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Codificaciones y
      fuentes><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Velocidad><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Cambios
      miscel�neos><value|toc-dots><pageref|toc-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
