<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Planes para el futuro>

  <with|color|red|[Deber�a ser actualizado]>

  <section|Composici�n tipogr�fica>

  Algunas facilidades importantes de composici�n tipogr�fica que a�n no han
  sido implementadas son las siguientes

  <\itemize>
    <item>Separaci�n de un documento en varias partes.

    <item>Objetos din�micos como en HTML.

    <item>Ambientes para las interfaces con sistemas de algebra
    computacional.\ 
  </itemize>

  Muchas facilidades menores deben tambi�n ser completadas. Listamos unas
  pocas de ellas:

  <\itemize>
    <item>Considerar fracciones como operadores
    <with|mode|math|\<Rightarrow\>> espacios antes y despu�s. Similarmente
    para scripts <with|mode|math|\<Rightarrow\>> peque�o espacio antes de
    scripts izquieros and despu�s de scripts derechos.
  </itemize>

  <section|Facilidades extra para edici�n de textos>

  Aunque el movimiento del curso, selecciones, etc. han sido ya
  implementadas, algunas otras facilidades de edici�n est�ndar no han sido
  a�n completadas. Perm�tanos mencionar unas pocas de ellas:

  <\itemize>
    <item>Buscar textos, f�rmulas ciertos entornos, etc.

    <item>Reemplzao de consulta.

    <item>Facilidades matem�ticas: simplificaci�n de una regi�n seleccionada,
    sustituciones de f�mrulas en otras f�rmulas, etc.

    <item>Control de version\ 

    <item>Compresi�n y protecci�n de datos.\ 

    <item>Revisores gramaticales y programas de traducci�n autom�ticas. Sabe
    alguien donde encontrar diccionarios libres detallados y cosas como esas?

    <item>Incorporaci�n de un pgorama de reconocimiento de habla libre.
  </itemize>

  <section|Una hoja de c�lculo universal>

  Nos gustar�a incorporar una facilidad de "hoja de c�lculo universal" en
  <TeXmacs>. La idea es que todas las dependencias entre las celdas en una
  hoja son analizadas por <TeXmacs>, pero todas las computaciones actuales
  son delegadas a un sistema externo de su elecci�n, como alguno de los
  sistemas de �lgebra computacional soportados actualmente. Tambi�n, los
  datos en la hoja de c�lculo no necesariamente est�n formateados en una
  tabla rectangular; uno puede tambi�n imaginar dependencias entre los nodos
  de un �rbol, elementos de un grafo, o cualquier otra cosa.

  <section|Diagramas t�cnicos>

  Me gustar�a tambi�n incluir una facilidad para dibujar diagramas t�cnicos.
  En esta implementaci�n usted deber�a ser capaz de beneficiarse del hecho de
  que puede definir macros para hacer construcciones geom�tricas. Ser�a por
  ejemplo posible escribir un estilo para dibujar circuitos electr�nicos o
  componentes qu�micos con una agradable barra de herramientas para
  seleccionar circuidos o componetes, justo como usted selecciona l�neas y
  c�rculos en los dibujos usuales.

  <section|Interface con sistemas de �lgebra computacional>

  Las siguientes mejoras deber�an a�n ser hechas a fin de enlazar
  <apply|TeXmacs> a sistemas de �lgebra computacional:

  <\enumerate>
    <item>Mejorar el estrato de las sesiones de �lgebra computacional.

    <item>Adicionar caracter�sticas extra para incrementar la
    interoperabilidad entre <apply|TeXmacs> y sistemas de �lgebra
    computacional para dar un control adicional sobre el estrato de una
    salida grande.)

    <item>M�s sem�ntica para los objetos siendo comunicados. Esto bien puede
    ser informaci�n de alto nivel (como Openmath o el etiquetamiento
    matem�tico) o informaci�n de bajo nivel (incluyendo informaci�n sobre la
    representaci�n de los datos), dependiendo de la velocidad requerida.

    <item>Posibilidades futuras para la evoluci�n concerniente a relsaltado,
    facilidades de depuraci�n y cosas por el estilo.
  </enumerate>

  <section|Interacci�n con otros proyectos estilo-GNU>

  Podr�a ser agradable incrementar la interacci�n entre <apply|TeXmacs> y
  otros proyectos estilo-GNU, tales como Gnome o GUI's multiplataforma. Esto
  podr�a facilitar la incorporaci�n de datos externos dentro de documentos
  <apply|TeXmacs> o incrementar el n�mero de plataformas soportadas. Por otro
  lado, varias caracter�sticas <apply|TeXmacs>, tales como manejo de fuentes,
  podr�an ser interesantes para otros proyectos tambi�n.

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
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Composici�n
      tipogr�fica><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Facilidades extra para edici�n de
      textos><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Una hoja de c�lculo
      universal><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Diagramas
      t�cnicos><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Interface con sistemas de �lgebra
      computacional><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Interacci�n con otros proyectos
      estilo-GNU><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
