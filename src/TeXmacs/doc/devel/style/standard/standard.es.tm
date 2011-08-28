<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Los estilos y paquetes <TeXmacs> est�ndar>

  Actualmente los siguientes estilos est�ndar han sido implementados:

  <\itemize>
    <item>Libro,

    <item>Art�culo;

    <item>Carta;

    <item>Seminario (para transparencias).
  </itemize>

  Cada uno de estos estilos exporta cierto n�mero de funciones y estilos
  est�ndar (listados abajo). Se espera que todos los futuros estilos est�ndar
  de documento den soporte al menos a los comandos y entornos que siguen, y
  sugerimos a los usuarios escribir archivos de estilo que lo hagan:

  <\itemize>
    <item>Comandos de divisi�n en secciones.

    <item>Listas de bolos y numeradas.

    <item>Entornos tipo ecuaci�n.

    <item>Entornos tipo teorema.

    <item>Entornos de programaci�n.
  </itemize>

  Note que los entornos tipo teorema no son est�ndar en <LaTeX>, lo cual
  constituye una fuente conocida de incompatibilidades. Los nuevos
  ``teoremas'' pueden ser adicionados con el comando <verbatim|newtheorem>.
  Es tambi�n posible a�adir nuevas ``observaciones'', con el comando
  <verbatim|newremark>. Las ``observaciones'' difieren de los ``teoremas'' en
  que en general el cuerpo de las primeras no se compone con �nfasis
  (usualmente cursiva).

  Por supuesto, los entornos de programaci�n no est�n soportados por <LaTeX>.
  Tales entornos est�n actualmente en desarrollo.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
