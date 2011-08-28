<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Libros y documentos de m�ltiples archivos>

  Cuando un documento se vuelve realmente grande, usted puede querer
  subdividirlo en piezas m�s peque�as. As� las piezas son reutilizables en
  otros trabajos y por otra parte, se mejora la velocidad de edici�n. Se
  puede insertar un fichero entero dentro de otro utilizando
  <apply|menu|Insertar|Enlace|Incluir fichero>. Para acelerar el tratamiento
  de todos los documentos incluidos por este procedimiento, <TeXmacs> recurre
  a una t�cnica conocida como <em|buffering>. Eso hace necesario actualizar
  los documentos inclu�dos a trav�s de <apply|menu|Tools|Update|Inclusions>.

  Por ejemplo, si est� escribiendo un libro, es normal que ponga los
  cap�tulos en archivos individuales, separados, llamados quiz�
  <verbatim|c1.tm>, <verbatim|c2.tm...>Un archivo, <verbatim|libro.tm> hace
  de <em|documento maestro>, simplemente insertando en �l los enlaces a las
  partes como se describe en el p�rrafo anterior. Es normal ubicar el �ndice,
  la bibliograf�a, etc, en <verbatim|libro.tm>.

  De todos modos, este modo de identificar el documento maestro s�lo funciona
  en una direcci�n: se le dice al documento maestro cu�les son sus partes,
  pero no a las partes cu�l es su documento maestro. Y esto �ltimo se hace
  imprescindible si se quieren ver las referencias cruzadas entre partes
  (cap�tulos en nuestro ejemplo) mientras se edita una de ellas en
  particular. La soluci�n es sencilla: especificar <verbatim|libro.tm> como
  <em|documento maestro> para <verbatim|ci.tm>, mediante la opci�n de men�
  <apply|menu|Document|Master|Attach>. Como por el momento esta operaci�n no
  resuelve la numeraci�n de cap�tulos, recuerde que puede asignar al
  principio de cada cap�tulo la variable de entonrno <verbatim|chapternr> de
  modo que al estar editando <verbatim|ci.tm> aparezca ``Cap�tulo i'' al
  principio del documento y no ``Cap�tulo 1''.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Incluir
      fichero>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Herramientas>|<with|font
      family|<quote|ss>|Actualizar>|<with|font
      family|<quote|ss>|Inclusiones>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Maestro>|<with|font
      family|<quote|ss>|Vincular>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
