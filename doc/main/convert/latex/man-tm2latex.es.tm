<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion de <TeXmacs> a <LaTeX>>

  La situaci�n m�s com�n es que quiera convertir un art�culo desde
  <apply|TeXmacs> a <apply|LaTeX>, con objeto de enviarlo a alguna
  publicaci�n. Dado un fichero <apply|TeXmacs> que llamaremos
  <verbatim|nombre.tm>, puede convertirlo a un fichero <apply|LaTeX>
  <verbatim|nombre.tex> mediante <apply|menu|Fichero|Exportar|LaTeX>. Como
  primera opci�n puede ejecutar <apply|LaTeX> sobre <verbatim|nombre.tex>, y
  comprobar si obtiene resultados satisfactorios. En caso afirmativo, debe
  enviar <verbatim|nombre.tex> junto con el fichero de estilo
  <verbatim|TeXmacs.sty>, que podr� encontrar en el directorio
  <verbatim|$TEXMACS_PATH/misc/latex>.

  Es usual que la publicaci�n a la que usted env�e sus trabajos use su propio
  fichero de estilo (llam�moslo <verbatim|journal.sty>). En ese caso , debe
  tambi�n copiar el fichero

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  como\ 

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  y usar <verbatim|journal> como su estilo de documento mediante
  <apply|menu|Documento|Estilo|Otro>. Opcionalmente puede editar
  <verbatim|journal.ts>, para que la presentaci�n del art�culo sea lo m�s
  conforme al estilo de la revista. En algunos casos, tendr� que crear una
  nueva copia de <verbatim|TeXmacs.sty>, y modificar alguno de los entornos
  para conseguir compatibilidad con el fichero <verbatim|journal.sty> de la
  revista.

  Si su primer intento de convertir tu documento a <apply|LaTeX> no produce
  un resultado satisfactorio, usualmente ser� debido a que la conversi�n
  incorrecta de peque�as partes del texto. Esto puede ser debido a tres
  razones fundamentalmente:

  <\itemize>
    <item>Su texto usa algunas caracter�sticas exclusivas de <apply|TeXmacs>.

    <item>Est� usando una caracter�stica de <apply|TeXmacs> que no ha sido
    implementada a�n en el algoritmo de conversi�n.

    <item>Acaba de encontrar un fallo en el algoritmo de conversi�n.
  </itemize>

  Estas posibilidades ser�n discutidas en la siguiente secci�n.

  En caso de problemas, una estrategia simple ser�a corregir el fichero
  <apply|LaTeX> producido y enviarlo a la revista. Sin embargo, esta
  estrategia tiene la desventaja de que debe hacer las correcciones una y
  otra vez con cada conversi�n de tu fichero <apply|TeXmacs>
  <verbatim|nombre.tm> tras hacer modificaciones extra. Una estrategia mejor
  es usar <apply|menu|Insertar|Espec�fico|Latex> e
  <apply|menu|Insertar|Espec�fico|Texmacs> para escribir texto que solo ser�
  visible en el fichero convertido o en el original respectivamente.

  Por ejemplo: asumamos que la palabra ``blauwbilgorgel'' es partida al final
  de la l�nea correctamente en <apply|TeXmacs>, pero no en la conversi�n a
  <apply|LaTeX>. Deber� hacer lo siguiente:

  <\enumerate>
    <item>Seleccionar ``blauwbilgorgel''.

    <item>Click en <apply|menu|Insertar|Espec�fico|Texmacs> para hacer el
    texto ``blauwbilgorgel'' espec�fico de <apply|TeXmacs>.

    <item>Click en <apply|menu|Insertar|Spec�fico|Latex>.

    <item>Teclear el c�digo latex <verbatim|blauw\\-bil\\-gor\\-gel> con las
    divisiones correctas.

    <item>Pulse <key|enter> para activar el texto espec�fico <apply|LaTeX>.
  </enumerate>

  De la misma forma puede insertar saltos de l�nea o de p�gina, espacios
  verticales, modificaciones de los par�metros del estilo, etc.

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
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Exportar>|<with|font
      family|<quote|ss>|LaTeX>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>|<with|font
      family|<quote|ss>|Otro>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Espec�fico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Espec�fico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Espec�fico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Spec�fico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
