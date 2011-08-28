<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversi�n de <LaTeX> a <TeXmacs>>

  La intenci�n del programa de conversi�n de <apply|LaTeX> a <apply|TeXmacs>,
  es <with|font shape|italic|ayudarle> en la conversi�n de sus viejos
  documentos a <apply|TeXmacs>. <with|font shape|italic|Grosso modo>, la
  conversi�n de <apply|LaTeX> a <apply|TeXmacs> es m�s problem�tica que a la
  inversa. Sin embargo, mientras se limite a usar los comandos <apply|LaTeX>
  m�s comunes, deber�a poder hacer la conversi�n de sus viejos documentos
  razonablemente bien. Por ejemplo, todos los archivos de ayuda de
  <apply|TeXmacs> fueron redactados originalmente en <apply|LaTeX> con el
  objeto de validar en programa de conversi�n de <apply|LaTeX> a
  <apply|TeXmacs>.

  Puede convertir un documento <apply|LaTeX> <verbatim|nombre.tex> a
  <apply|TeXmacs> usando <apply|menu|File|Import|Latex> y guardarlo como
  <verbatim|nombre.tm>. Si su documento <apply|LaTeX> fue escrito
  suficientemente bien, entonces, el resultado de la conversi�n deber�a ser
  m�s o menos aceptable, con excepci�n de ciertos comando no reconocidos que
  aparecer�an en rojo. Una buena soluci�n ser�a escribir su propio archivo de
  estilo para documentos convertidos, basado en el estilo original, y en el
  que los comandos no reconocidos estuviesen definidos.

  Sin embargo, en ciertos casos desafortunados, aparecer� un gran caos. Esto
  es debido frecuentemente al hecho de que <apply|TeX> y <apply|LaTeX>
  permiten al usuario modificar el analizador din�micamente, por ejemplo
  usando el comando <verbatim|\\catcode>. En este caso, el programa de
  conversi�n podr�a llegar a confundirse realizando presuposiciones err�neas
  sobre el modo o el entorno. Como resultado, puede que el texto sea
  confundido con las matem�ticas, las matem�ticas con el texto sin formato
  (literal), etc. En cualquier caso, los comandos en su archivo
  <verbatim|nombre.tex> que confundan al programa de conversi�n son
  f�cilmente localizables comparando el c�digo fuente <apply|LaTeX> con su
  conversi�n a <apply|TeXmacs>. Realizando algunas modificaciones en el
  c�digo deber�a poder eliminar la parte problem�tica de forma que el
  documento se convierta razonablemente bien.

  En el futuro, planeamos extender el programa de conversi�n con un conversor
  de archivos de estilo y algunas caracter�sticas adicionales que facilitar�n
  la traducci�n de comandos definidos por el usuario, que est�n definidos en
  otro documento aparte del que quiere convertir.

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
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Importar>|<with|font
      family|<quote|ss>|LaTeX>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Importar>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
