<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <htab|5mm><expand|tmdoc-title|Introducci�n al lenguaje de extensi�n
  <name|Guile>>

  Como <name|Emacs>, <apply|TeXmacs> viene con un lenguaje de extensi�n tipo
  <name|Lisp>, llamado el dialecto <with|font shape|small-caps|Guile Scheme>
  del proyecto <name|Gnome>. Para documentaci�n acerca de <with|font
  shape|small-caps|Guile Scheme>, nos referimos a

  <\verbatim>
    \ \ \ http://www.gnu.org/software/guile/guile.html
  </verbatim>

  <apply|scheme> tiene la ventaja de que puede ser extendido con tipos y
  rutinas externas C y C++. En nuestro caso, hemos extendido <name|Scheme>
  con rutinas que puede usar para crear sus propios men�s y combinaciones de
  teclas, e incluso escribir sus propias extensiones a <TeXmacs>.

  Si usted ha descargado los archivos fuentes de <TeXmacs>, entonces puede
  ser interesante para usted echar un vistazo a los archivos

  <\verbatim>
    \ \ \ Guile/Glue/build-glue-basic.scm<format|next line>
    \ \ Guile/Glue/build-glue-editor.scm<format|next line>
    \ \ Guile/Glue/build-glue-server.scm
  </verbatim>

  Estos tres archivos ``pegamento'' contiene las rutinas C++, que son
  visibles dentro de <apply|scheme>. EN lo que sigue, discutiremos algunas de
  las rutinas m�s importantes. Planeamos escribir una gu�a de referencia m�s
  completa despu�s. Usted puede tambi�n echar un vistazo a los archivos
  <name|Scheme> <verbatim|.scm> en el directorio
  <verbatim|$TEXMACS_PATH/progs>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
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
