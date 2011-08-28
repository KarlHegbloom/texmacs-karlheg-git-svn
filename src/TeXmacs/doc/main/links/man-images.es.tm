<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Insertar im�genes>

  Puede incluir im�genes en el texto usando el men�
  <apply|menu|Insert|Image>. Actualmente <TeXmacs> reconoce los formatos
  <verbatim|ps>, <verbatim|eps>, <verbatim|tif>, <verbatim|pdf>,
  <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>, <verbatim|xpm> y
  <verbatim|fig>. Para ver las im�genes Postscript, <TeXmacs>
  utiliza<verbatim| gs> (ghostscript). Si no est� instalado en su sistema,
  puede descargarlo desde\ 

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  De hecho, los otros formatos de fichero se traducen a Postscrip a trav�s de
  los scripts <verbatim|tiff2ps>, <verbatim|pdf2ps>, <verbatim|pnmtops>,
  <verbatim|giftopnm>, <verbatim|ppmtogif>, <verbatim|xpmtoppm>. Si no est�n
  disponibles en su sistema, pregunte al administrador del sistema.

  Por defecto las im�genes se muestran al tama�o que fueron dise�adas. Las
  im�genes admiten las siguientes operaciones:

  <\itemize>
    <item>Recortar seg�n un rect�ngulo. Se toma la esquina inferior izquierda
    de la imagen por defecto como origen para especificar un rect�ngulo que
    sirva de m�scara para el recorte.

    <item>Redimensionar. Cuando se especifica una sola de las dimensiones
    (ancho y no alto o vice versa) la imagen cambia de tama�o manteniendo la
    proporci�n inicial.

    <item>Ampliar. Una forma alternativa de redimensionar, multiplicando
    altura y anchura por el mismo factor constante.
  </itemize>

  <TeXmacs> tambi�n incluye un <em|script> para convertir figuras,
  opcionalmente con f�rmulas <LaTeX> inclu�das en Postscript encapsulado.
  Para incluir una f�rmula <LaTeX> en una figura de <verbatim|xfig> hay que
  introducir la f�rmula como texto, seleccionando una fuente <LaTeX>, y fijar
  el indicador especial en los indicadores de texto.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Imagen>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
