<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Crear sus propios men�s din�micos>

  En particular, el archivo de inicializaci�n por defecto ejecuta

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu/main_menu.scm
  </verbatim>

  a fin de configurar el men� principal de <TeXmacs>. Le sugerimos echar un
  vistazo a este archivo a fin de ver c�mo son creados los men�s.

  De hecho, cualquier men� o parte de un men� es representada por un
  programa. El programa consiste de una lista de programas de una de las
  siguientes formas:

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<format|next line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<format|next line>
    \ \ \ ("entrada" accion)<format|next line> \ \ \ ("entrada" "shorthand"
    accion)<format|next line> \ \ \ ---<format|next line> \ \ \ (if condition
    menu-definition)<format|next line> \ \ \ (link variable)
  </verbatim>

  Los constructores <verbatim|=\<gtr\>> y <verbatim|-\<gtr\>> son usados para
  crear un men� desplegable hacia abajo o un men� desplegable hacia la
  derecha y la condici�n <verbatim|menu-definition> debe contener un programa
  que crea un submen�. El constructor <verbatim|("entrada" accion)> crea una
  entrada ordinaria, donde la <verbatim|accion> ser� compilado y ejecutado
  cuando haga click en la <verbatim|entrada>. El c�digo opcional
  <verbatim|"shorthand"> se establece para un macro de teclado con la misma
  acci�n. Los items de un men� pueden separarse usando <verbatim|--->. El
  constructor <verbatim|if> es usado para insertar items de men� s�lo si una
  cierta condici�n es satisfecha (por ejemplo, si estamos en el modo
  matem�tico).

  Finalmente, si hemos declarado una variable <TeXmacs> ser un men� mediante

  <\verbatim>
    \ \ \ \ (define variable menu-definition)
  </verbatim>

  entonces podemos usar este men� indirectamente usando el constructor
  <verbatim|link>. esta forma indirecta de declarar submen�s tiene dos
  ventajas

  <\itemize>
    <item>Un men� ``indirecto'' puede ser vinculado a tantos men�s como nos
    guste.

    <item>Nuevos items pueden ser adicionados a los submen�s ``indirectos''
    <with|font shape|italic|a posteriori> usando\ 

    <\verbatim>
      \ \ \ \ (set! variable (menu-merge variable menu-declaration))
    </verbatim>

    De hecho, los t�rminos existentes pueden ser tambi�n sobrescritos en esta
    forma.
  </itemize>

  Algunos men�s indirectos est�ndar en <TeXmacs> son <verbatim|texmacs-menu>,
  <verbatim|file-menu>, <verbatim|edit-menu>, <verbatim|insert-menu>,
  <verbatim|text-menu>, <verbatim|paragraph-menu>, <verbatim|document-menu>,
  <verbatim|options-menu> y <verbatim|help-menu>. El comando

  <\verbatim>
    \ \ \ \ (menu-main menu-declaration)
  </verbatim>

  es usado a fin de configurar de hecho el men� principal. Por ejemplo, en la
  inicializaci�n, ejecutamos

  <\verbatim>
    \ \ \ \ (menu-main '(link texmacs-menu))
  </verbatim>

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
