<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creaci�n de r�tulos, enlaces y referencias>

  Puede crear un nuevo r�tulo inactivo usando <shortcut|(make-label)> o
  <apply|menu|Insert|Link|Label> y una referencia a este r�tulo usando
  <shortcut|(make 'reference)> o <apply|menu|Insert|Link|Reference>. Sea cuidadoso de
  poner el r�tulo a un punto donde su n�mero ser� correcto. Cuando rotule las
  secciones, el lugar recomendado es justo despu�s del nombre de la secci�n.
  Cuando rotule ecuaciones, el lugar recomendado es al inicio dentro de la
  ecuaci�n.\ 

  Es posible crear hiperenlaces a otros documentos usando
  <key|inactive \<gtr\>> o <apply|menu|Insert|Link|Hyperlink>. El segundo
  campo de un hiperenlace es el texto asociado, que es mostrado en azul
  cuando es activado. El segundo campo contiene el nombre de un documetno, el
  cual puede estar en la Web. Como es usual para los hiperenlaces, un enlace
  de la forma <verbatim|#<with|font shape|italic|label>> apunta a un r�tulo
  en el mismo documento y un link de la forma <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|label>> apunta al r�tulo en el
  documento localizado en la <verbatim|<with|font shape|italic|url>>.

  De modo similar, una acci�n puede ser asociada a una pieza de texto o
  gr�ficos usando <key|inactive *> o <apply|menu|Insert|Link|Action>. El
  segundo campo ahora contiene un comando de <em|script> de Guile/Scheme, en
  cual es ejecutado en el momento que haga doble click sobre el texto,
  despu�s de su activaci�n. Por razones de seguridad, tales <em|scripts> no
  son aceptados siempre. Por defecto, usted es interrogado para su
  aceptaci�n; este comportamiento por defecto puede ser cambiado en
  <apply|menu|Options|Security>. Note que el comando de Guile/Scheme

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  evalua <verbatim|shell-command> como un comando de shell.

  Finalmente, usted puede directamente incluir otros documentos dentro de un
  documento usando <key|inactive i> o <apply|menu|Insert|Link|Include>. Esto
  le permite por ejemplo incluir un listado de un programa en su texto de
  forma tal que sus modificaciones en su programa son autom�ticamente
  reflejadas en su texto.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font
      family|<quote|ss>|Etiqueta>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font
      family|<quote|ss>|Referencia>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font
      family|<quote|ss>|Hiperenlace>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font
      family|<quote|ss>|Acci�n>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Opciones>|<with|font
      family|<quote|ss>|Seguridad>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Incluir
      fichero>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
