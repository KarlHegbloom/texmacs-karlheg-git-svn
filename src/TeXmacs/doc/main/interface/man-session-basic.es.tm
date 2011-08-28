<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Uso b�sico>

  Una sesi�n puede ser iniciada desde el men� <apply|menu|Insert|Session>.
  Una sesi�n consiste de una secuencia de entornos de entrada y salida y de
  un posible texto entre ellos. Cuanto teclea <key|enter> dentro de un
  entorno de entrada, el texto dentro del entorno es evaluado y el resultado
  es mostrado como un entorno de salida.

  Cuando se ingresa un comando en una sesi�n, la aplicaci�n intenta
  ejecutarlo. Varios comandos pueden ser lanzados concurrentemente en el
  mismo documento, pero la salida s�lo ser� activada en la sesi�n donde el
  cursor est� y en la posici�n del cursor. Por tanto, recomendamos usar
  diferentes <em|buffers> para ejecuciones paralelas. Las ejecuciones pueden
  ser interrumpidas desde la barra de iconos. Tambi�n es posible desconectar
  (cerrar) la aplicaci�n; en este caso ning�n comando posterior puede ser
  ejecutado en la sesi�n correspondiente.

  En la segunda barra de iconos usted puede tener tambi�n unos peque�os
  botones para seleccionar entradas matem�ticas e interrumpir la ejecuci�n.
  Cuando est� implementada para un sistema dado, la entrada matem�tica le
  permite teclear la entrada en una forma gr�fica bidimensional. Los otros
  dos botones le permite interrumpir la ejecuci�n de un comando particular
  (aunque estno no trabaja bien para ciertos sistemas) o desconectar el
  sistema externo. Cuando presiona enter en la entrada de un sistema no
  conectado, el sistema ser� reiniciado autom�ticamente.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Sesi�n>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
