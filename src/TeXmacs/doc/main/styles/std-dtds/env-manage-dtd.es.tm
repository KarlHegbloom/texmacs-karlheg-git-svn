<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Definir nuevos entornos>

  El <abbr|d.t.d> <tmdtd|env-manage> contiene etiquetado de alto nivle el
  cual puede ser usado por el usuario para defninir nuevos entornos para
  teoremas, ejercicios y figuras:

  <\explain|<markup|new-theorem>>
    Define un entorno tipo teorema. Usted debe especificar un nombre para el
    entorno (como ``experimento'') y el texto correspondiente (como
    ``Experimento'').
  </explain>

  <\explain|<markup|new-remark>>
    Similar a <markup|new-theorem>, pero para avisos.
  </explain>

  <\explain|<markup|new-exercise>>
    Similar a <markup|new-theorem>, pero para ejercicios.
  </explain>

  <\explain|<markup|new-figure>>
    Similar a <markup|new-theorem>, pero para figuras (en pares grandes y
    peque�os).
  </explain>

  El <abbr|d.t.d.> tambi�n contienen etiquetado de bajo nivel para las
  definiciones actuales de los entornos. De hecho, la definici�n de nuevos
  teoremas es hecha en dos estados. En un primer estado, la etiqueta
  <markup|new-theorem> es usada a fin de especificar que un entorno tipo
  teorema deber�a ser definido. En un segundo estado (justo antes de que el
  documento del usuario es procesado) los entornos tipo teorema son
  definidos. Este mecanismo hace posible personalizar los entornos en
  paquetes que son procesados entre los dos estados. Por ejemplo, la
  numeraci�n de teoremas es personalizada de esta forma.

  <\warning>
    En el momento, usted deber�a s�lo usar la etiqueta <markup|new-theorem> y
    similares dentro de archivos o paquetes de estilo personales. Si usa
    <markup|new-theorem> directamente dentro de un documento, entoces la
    numeraci�n puede ser incorrecta, debudo a el esquema de dos estados
    explicados arriba. Esta inconveniencia desaparecer� tan prnto como sea
    posible especificar pre�mbulos l�mpios para documentos <TeXmacs>.
  </warning>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|spanish>
  </collection>
</initial>