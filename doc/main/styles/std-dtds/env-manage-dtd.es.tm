<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Definir nuevos entornos>

  El <abbr|d.t.d> <tmdtd|env-manage> contiene etiquetado de alto nivle el
  cual puede ser usado por el usuario para defninir nuevos entornos para
  teoremas, ejercicios y figuras:

  <\description>
    <expand|item*|<markup|newtheorem>>Define un entorno tipo teorema. Usted
    debe especificar un nombre para el entorno (como ``experimento'') y el
    texto correspondiente (como ``Experimento'').

    <expand|item*|<markup|newremark>>Similar a <markup|newtheorem>, pero para
    avisos.

    <expand|item*|<markup|newexercise>>Similar a <markup|newtheorem>, pero
    para ejercicios.

    <expand|item*|<markup|newfigure>>Similar a <markup|newtheorem>, pero para
    figuras (en pares grandes y peque�os).
  </description>

  El <abbr|d.t.d.> tambi�n contienen etiquetado de bajo nivel para las
  definiciones actuales de los entornos. De hecho, la definici�n de nuevos
  teoremas es hecha en dos estados. En un primer estado, la etiqueta
  <markup|newtheorem> es usada a fin de especificar que un entorno tipo
  teorema deber�a ser definido. En un segundo estado (justo antes de que el
  documento del usuario es procesado) los entornos tipo teorema son
  definidos. Este mecanismo hace posible personalizar los entornos en
  paquetes que son procesados entre los dos estados. Por ejemplo, la
  numeraci�n de teoremas es personalizada de esta forma.

  <\warning>
    En el momento, usted deber�a s�lo usar la etiqueta <markup|newtheorem> y
    similares dentro de archivos o paquetes de estilo personales. Si usa
    <markup|newtheorem> directamente dentro de un documento, entoces la
    numeraci�n puede ser incorrecta, debudo a el esquema de dos estados
    explicados arriba. Esta inconveniencia desaparecer� tan prnto como sea
    posible especificar pre�mbulos l�mpios para documentos <TeXmacs>.
  </warning>

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
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-manage>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newremark>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newexercise>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newfigure>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
