<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Reglas generales de prefijos>

  Como hay muchos atajos de teclado, es importante tener algunas formas de
  clasificarlas en varias categor�as, a fin de hacer m�s f�cil memorizarlas.
  Como regla general, los atajos de teclado que caen en la misma categor�a
  son identificados por un prefijo com�n. Las principales de tales prefijos
  son:

  <\description>
    <expand|item*|<key|C-<with|mode|math|x>>>Los atajos de teclado basados en
    la tecla Control son usados para comandos de edici�n frecuentemente
    usados. Ellos dependen mucho del ``aspecto y comportamiento'' en
    <apply|menu|Edit|Preferences>. Por ejemplo, si usted usa un aspecto y
    comportamiento comatible con <name|Emacs>, entonces los atajos de la
    forma <key|C-<with|mode|math|x>> corresponden a los comandos
    <name|Emacs>, como <key|C-y> para pegar texto.

    <expand|item*|<key|A-<with|mode|math|x>>>La tecla alternate key es usada
    para comandos que dependen del modo en el cual usted est�. Por ejemplo,
    <expand|kbd-text|s> produce texto <strong|resaltado> en el modo texto y
    una ra�z cuadrada en <with|mode|math|<sqrt|>> en el modo matem�tico. Note
    que <key|escape escape> es equivalente a <key|A->.

    <expand|item*|<key|M-<with|mode|math|x>>>La tecla meta es usada para
    comandos <apply|TeXmacs> de prop�sito general, que pueden ser usados en
    todos los modos. Por ejemplo, <expand|kbd-gen|!> produces una etiqueta.
    Es tambi�n usado para comandos de edici�n adicional, como <key|A-w> para
    copiar texto si usted usa el aspecto y comportamiento <name|Emacs>. Note
    que <key|escape> es equivalente a <key|M->.

    <expand|item*|<key|H-<with|mode|math|x>>>El modificador de teclado de
    usuario es usado para producer s�mbolos especiales como los catacteres
    griegos en el modo matem�tico. Puede configurar su teclado para dejar que
    la tecla de may�sculas juegue el rol de la tecla hiper. El <key|F5> is
    equivalent to <key|M->.
  </description>

  Recordamos que los modificadores particulares que son usados a fin de
  obtener los prefijos <key|M-> y <key|H-> pueden ser
  <apply|hyper-link|configurados|../../config/man-config-kbd-modkeys.es.tm>
  en <apply|menu|Edit|Preferences>.

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
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
