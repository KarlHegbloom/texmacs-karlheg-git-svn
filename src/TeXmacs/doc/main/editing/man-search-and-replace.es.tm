<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Buscar y sustituir>

  Puede iniciar una b�squesda de texto tecleando <key|C-s> o
  <apply|menu|Edit|Search>. Durante una b�squeda la ``cadena de b�squeda'' se
  muestra en el lado izquierdo de la parte inferior de la ventana. La
  b�squeda es incremental, lo que significa que cada car�cter que introduce
  se a�ade a la cadena de b�squeda y su siguiente aparici�n queda rodeada por
  una caja de bordes rojos. Cada vez que teclea <key|C-s> el programa busca
  la siguiente aparici�n de la palabra. Si <TeXmacs> no encuentra m�s
  apariciones de la cadena en el documento, deber�a oir un bip. La b�squeda
  es tambi�n c�clica, en el sentido de que, si llegado ese momento, vuelve a
  pulsar <key|C-s> continuar� desde comienzo del documento. Puede teclear
  <key|backspace> para deshacer la digitaci�n de teclas durante una b�squeda.

  La b�squeda descrita es hacia adelante, empezando en la posici�n actual del
  cursor. Tambi�n puede buscar hacia atr�s, utilizando <key|C-r>. Las
  b�squedas se producen s�lo en texto que est� en el mismo modo e idioma que
  los activos en la posici�n de comienzo. En otras palabras, al buscar
  <with|mode|math|x> en modo matem�tico no encontrar�s ninguna x de texto
  ordinario. Una limitaci�n actual es que la cadena de b�squeda puede
  contener s�lo texto ordinario y no s�mbolos matem�ticos o texto
  estructurado m�s complicado.

  La b�squeda con sustituci�n se arranca tecleando <key|C-=> o
  <apply|menu|Edit|Replace>. <TeXmacs> le pide la cadena que quiere sustituir
  y la cadena que aparecer� en su lugar. Cada vez que se encuentre una de las
  cadenas a sustituir se le pedir� que elija entre sustituirla (y), no
  sustituirla (n) y sustituir todas las apariciones siguientes (a). Como
  antes, la b�squeda con sustituci�n afecta s�lo al texto que est� en el
  mismo modo e idioma.

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
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Buscar>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Sustituir>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
