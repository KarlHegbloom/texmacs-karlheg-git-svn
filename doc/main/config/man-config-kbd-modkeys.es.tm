<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configuraci�n de las teclar modificadoras>

  <apply|TeXmacs> usa cinco modificadores de teclado principales:
  <key|S->, <key|control>, <key|alternate>, <key|meta> e <key|hyper>, que
  son abreviadas como <key|S->, <key|C->, <key|A->, <key|M-> and <key|H->.
  Las teclas <key|S-> y <key|control> est�n presentes en virtualmente
  todos los teclados y la tecla <key|alternate> en la mayor�a. Muchos
  fabricantes de PC's estos d�as tienen tambi�n una tecla \ <key|windows>, la
  cual es usualmente equivalente a la tecla <key|meta> para <TeXmacs>.

  Antes de reconfigurar su teclado, deber�a primero chequear que esto es de
  hecho necesario. Si tiene teclas que corresponden a <key|S->,
  <key|control>, <key|alternate> y <key|meta> en una forma apropidad,
  entonces problamente no requiera hacer algo. Una posible excepci�n es
  cuando usted quiera sar una tecla simple como <key|capslock> para teclear
  s�mbolos matem�ticos. En ese caso, deber�a hacer corresponder
  <key|capslock> a <key|hyper>.

  A fin de reconfigurar el teclado, simplemente selccione el moficador l�gico
  que quiere corresponder a una tecla f�sica dada en
  <apply|menu|Edit|Preferences|Keyboard>. Por ejemplo, seleccionado
  <apply|menu|Windows key|Map to M modifier>, la tecla <key|windows>
  corresponder� al modificador \ <key|meta>. Similarmente, cuanco seleccione
  <apply|menu|Caps-lock key|Map to H modifier>, la tecla <key|capslock>
  corresponder� al modificador <key|hyper>.

  Infortunadamente, X Window s�lo permite una reconfiguraci�n a lo ancho del
  sistema. Consecuentemente, si usted reconfigura la tecla <key|capslock>
  dentro de <apply|TeXmacs>, entonces el nuevo comportamiento de
  <key|capslock> afectar� todas las otras aplicaciones tambi�n. Es por esto
  importante reconfigurar s�lo aquellas teclas que usted no usa para algo m�s
  en otras aplicaciones. Por ejemplo, la tecla <key|windows> no es usada por
  muchas aplicaciones, as� que generamente no hace ning�n da�o
  reconfigurarla. Puede tambi�n preferir realizar alguna configuraci�n a lo
  ancho del sistema. Esto puede ser hecho con el comando <verbatim|xmodmap>;
  vea la p�gina correspondiente del manual para mayor informaci�n.

  En ciertos casos, usted ya tiene teclas en su teclado que corresponde a
  <key|alter>, <key|meta> e <key|hyper>, pero no en la forma en que usted
  quiere. Esto puede ser hecho reasinando los prefijos \ <key|A->, <key|M-> y
  <key|H-> a otros modificadores l�gicos en el primer grupo de submenus de
  <apply|menu|Edit|Preferences|Keyboard>.

  Por ejemplo, para compatibilidad con <name|Emacs>, podr�a querer permutar
  la tecla <key|meta> o <key|windows> con <key|alter> sin hacer ning�n cambio
  a lo amplio de sistema. Esto puede ser hecho encontrando que modificadores
  corresponde a estas teclas; usualmente esto ser� <key|Mod1> para
  <key|alter> y <key|Mod4> para <key|meta> o <key|windows>. Realizaremos las
  permutaciones necesarias en <apply|menu|Edit|Preferences|Keyboard>,
  selecionando <apply|menu|A modifier|Equivalent for Mod4> y <apply|menu|M
  modifier|Equivalent for Mod1>.

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tecla de Windows>|<with|font
      family|<quote|ss>|Map to M modifier>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Caps-lock key>|<with|font
      family|<quote|ss>|Map to H modifier>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Preferencias>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|A modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|M modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
