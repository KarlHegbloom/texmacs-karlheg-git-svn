<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generar un �ndice>

  Para la generaci�n de un �ndice, primero tiene que crear las entradas de
  �ndice en su documento mediante <apply|menu|Insert|Link|Index entry>.
  Despu�s debes colocar el cursor en el lugar donde desea que aparezca el
  �ndice de materias y pulsar en <apply|menu|Insert|Automatic|Index>. El
  �ndice de materias se crea entonces de un modo similar a la tabla de
  contenidos.

  En el men� <apply|menu|Insertar|Enlace|Entrada de �ndice> se pueden
  encontrar diversos tipos de entradas de �ndice. Las m�s sencillas son
  ``principal'', ``sub'' y ``subsub'', que son macros con uno, dos y tres
  argumentos respectivamente. Las entradas de tipo ``sub'' y ``subsub''
  pueden usarse para subordinar unas entradas a otras.

  Una entrada de �ndice compleja toma cuatro argumentos. El primero es una
  clave que sirve exclusivamente para ubicar la entrada en el �ndice, y debe
  ser una ``tupla'' (creada usando <expand|kbd-ia|\<less\>>) cuyo primer
  componente es la categor�a principal, el segundo la subcategor�a, etc. El
  segundo argumento de una entrada de �ndice compleja es o bien ``blank'' o
  ``strong''; en este �ltimo caso, el n�mero de p�gina de la entrada
  aparecer� en negrita. El tercer argumento se suele dejar vac�o, pero si
  crea dos entradas de �ndice con el mismo tercer argumento (no vac�o,
  claro), entonces se crear� un rango de n�meros de p�gina. El cuarto
  argumento, que es, de nuevo, una tupla, es la entrada en s�.

  Tambi�n es posible crear una l�nea en el �ndice sin n�mero de p�gina,
  usando ``interject'' en <apply|menu|Insert|Link|Index entry>. El primer
  argumento de este macro es una clave para ordenar esta l�nea en el �ndice.
  El segundo argumento contiene el texto que saldr� en el �ndice. Esta
  construcci�n puede ser �til para crear diferentes secciones ``A'', ``B'' en
  su �ndice.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
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
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      �ndice>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Autom�tico>|<with|font family|<quote|ss>|�ndice de
      materias>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      �ndice>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Entrada de
      �ndice>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
