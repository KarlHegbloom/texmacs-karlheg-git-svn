<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, funciones y variables de entorno >

  Las principales combinaciones de teclas que usted debe saber para escribir
  archivos de estilo son las siguientes:

  <\description>
    <expand|item*|<expand|kbd-gen|=>>crea una nueva asignaci�n. El primer
    argumento es el nombre de un nuevo comando y el segundo una expresi�n.

    <expand|item*|<expand|kbd-gen|w>>permite cambiar localmente una o m�s
    variables de entorno. Las sentencias <with|font family|tt|with> son de la
    forma <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>,
    donde los <with|mode|math|x<rsub|i>> son los nombres de las variables,
    los <with|mode|math|a<rsub|i>> son sus valores locales, y
    <with|mode|math|b> es el texto sobre el que se aplica el entorno local.

    <expand|item*|<expand|kbd-gen|m>>crea un macro. Los argumentos se
    insertan utilizando la tecla <key|tab>.

    <expand|item*|<expand|kbd-gen|f>>crea una funci�n. Los argumentos se
    insertan utilizando la tecla <key|tab>.

    <expand|item*|<expand|kbd-ia|#>>obtiene el valor de un argumento del
    macro.

    <expand|item*|<expand|kbd-ia|v>>obtiene el valor de una variable de
    entorno.

    <expand|item*|<expand|kbd-ia|e>>expande el macro con cero o m�s
    argumentos.

    <expand|item*|<expand|kbd-ia|a>>aplica una funci�n a cero o m�s
    argumentos.
  </description>

  M�s precisamente, cuando se evalua una expasi�n de macro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> creada por
  <expand|kbd-ia|e>, las siguientes acciones son llevadas a cabo:

  <\itemize>
    <item>Si <with|mode|math|a> no es una cadena ni un macro, entonces
    <with|mode|math|a> se eval�a una sola vez. Esto produce o bien un nombre
    de macro o una expresi�n de macro <with|mode|math|f>.

    <item>Si obtenemos el nombre de un macro, entonces reemplazamos
    <with|mode|math|f> por el valor de la variable de entorno
    <with|mode|math|f>. Si despu�s de esto <with|mode|math|f >no es todav�a
    un macro, entonces devolvemos <with|mode|math|f>.

    <item>Sean <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> los argumentos
    de <with|mode|math|f> y <with|mode|math|b> su cuerpo (se desechan los
    argumentos superfluos; los argumentos faltantes toman la cadena vac�a
    como su valor por defecto). Entonces sustitu�mos
    <with|mode|math|x<rsub|i>> por cada <with|mode|math|y<rsub|i>> en
    <with|mode|math|b> y devolvemos el resultado que ha sido evaluado.
  </itemize>

  Las funciones son similares a los macros, salvo en que los argumentos de
  las aplicaciones de una funci�n son evaluados y no pueden ser editados
  directamente (primero necesitas desactivar la aplicaci�n de la funci�n,
  despu�s editar los argumentos y por �ltimo, reactivar). Tambi�n,
  <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> se consideran ahora como
  variables de entorno locales, a las que se atribuyen
  <with|mode|math|x<rsub|1>\<ldots\>x<rsub|n>> como sus valores. Estas
  variables locales no se recuerdan cuando una funci�n devuelve una funci�n
  que involucra esas variables.

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
