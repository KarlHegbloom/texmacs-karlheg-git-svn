<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Entornos matem�ticos>

  El d.t.d. <tmdtd|env-math> espeficica cuales entornos matem�ticos pueden
  ser usados dentro del modo de texto. En otras palabras, los entornos
  deber�an ser usados detro del mode de texto, pero sus cuerpos pueden
  contener f�rmulas matem�ticas o tablas de f�rmulas matem�ticas.

  <\explain|<markup|equation>>
    Una ecuaci�n numerada.
  </explain>

  <\explain|<markup|equation*>>
    Una ecuaci�n no numerada.
  </explain>

  <\explain|<markup|eqnarray>>
    \ Un arreglo de ecuaciones numeradas. (no debe ser�a usarse a�n).
  </explain>

  <\explain|<markup|eqnarray*>>
    Un arreglo de ecuaciones no numeradas.
  </explain>

  Dentro del entorno <markup|eqnarray*>, usted puede usar la etiqueta
  <markup|eq-number> a fin de numerar una ecuaci�n.

  <\warning>
    La numeraci�n de ecuaciones dentro de tablas no es a�n como deber�a ser.
    En particular, la etiqueta <markup|eqnarray> es equivalente a
    <markup|eqnarray*> en el momento. Despu�s, cuando la etiqueta
    <markup|eqnarray> sea implementada correctamente, usted tambi�n dispondr�
    de una etiqueta <markup|no-number> a fin de suprimir el n�mero de una
    ecuaci�n y un paquete de estilo para numerar ecuaciones al lado
    izquierdo.
  </warning>

  <\warning>
    No hay opci�n disponible para numerar ecuaciones a al lado izquierdo a�n.
    Sin embargo, puede usar la etiqueta manual <markup|leq-number> para esto.
    Tambi�n tiene la etiqueta <markup|next-number> el cual directamente
    muestra el siguiente n�mero e incremente el contador de ecuaciones.
  </warning>

  <\warning>
    No alentamos el uso de los entornos AMS-<TeX>, <verbatim|align>,
    <verbatim|gather> y <verbatim|split>. No obstante, est�n disponibles bajo
    los nombres <markup|align>, <markup|gather>, <markup|eqsplit> junto con
    sus variantes <markup|align*>, <markup|gather*> and <markup|eqsplit*>. En
    el futuro, planeamos proveer entornos m�s poderosos.
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