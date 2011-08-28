<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Entornos matem�ticos>

  El d.t.d. <tmdtd|env-math> espeficica cuales entornos matem�ticos pueden
  ser usados dentro del modo de texto. En otras palabras, los entornos
  deber�an ser usados detro del mode de texto, pero sus cuerpos pueden
  contener f�rmulas matem�ticas o tablas de f�rmulas matem�ticas.

  <\description>
    <expand|item*|<markup|equation>>Una ecuaci�n numerada.

    <expand|item*|<markup|equation*>>Una ecuaci�n no numerada.

    <expand|item*|<markup|eqnarray>> Un arreglo de ecuaciones numeradas. (no
    debe ser�a usarse a�n).

    <expand|item*|<markup|eqnarray*>>Un arreglo de ecuaciones no numeradas.
  </description>

  Dentro del entorno <markup|eqnarray*>, usted puede usar la etiqueta
  <markup|eqnumber> a fin de numerar una ecuaci�n.

  <\warning>
    La numeraci�n de ecuaciones dentro de tablas no es a�n como deber�a ser.
    En particular, la etiqueta <markup|eqnarray> es equivalente a
    <markup|eqnarray*> en el momento. Despu�s, cuando la etiqueta
    <markup|eqnarray> sea implementada correctamente, usted tambi�n dispondr�
    de una etiqueta <markup|nonumber> a fin de suprimir el n�mero de una
    ecuaci�n y un paquete de estilo para numerar ecuaciones al lado
    izquierdo.
  </warning>

  <\warning>
    No hay opci�n disponible para numerar ecuaciones a al lado izquierdo a�n.
    Sin embargo, puede usar la etiqueta manual <markup|leqnumber> para esto.
    Tambi�n tiene la etiqueta <markup|nextnumber> el cual directamente
    muestra el siguiente n�mero e incremente el contador de ecuaciones.
  </warning>

  <\warning>
    No alentamos el uso de los entornos AMS-<TeX>, <verbatim|align>,
    <verbatim|gather> y <verbatim|split>. No obstante, est�n disponibles bajo
    los nombres <markup|align>, <markup|gather>, <markup|eqsplit> junto con
    sus variantes <markup|align*>, <markup|gather*> and <markup|eqsplit*>. En
    el futuro, planeamos proveer entornos m�s poderosos.
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-20|<tuple|3|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-21|<tuple|3|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-22|<tuple|3|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-23|<tuple|3|?>>
    <associate|idx-14|<tuple|3|?>>
    <associate|idx-24|<tuple|3|?>>
    <associate|idx-15|<tuple|3|?>>
    <associate|idx-25|<tuple|3|?>>
    <associate|idx-16|<tuple|3|?>>
    <associate|idx-17|<tuple|3|?>>
    <associate|idx-18|<tuple|3|?>>
    <associate|idx-19|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnumber>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nonumber>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|leqnumber>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nextnumber>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit*>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
