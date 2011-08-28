<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Etiquetas est�ndar>

  Varias etiquetas est�ndar est�n definidas en <tmdtd|std-markup>. Los
  siguientes etiquetas de contenido textual toman todas un argumento. La
  maryor�a puede ser encontradas en el men� <apply|menu|Insert|Content tag>.

  <\description>
    <expand|item*|<markup|strong>>Indica una <strong|importante> regi�n de
    texto. Pueden entrar esta etiqueta v�a <apply|menu|Insert|Content
    tag|Strong>.

    <expand|item*|<markup|em>>Enfatiza una regi�n del texto como en ``la cosa
    <em|real>''. Esta etiqueta corresponde a la entrada del men�
    \ <apply|menu|Insert|Content tag|Emphasize>.

    <expand|item*|<markup|dfn>>Para definiciones como ``un <dfn|gnu> es una
    bestia cornuda''. Esta etiqueta corresponde a <apply|menu|Insert|Content
    tag|Definition>.

    <expand|item*|<markup|samp>>Una secuencia de caracteres literales como la
    ligadura <samp|ae> (�). Puede obtener esta etiqueta v�a
    <apply|menu|Insert|Content tag|Sample>.

    <expand|item*|<markup|name>>El nombre de una cosa o concepto particular
    como el sistema <name|Linux>. Esta etiqueta es obtenida usando
    <apply|menu|Insert|Content tag|Name>.

    <expand|item*|<markup|person>>El nombre de una persona como <name|Joris>.
    Esta etiqueta corresponde a <apply|menu|Insert|Content tag|Person>.

    <expand|item*|<markup|cite*>>Una cita bibliogr�fica particuar como un
    libro o revista. Ejemplo: <expand|cite*|Moby Dick> de Melville. Esta
    etiqueta, que es obtenida usando <apply|menu|Insert|Content tag|Cite>, no
    debe ser confundida con <markup|cite>. La �ltima etiqueta es tambi�n
    usada para citas, pero cuando el artumento se refiere a una entrada en
    una base de datos con referencias bibliogr�ficas.

    <expand|item*|<markup|abbr>>Una abreviaci�n. Ejemplo: trabajo en el
    <abbr|C.N.R.S.> Una abreviaci�n es creada usando <apply|menu|Insert|Content
    tag|Abbreviation> o el atajo de teclado <key|text a>.

    <expand|item*|<markup|acronym>>Un acr�nimo es una abreviaci�n formada por
    la priemra letra de cada palabra en un nombre o frase, tal como
    <acronym|HTML> o <acronym|IBM>. En particular, las letras no est�n
    separadas por puntos. Puede entrar un acr�nimo usando
    <apply|menu|Insert|Content tag|Acronym>.

    <expand|item*|<markup|verbatim>>Texto literal como la salida de un
    programa de computador. Ejemplo: El programa dijo <verbatim|hola>. Puede
    ingresar texto literal v�a <apply|menu|Insert|Content tag|Verbatim>. La
    etiqueta tambi�n puede ser usada como un entorno para texto multip�rrafo.

    <expand|item*|<markup|kbd>>Texto que deber�a ser ingresado en el teclado.
    Ejemplo: por favor presione <kbd|enter>. Esta etiqueta corresponde a la
    entrada de men� <apply|menu|Insert|Content tag|Keyboard>.

    <expand|item*|<markup|code*>>C�digo de un programa de computadora como en
    ``<expand|code*|cout \<less\>\<less\> 1+1;> produce <verbatim|2>''. Esto
    es entrado usando <apply|menu|Insert|Content tag|Code>. Para trozos m�s
    largos de c�digo, deber�a usar el entorno <markup|code>.

    <expand|item*|<markup|var>>Variables en un programa de computador como en
    <verbatim|cp <var|src-file> <var|dest-file>>. Esta etiqueta corresponde a
    la entrada del men� <apply|menu|Insert|Content tag|Variable>.

    <expand|item*|<markup|math>>Esta es una etiqueta que ser� usada en el
    futuro para texto matem�tico dentro del texto regular. Ejemplo: la
    f�rmula <math|sin<rsup|2> x+cos<rsup|2> x=1> es bien-conocida.

    <expand|item*|<markup|op>>Esta es una etiqueta que puede ser usada dentro
    del entorno matem�tico para especificar que un operador puede ser
    considerado en s� mismo, sin alg�n otro argumento. Ejemplo: la operaci�n
    <math|<op|+>> es una funci�n de <with|mode|math|\<bbb-R\><rsup|2>> a
    <with|mode|math|\<bbb-R\>>. Esta etiqueta puede entrar en desuso.

    <expand|item*|<markup|tt>>Este es una etiqueta f�sica para una frase con
    texto de m�quina de escribir. Es usado por compabilidad con <name|HTML>,
    pero no recomendamos su uso.
  </description>

  Los siguientes son ambientes est�ndar:

  <\description>
    <expand|item*|<markup|verbatim>>Descrito arriba.

    <expand|item*|<markup|code>>Similar a <markup|code*>, pero para piezas de
    c�digo de varias l�neas.

    <expand|item*|<markup|quote>>Entorno para citaciones cortas (de un
    p�rrafo).

    <expand|item*|<markup|quotation>>Entorno para citaciones largas.
    (multi-p�rrafo).

    <expand|item*|<markup|verse>>Entorno para poes�a.

    <expand|item*|<markup|center>>Esta es una etiqueta f�sica para centrar
    una o varias l�neas de texto. Es usado para compatibilidad con
    <name|HTML>, pero no recomendamos su uso.
  </description>

  Algunos ambientes tabulares est�ndar son:

  <\description>
    <expand|item*|<markup|tabular*>>Tablas centradas.

    <expand|item*|<markup|block>>Tablas alineadas a la izquierda con un borde
    est�ndar de <verbatim|1ln> de ancho.

    <expand|item*|<markup|block*>>Tablas centradas con un borde est�ndar de
    <verbatim|1ln> de ancho.
  </description>

  Las siguientes etiquetas miscel�neas no tomas argumentos:

  <\description>
    <expand|item*|<markup|TeXmacs>>El logo de <TeXmacs>.

    <expand|item*|<markup|TeX>>El logo de <TeX>.

    <expand|item*|<markup|LaTeX>>El logo de <LaTeX>.

    <expand|item*|<markup|hflush>>Usado por los desarrolladores para nivelar
    a la derecha en la definici�n de entornos.

    <expand|item*|<markup|hrule>>Una regla horizontal como la que ve abajo:

    <value|hrule>
  </description>

  Las siguientes etiquetas miscel�neas toman todas uno o m�s argumentos:

  <\description>
    <expand|item*|<markup|overline>>Para <overline|texto con una supral�nea>,
    que puede ser cubierto a lo largo de varias l�neas.

    <expand|item*|<markup|underline>>Para <underline|texto subrayado>, que
    puede ser cubierto a lo largo de varias l�neas..

    <expand|item*|<markup|fold>>Macro con dos argumentos. El primer argumento
    es mostrado y el segundo es ignorado: El macro corresponde a la
    presentaci�n plegada de una pieza de contenido asociada a un corto t�tulo
    o resumen. El segundo argumento puede ser hecho visible usando
    <apply|menu|Insert|Switch|Unfold>.

    <expand|item*|<markup|unfold>>Macro con dos argumentos <var|x> y <var|y>,
    que produce la presentaci�n desplegada de una pieza de contenidos <var|y>
    asociado a un peque�o t�tulo o resumen <var|x>. El segundo argumento
    puede ser hecho visible usando <apply|menu|Insert|Switch|Fold>.

    <expand|item*|<markup|switch>>Macro con dos argumentos <var|x> y <var|y>,
    donde <var|y> es un conjunto de posibles representaciones del intercambio
    y <var|x> la representaci�n actual. Las teclas de funci�n <key|F9>,
    <key|F10>, <key|F11> y <key|F12> pueden ser usadas para intercambiar
    entre diferentes representaciones.

    <expand|item*|<markup|phantom>>Funci�n con un argumento <var|x>. Esta
    etiqueta toma tanto espacio como la composici�n tipogr�fica del argumento
    <var|x> tomar�a, pero <var|x> no es mostrado. Por ejemplo, el texto
    ``fantasma'' como un argumento de <markup|phantom> produce
    ``<apply|phantom|fantasma>''.

    <expand|item*|<markup|set-header>>Funci�n con un argumento para cambiar
    permanentemente la cabecera. Note que ciertas etiquetas en el archivo de
    estilo, como las etiquetas de secci�n, pueden sobreescribir tales cambios
    manuales.

    <expand|item*|<markup|set-footer>>Funci�n con un argumento para cambiar
    permanentemente el pie.
  </description>

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <expand|tmdoc-license|El permiso est� garantizado para copiar, distribuir
  y/o modificar este documento bajo los terminos de la GNU Free Documentation
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
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-60|<tuple|<uninit>|?>>
    <associate|idx-50|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-61|<tuple|<uninit>|?>>
    <associate|idx-51|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-52|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-53|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-54|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-55|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-56|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-57|<tuple|<uninit>|?>>
    <associate|idx-47|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-58|<tuple|<uninit>|?>>
    <associate|idx-48|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-59|<tuple|<uninit>|?>>
    <associate|idx-49|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-markup>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|strong>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Resaltado>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font family|<quote|ss>|Con
      �nfasis>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dfn>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Definici�n>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|samp>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Muestra>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|name>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Nombre>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|person>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Persona>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite*>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Cita>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|abbr>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Abreviatura>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|acronym>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Acr�nimo>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font family|<quote|ss>|Sin
      formato>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Teclado>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|C�digo>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|var>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Marcar contenido>|<with|font
      family|<quote|ss>|Variable>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|math>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|op>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tt>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|center>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular*>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block*>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeXmacs>>|<pageref|idx-44>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeX>>|<pageref|idx-45>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|LaTeX>>|<pageref|idx-46>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hflush>>|<pageref|idx-47>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hrule>>|<pageref|idx-48>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|overline>>|<pageref|idx-49>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|underline>>|<pageref|idx-50>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|fold>>|<pageref|idx-51>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Fotogramas>|<with|font
      family|<quote|ss>|Unfold>>|<pageref|idx-52>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|unfold>>|<pageref|idx-53>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Fotogramas>|<with|font
      family|<quote|ss>|Plegar>>|<pageref|idx-54>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|switch>>|<pageref|idx-55>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-56>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-57>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-header>>|<pageref|idx-58>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-footer>>|<pageref|idx-59>>
    </associate>
  </collection>
</auxiliary>
