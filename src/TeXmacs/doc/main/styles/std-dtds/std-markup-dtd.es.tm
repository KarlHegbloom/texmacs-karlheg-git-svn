<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Etiquetas est�ndar>

  Varias etiquetas est�ndar est�n definidas en <tmdtd|std-markup>. Los
  siguientes etiquetas de contenido textual toman todas un argumento. La
  maryor�a puede ser encontradas en el men� <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Indica una <strong|importante> regi�n de texto. Pueden entrar esta
    etiqueta v�a <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Enfatiza una regi�n del texto como en ``la cosa <em|real>''. Esta
    etiqueta corresponde a la entrada del men� \ <menu|Insert|Content
    tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Para definiciones como ``un <dfn|gnu> es una bestia cornuda''. Esta
    etiqueta corresponde a <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Una secuencia de caracteres literales como la ligadura <samp|ae> (�).
    Puede obtener esta etiqueta v�a <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    El nombre de una cosa o concepto particular como el sistema <name|Linux>.
    Esta etiqueta es obtenida usando <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<markup|person>>
    El nombre de una persona como <name|Joris>. Esta etiqueta corresponde a
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Una cita bibliogr�fica particuar como un libro o revista. Ejemplo:
    <cite*|Moby Dick> de Melville. Esta etiqueta, que es obtenida usando
    <menu|Insert|Content tag|Cite>, no debe ser confundida con <markup|cite>.
    La �ltima etiqueta es tambi�n usada para citas, pero cuando el artumento
    se refiere a una entrada en una base de datos con referencias
    bibliogr�ficas.
  </explain>

  <\explain|<markup|abbr>>
    Una abreviaci�n. Ejemplo: trabajo en el <abbr|C.N.R.S.> Una abreviaci�n
    es creada usando <menu|Insert|Content tag|Abbreviation> o el atajo de
    teclado <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Un acr�nimo es una abreviaci�n formada por la priemra letra de cada
    palabra en un nombre o frase, tal como <acronym|HTML> o <acronym|IBM>. En
    particular, las letras no est�n separadas por puntos. Puede entrar un
    acr�nimo usando <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Texto literal como la salida de un programa de computador. Ejemplo: El
    programa dijo <verbatim|hola>. Puede ingresar texto literal v�a
    <menu|Insert|Content tag|Verbatim>. La etiqueta tambi�n puede ser usada
    como un entorno para texto multip�rrafo.
  </explain>

  <\explain|<markup|kbd>>
    Texto que deber�a ser ingresado en el teclado. Ejemplo: por favor
    presione <kbd|enter>. Esta etiqueta corresponde a la entrada de men�
    <menu|Insert|Content tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    C�digo de un programa de computadora como en ``<code*|cout
    \<less\>\<less\> 1+1;> produce <verbatim|2>''. Esto es entrado usando
    <menu|Insert|Content tag|Code>. Para trozos m�s largos de c�digo, deber�a
    usar el entorno <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Variables en un programa de computador como en <verbatim|cp
    <var|src-file> <var|dest-file>>. Esta etiqueta corresponde a la entrada
    del men� <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Esta es una etiqueta que ser� usada en el futuro para texto matem�tico
    dentro del texto regular. Ejemplo: la f�rmula <math|sin<rsup|2>
    x+cos<rsup|2> x=1> es bien-conocida.
  </explain>

  <\explain|<markup|op>>
    Esta es una etiqueta que puede ser usada dentro del entorno matem�tico
    para especificar que un operador puede ser considerado en s� mismo, sin
    alg�n otro argumento. Ejemplo: la operaci�n <math|<op|+>> es una funci�n
    de <math|\<bbb-R\><rsup|2>> a <math|\<bbb-R\>>. Esta etiqueta puede
    entrar en desuso.
  </explain>

  <\explain|<markup|tt>>
    Este es una etiqueta f�sica para una frase con texto de m�quina de
    escribir. Es usado por compabilidad con <name|HTML>, pero no recomendamos
    su uso.
  </explain>

  Los siguientes son ambientes est�ndar:

  <\explain|<markup|verbatim>>
    Descrito arriba.
  </explain>

  <\explain|<markup|code>>
    Similar a <markup|code*>, pero para piezas de c�digo de varias l�neas.
  </explain>

  <\explain|<markup|quote>>
    Entorno para citaciones cortas (de un p�rrafo).
  </explain>

  <\explain|<markup|quotation>>
    Entorno para citaciones largas. (multi-p�rrafo).
  </explain>

  <\explain|<markup|verse>>
    Entorno para poes�a.
  </explain>

  <\explain|<markup|center>>
    Esta es una etiqueta f�sica para centrar una o varias l�neas de texto. Es
    usado para compatibilidad con <name|HTML>, pero no recomendamos su uso.
  </explain>

  Algunos ambientes tabulares est�ndar son:

  <\explain|<markup|tabular*>>
    Tablas centradas.
  </explain>

  <\explain|<markup|block>>
    Tablas alineadas a la izquierda con un borde est�ndar de <verbatim|1ln>
    de ancho.
  </explain>

  <\explain|<markup|block*>>
    Tablas centradas con un borde est�ndar de <verbatim|1ln> de ancho.
  </explain>

  Las siguientes etiquetas miscel�neas no tomas argumentos:

  <\explain|<markup|TeXmacs>>
    El logo de <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    El logo de <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    El logo de <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Usado por los desarrolladores para nivelar a la derecha en la definici�n
    de entornos.
  </explain>

  <\explain|<markup|hrule>>
    Una regla horizontal como la que ve abajo:

    <hrule>
  </explain>

  Las siguientes etiquetas miscel�neas toman todas uno o m�s argumentos:

  <\explain|<markup|overline>>
    Para <overline|texto con una supral�nea>, que puede ser cubierto a lo
    largo de varias l�neas.
  </explain>

  <\explain|<markup|underline>>
    Para <underline|texto subrayado>, que puede ser cubierto a lo largo de
    varias l�neas..
  </explain>

  <\explain|<markup|fold>>
    Macro con dos argumentos. El primer argumento es mostrado y el segundo es
    ignorado: El macro corresponde a la presentaci�n plegada de una pieza de
    contenido asociada a un corto t�tulo o resumen. El segundo argumento
    puede ser hecho visible usando <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|unfold>>
    Macro con dos argumentos <var|x> y <var|y>, que produce la presentaci�n
    desplegada de una pieza de contenidos <var|y> asociado a un peque�o
    t�tulo o resumen <var|x>. El segundo argumento puede ser hecho visible
    usando <menu|Insert|Switch|Fold>.
  </explain>

  <\explain|<markup|switch>>
    Macro con dos argumentos <var|x> y <var|y>, donde <var|y> es un conjunto
    de posibles representaciones del intercambio y <var|x> la representaci�n
    actual. Las teclas de funci�n <key|F9>, <key|F10>, <key|F11> y <key|F12>
    pueden ser usadas para intercambiar entre diferentes representaciones.
  </explain>

  <\explain|<markup|phantom>>
    Funci�n con un argumento <var|x>. Esta etiqueta toma tanto espacio como
    la composici�n tipogr�fica del argumento <var|x> tomar�a, pero <var|x> no
    es mostrado. Por ejemplo, el texto ``fantasma'' como un argumento de
    <markup|phantom> produce ``<phantom|fantasma>''.
  </explain>

  <\explain|<markup|set-header>>
    Funci�n con un argumento para cambiar permanentemente la cabecera. Note
    que ciertas etiquetas en el archivo de estilo, como las etiquetas de
    secci�n, pueden sobreescribir tales cambios manuales.
  </explain>

  <\explain|<markup|set-footer>>
    Funci�n con un argumento para cambiar permanentemente el pie.
  </explain>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
  C�rdenas>

  <tmdoc-license|El permiso est� garantizado para copiar, distribuir y/o
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