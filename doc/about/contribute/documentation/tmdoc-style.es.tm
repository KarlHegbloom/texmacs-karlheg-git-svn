<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Usar el estilo tmdoc>

  Adem�s de los macros de <apply|hyper-link|informaci�n de
  copyright|copyright.es.tm> y <apply|hyper-link|macros
  transversales|traversal.es.tm>, los cuales han sido docuementados antes, el
  estilo <tmstyle|tmdoc> viene con cierto n�mero de otros macros y funciones,
  las cuales usted debe usar cuando sea apropiado:

  <\description>
    <expand|item*|<markup|key>>Este macro es usado para indicar una entrada
    de teclado como <key|C-x C-s>. Los macros especializados
    <markup|kbd-gen>, <markup|kbd-text>, <markup|kbd-math>,
    <markup|kbd-symb>, <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>,
    <markup|kbd-exec> y <markup|kbd-table> son usados para entradas de
    teclado correspondientes a tipos espec�ficos de acci�n o modos. Por
    ejemplo, <markup|kbd-math> corresponde a atajos de teclado para
    operaciones matem�ticas, tales como <expand|kbd-math|f>, el cual inicia
    una fracci�n.

    <expand|item*|<markup|menu>>Esta funci�n con un n�mero arbitrario de
    argumentos indica un men� como <apply|menu|File> o
    <apply|menu|Document|Language>. Las entradas de menu con autom�ticamente
    traducidas por esta funci�n.

    <expand|item*|<markup|markup>>Esta macro es usado a fin de indicar un
    macro o una funci�n como <markup|section>.

    <expand|item*|<markup|tmstyle>>Este macro indica el nombre de un archivo
    estilo <TeXmacs> o un paquete como <tmstyle|article>.

    <expand|item*|<markup|tmpackage>>Este macro indica el nombre de un
    paquete <TeXmacs> como <tmpackage|std-markup>.

    <expand|item*|<markup|tmdtd>>Este macro indica de un <abbr|d.t.d.> de
    <TeXmacs> like <tmdtd|number-env>.
  </description>

  Note que los contenidos de ninguna de las etiquetas de arriba deben ser
  traducidos a lenguajes extrangeros. De hecho, para etiquetas de men�, las
  traducciones son hechas autom�ticamente, as� se mantienen sincronizadas con
  las traducciones en los menus acutales de <TeXmacs>. En los casos de
  etiquetas, estilos, paquetes y <abbr|d.t.d.>s, es importante mantener el
  nombre original, porque a menudo corresponde a un nombre de archivo.

  Los siguientes macros y funciones son usadas para prop�sitos de enlace e
  indexaci�n, aunque ellos deben ser mejorados en el futuro:

  <\description>
    <expand|item*|<markup|simple-link>>Este macro toma una URL
    <with|mode|math|x> como argumento y es un hiperenlace con un nombre y
    destinaci�n <with|mode|math|x>.

    <expand|item*|<markup|hyper-link>>Este macro is un hiperenlace usual.

    <expand|item*|<markup|concept-link>>Este macro toma un concepto como
    argumento. Despu�s un hiperenlace apropriado podr�a ser creado
    automaticamente desde esta y la otra documentaci�n.

    <expand|item*|<markup|only-index>>Indexa una cadena simple.

    <expand|item*|<markup|def-index>>Definici�n de un nuevo concepto; el
    texto es impreso en it�lica e indexado.

    <expand|item*|<markup|re-index>>Reaparici�n de un concepto ya definido;
    el texto es impreso en romano y puesto en el �ndice.
  </description>

  Las siguientes etiquetas son tambi�n frecuentemente usadas:

  <\description>
    <expand|item*|<markup|icon>>Enlace a un icono en un directorio central
    como <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.

    <expand|item*|<markup|screenshot>>Enlace a una captura de pantalla. Las
    capturas de pantalla actuales son almacenadas en un directorio central
    como <verbatim|$TEXMACS_PATH/doc/images/screenshots>.

    <expand|item*|<markup|scheme>>El lenguage <value|scheme>.

    <expand|item*|<markup|framed-fragment>>Para mostrar un pedazo de c�digo
    en un marco agradable.

    <expand|item*|<markup|scheme-fragment>>Para c�digo <value|scheme>
    multi-p�rrafo.

    <expand|item*|<markup|tm-fragment>>Para un pedazos de etiquetas de c�digo
    <TeXmacs> en formato <value|scheme>.

    <expand|item*|<markup|descriptive-table>>Para tablas descriptivas; tales
    tablas pueden ser usadas para documentar listas de atajos de teclado,
    diferentes tipos de etiquetas, etc.
  </description>

  El estilo <tmstyle|tmdoc> herededa de el estilo <tmstyle|generic> y usted
  debe usar macros como <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> para este estilo donde sea apropiado.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir
  Luna C�rdenas>

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
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-50|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-51|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-52|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-47|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-48|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-49|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|key>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-gen>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-text>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-symb>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-big>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-large>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-ia>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-exec>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-table>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd-math>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|menu>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Idioma>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|markup>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmstyle>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|article>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmpackage>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|std-markup>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdtd>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|number-env>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|simple-link>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hyper-link>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|concept-link>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|only-index>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|def-index>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|re-index>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|icon>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|screenshot>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|framed-fragment>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|scheme-code>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tm-fragment>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|descriptive-table>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|generic>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-41>>
    </associate>
  </collection>
</auxiliary>
