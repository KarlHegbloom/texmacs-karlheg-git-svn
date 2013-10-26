<TeXmacs|1.0.7.1>

<style|tmdoc>

<\body>
  <tmdoc-title|Allgemeine Kontextvariablen>

  <\explain>
    <var-val|mode|text><explain-synopsis|Hauptmodus>
  <|explain>
    Diese sehr wichtige Kontextvariable definiert den aktuellen <em|Modus>.
    Es gibt vier m�gliche Modi: <verbatim|text> (Text-Modus), <verbatim|math>
    (Mathematik-Modus), <verbatim|prog> (Programmier-Modus) und
    <verbatim|src> (Quellcode-Modus). Die Verhaltensweise des Editors
    \ (Men�s, Kurzbefehle, Schriftsatz, <abbr|usw.>) h�ngt empfindlich vom
    Modus ab. So kann beispielsweise der folgende Code zur Darstellung einer
    mathematischen Formel innerhalb eines Textes verwendet werden:

    <\tm-fragment>
      Die Formel <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>> ist gut
      bekannt.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|Die Formel <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>
      ist gut bekannt.>
    </tm-fragment>

    Einige andere Kontextvariablen (haupts�chlich Sprache und Schriftart)
    h�ngen auch von dem aktuellen Modus ab. Dabei benimmt sich der
    Quellcode-Modus immer �hnlich wie der text-Modus. Bei Kopier- und
    Einf�gungs-Vorg�ngen versucht <TeXmacs> den Modus zu erhalten.
  </explain>

  <\explain>
    <var-val|language|english>

    <var-val|math-language|texmath>

    <var-val|prog-language|scheme><explain-synopsis|Sprache>
  <|explain>
    Eine weitere wichtige Variable ist die<em| aktuelle Sprache>. Tats�chlich
    sind es drei solche Variablen, eine f�r jeden Modus, wobei Text und
    Quellcode gleichgesetzt werden. Die Sprache, in der der Inhalt
    geschrieben ist, bestimmt die Semantik. Diese wird f�r verschiedene
    Zwecke gebraucht:

    <\itemize>
      <item>Die Sprache spezifiziert Regeln f�r den Schriftsatz z.B.
      sprachspezifische Interpunktions- und Trennungsregeln und
      <src-var|math-language> sorgt f�r ad�quate Abstandsregeln bei
      mathematischen Operatoren.

      <item>Verschiedene Editiervorg�nge h�ngen von der Spracheinstellung ab,
      z.B. Suchoperationen, <TeXmacs> ist sowohl Modus- wie Sprach-abh�ngig.
      Auch bestimmt die Spracheinstellung, welches W�rterbuch bei der
      Rechtschreibpr�fung verwendet wird.\ 

      <item>Die Spracheinstellung regelt zusammen mit dem Modus und
      Stilvorgaben wie der Inhalt bei dem Wechsel von einem Modus zum anderen
      zu konvertieren ist.

      Derzeit sind noch keine wirklichen sprachabh�ngigen Konvertierungen
      implementiert. F�r die Zukunft kann man sich aber vorstellen, dass ein
      Textfragment, dass aus einem englischen Text ausgeschnitten wird, in
      ein franz�sisches Dokument �bersetzt eingef�gt wird. Auch k�nnte in
      mathematischen Dokumenten beispielsweise eine automatische
      Konvertierung von infix zu postfix-Notation vorgenommen werden.

      <item>Die Programmiersprache bestimmt die aktuell benutzte
      Skriptsprache. Andere Skriptsprachen als <scheme> k�nnen derzeit nur in
      interaktiven Sitzungen verwendet werden. In der Zukunft k�nnten
      Konstrukte wie <markup|extern> Programmiersprachen-abh�ngig werden.
    </itemize>

    Im Moment wird die aktuelle Sprache als ein Hinweis auf die Semantik von
    Text verwendet. Es ist nicht erforderlich, dass ein Text, der
    beispielsweise in Englisch geschrieben ist, keine Rechtschreibfehler
    enth�lt, oder dass eine mathematische Formel mathematisch oder semantisch
    korrekt ist. Jedoch ist es beabsichtigt, den Editor mehr und mehr dazu zu
    bringen, f�r korrekten Inhalt zu sorgen.

    Global kann die Sprache im Men� <menu|Document|Language> definiert
    werden, lokal mit <menu|Format|Language>.
  </explain>

  <\explain>
    <var-val|prog-session|default><explain-synopsis|Name der
    Programmiersitzung>
  <|explain>
    Die Kontextvariable wird zus�tzlich zur <src-var|prog-language>-Variablen
    benutzt, um die konkrete Implementierung und Version der
    Programmiersprache zu kennzeichnen. Im Fall von
    <hlink|<name|Maxima>|../../../main/interface/cas/man-maxima.en.tm> k�nnen
    unterschiedliche <name|Lisp>-Versionen Verwendung finden. Manchmal m�chte
    man auch unterschiedliche <name|Maxima>-Versionen parallel einsetzen.
  </explain>

  <\explain>
    <label|magnification><var-val|magnification|1><explain-synopsis|Vergr��erungsfaktor>
  <|explain>
    Diese Variable bestimmt, welche Vergr��erung bei der Darstellung auf den
    ganzen Inhalt angewendet werden soll. Vergr��erungsfaktoren �ber 1 werden
    typischerweise bei Pr�sentationen verwendet z.B. bei Pr�sentationen mit
    Beamer von einem Laptop.

    <\tm-fragment>
      normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>
    </tm-fragment>

    <\tm-fragment>
      <inactive*|normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>>
    </tm-fragment>

    Der Vergr��erungsfaktor sollte nicht mit der Schriftgr��e (<hlink|font
    size|env-font.en.tm#font-base-size>) verwechselt werden. Im Gegensatz zur
    Vergr��erung ver�ndert die Schriftgr��e die Form der Buchstaben. Der
    Vergr��erungsfaktor wir normalerweise f�r das ganze Dokument im Men�
    <menu|Document|Magnification> eingestellt.
  </explain>

  <\explain>
    <var-val|bg-color|white><explain-synopsis|Hintergrundfarbe>
  <|explain>
    Die Hintergrundfarbe des Dokuments wird im Men�
    <menu|Document|Color|Background> festgelegt.
  </explain>

  <\explain>
    <var-val|color|black><explain-synopsis|Vordergrundfarbe>
  <|explain>
    Die Vordergrundfarbe f�r Text und Graphik kann global im Men�
    <menu|Document|Color|Foreground> oder lokal im Men� <menu|Format|Color>
    eingestellt werden.
  </explain>

  <\explain>
    <var-val|preamble|false><explain-synopsis|Quellcode-Modus?>
  <|explain>
    Dieses Flag steuert, ob eine normales Textdokument oder eine
    Stil-Definition editiert wird. Der Quellcode-Modus (preamble mode) kann
    im Men� <menu|Document|Source|Edit source tree> ausgew�hlt werden.
  </explain>

  <\explain>
    <var-val|info-flag|short><explain-synopsis|Darstellung von
    Informations-Marken>
  <|explain>
    Diese Variable steuert die Darstellung von Informations-Marken, die in
    den Text eingef�gt werden, um sonst unsichtbare Marken oder
    Schriftsatz-Konstrukte erkennbar zu machen. Der <src-var|info-flag> kann
    die Werte <verbatim|none>, <verbatim|short> und <verbatim|detailed>
    annehmen:

    <\tm-fragment>
      <with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|<with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.>
    </tm-fragment>

    Normalerweise wird die Darstellung von Informations-Marken global im Men�
    <menu|Document|Informative flags> eingestellt.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>