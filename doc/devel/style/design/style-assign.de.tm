<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Zuweisungen>

  Alle vom Benutzer definierten <TeXmacs>-Makros und Stil-Variablen werden in
  der \Raktuellen Schriftsatzumgebung'' gespeichert. Da alle Dokumente in der
  Form von B�umen gespeichert werden, assoziiert diese Umgebung mit jeder
  Zeichenketten-Variablen einen zugeh�rigen \RBaum-Wert''. Variablen, deren
  Werte Makros sind, entsprechen neuen Konstrukten. Alle anderen sind normale
  <TeXmacs>-Umgebungsvariablen. Die Konstrukte, die auf der aktuellen
  Schriftsatzumgebung arbeiten, k�nnen �ber das Men� <menu|Source|Definition>
  erreicht werden.

  Sie k�nnen den Wert einer Umgebungsvariablen global �ndern, indem Sie das
  Konstrukt \ <markup|assign> benutzen, wie z.B. in:\ 

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hallo da!>>>
  </tm-fragment>

  Sie k�nnen auch nur lokal Werte �ndern mit dem <markup|with> Konstrukt:

  <\tm-fragment>
    <inactive*|<with|font-series|bold|color|red|Fetter roter Text>>
  </tm-fragment>

  Der Wert der Umgebungsvariablen kann mit dem <markup|value>-Konstrukt
  geholt werden, was z.B. in einem Z�hler verwendet werden k�nnte:\ 

  <\tm-fragment>
    <inactive*|<assign|mein-zaehler|<plus|<value|mein-zaehler>|1>>>
  </tm-fragment>

  Schlie�lich k�nnen Sie Umgebungsvariablen Logik-Eigenschaften verleihen,
  indem Sie das Konstrukt <markup|drd-props> benutzen. Das ist in
  <hyper-link|Makro-Konstrukte|../../format/stylesheet/prim-macro.de.tm>
  genauer erkl�rt.

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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