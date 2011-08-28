<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|<translate|standard length units|english|german>>

  Die Bl�tter von <TeXmacs> - B�umen enthalten entweder normalen Text oder
  spezielle Daten. \ <TeXmacs> kennt die folgenden atomaren Datentypen:

  <\description>
    <item*|Boolesche Zahlen>Entweder <verbatim|true> oder <verbatim|false>
    (richtig, falsch - ja, nein).

    <item*|Ganzzahl>Folge von Ziffern, vor denen ein Plus- oder Minus-Zeichen
    stehen darf.

    <item*|Gleitpunktzahlen>Zahlen mit Dezimalpunkt in der �blichen
    wissenschaftlichen Darstellung.

    <item*|L�ngen>Gleitpunktzahl gefolgt von einer L�ngeneinheit z. B.
    <verbatim|29.7cm> oder <verbatim|2fn>.
  </description>

  In diesem Abschnitt besprechen wir die <TeXmacs> - L�ngeneinheiten.

  Es gibt zwei prinzipiell verschiedene Arten von L�ngeneinheiten/L�ngen:
  fixe L�ngeneinheiten und Kontext-abh�ngige L�ngeneinheiten. Fixe
  L�ngeneinheiten sind die �blichen L�ngeneinheiten: ihre L�nge auf dem
  Bildschirm oder dem Papier ist vorgegeben und unver�nderlich. Die L�nge
  Kontext-abh�ngiger Einheiten wird dagegen den vorliegenden Gegebenheiten
  angepasst. Solche k�nnen beispielsweise Schriftart und Schriftgr��e sein.

  Einige der variablen L�ngeneinheiten sind <em|dehnbar>. Drei Kennzahlen
  charakterisieren eine <em|dehnbare> L�nge: die minimale L�nge, die
  Vorgabel�nge und die maximale L�nge. Wenn Zeilen oder Seiten im Blocksatz
  umgebrochen werden, werden <em|dehnbare L�ngen> so angepasst, dass ein
  optimales Druckbild entsteht.\ 

  Im Fall des Seitenumbruchs erlaubt die <src-var|page-flexibility> -
  Umgebung eine zus�tzliche Steuerung der Dehnbarkeit von Leerr�umen. Setzt
  man die <src-var|page-flexibility> auf <with|mode|math|1>, so verh�lt sich
  dehnbarer Leerraum wie gewohnt. Wird die <src-var|page-flexibility> dagegen
  auf <with|mode|math|0> gesetzt, dann werden dehnbare L�ngen
  <em|<strong|fix><strong|>>. Andere Werte beeinflussen das Verhalten linear.

  <paragraph*|Fixe L�ngeneinheiten>

  <\description>
    <item*|<code*|cm>>1 Zentimeter.

    <item*|<code*|mm>>1 Millimeter.

    <item*|<code*|in>>1 inch (Zoll).

    <item*|<code*|pt>>1 typographischer Punkt: 1/72 inch = 0.353 mm.
  </description>

  <paragraph*|kontext-abh�ngige L�ngeneinheiten>

  <\description>
    <item*|<code*|fn>>Die Nenngr��e der Schrift. Typischerweise sind die
    Basislinien zweier aufeinander folgender Zeilen durch den Abstand
    <verbatim|1fn> getrennt (in <TeXmacs> und <LaTeX> wird ein geringf�gig
    gr��erer Abstand benutzt, um obere und untere Indices besser darstellen
    zu k�nnen. Die Dehnbarkeit liegt f�r 1fn zwischen <verbatim|0.5fn> und
    <verbatim|1.5fn>.

    <item*|<verbatim|fn*>>Ist eine Variante von <verbatim|fn>, mit der
    Vorgabel�nge Null, die aber bis <verbatim|1fn> gedehnt werden kann.

    <item*|<code*|spc>>Die dehnbare Breite eines Leerzeichens in der
    aktuellen Schrift.

    <item*|<verbatim|ex>>Die H�he des Buchstabens ``x'' in der aktuellen
    Schrift.\ 

    <item*|<code*|ln>>Die Breite eines gut aussehenden Bruchstrichs in der
    aktuellen Schrift.

    <item*|<verbatim|yfrac>>Der Abstand eines Bruchstrichs von der Basislinie
    in der aktuellen Schrift. <line-break>(ungef�hr <verbatim|0.5ex>).

    <item*|<code*|sep>>Ein typischer Abstand zwischen Text und Graphik in der
    aktuellen Schrift, der ben�tigt wird, um den Text lesbar darzustellen.
    Beispielsweise wird der Z�hler eines Bruchs um den Betrag <verbatim|1sep>
    nach oben gesetzt.
  </description>

  <paragraph*|weitere L�ngeneinheiten>

  <\description>
    <item*|<code*|par>>Die zul�ssige Text-breite in einem Absatz. Sie h�ngt
    von der Papiergr��e, den R�ndern, der Spaltenzahl, dem Spaltenabstand
    usw. ab.\ 

    <item*|<verbatim|pag>>Die L�nge des Haupt-Textes einer Seite. �hnlich wie
    \ <verbatim|par>, wird diese L�ngeneinheit von der Papiergr��e, den
    R�ndern usw. beeinflusst.

    <item*|<code*|px>>1 Bildschirm-Pixel. Diese L�ngeneinheit h�ngt von der
    Bildschirmaufl�sung ab, die \ <TeXmacs> vom X Server beim Start
    mitgeteilt bekommt.

    <item*|<code*|unit>>px/256. Diese L�ngeneinheit wird von <TeXmacs> intern
    f�r L�ngenberechnungen benutzt.
  </description>

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