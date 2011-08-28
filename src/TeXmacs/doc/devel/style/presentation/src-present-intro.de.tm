<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|ASCII-basierte oder Baum-basierte Editierung: problematische
  Alternativen>

  Die meisten Nutzer sind daran gew�hnt, Quellcode mit einem konventionellen
  Editor wie <name|Emacs> zu editieren, die den Quellcode im ASCII-Format
  darstellen. <TeXmacs>-Dokumente werden aber als
  <hyper-link|B�ume|../../format/basics/basics.de.tm> (tree) gespeichert. Es
  ist daher eine interessante aber sehr komplizierte Frage, welches Format
  f�r die Editierung am besten geeignet ist. Eine Option w�re, ein
  ASCII-basiertes Format wie \ XML oder Scheme zu verwenden oder auch das
  ASCII-basierte Format, in dem Dateien auf einer Diskette oder Festplatte
  gespeichert werden. Die andere Option besteht darin die B�ume als solche zu
  verwenden und damit Textdokumente und Quellcode gleich zu behandeln.\ 

  Wir haben uns entschlossen in <TeXmacs> die zweite Alternative zu
  verwenden. Genauer gesagt, jedes Dokument kann im \RQuellmodus'' editiert
  werden. Das ist nur eine spezielle Form der Darstellung, die die
  Baumstruktur besser erkennbar macht. Es ist sehr instruktiv, irgendein
  Dokument zu nehmen und es im Quellmodus zu betrachten. Dazu w�hlen Sie
  \ <menu|Dokument|Ansicht|Quellmodus>. Auf diese Weise k�nnen Sie vorallem
  bei Verweisen jeder Art das Ziel erkennen.

  Die Wahl zwischen ASCII-basiertem Editieren und Baum-basierten Editieren
  ist nicht trivial, denn <TeXmacs> Stil-Dateien und Pakete haben eine
  Doppelnatur. Sie sind einerseits Programme, die spezifizieren, wie Makros
  dargestellt werden, andererseits enhalten sie normalen Text. Es gibt eine
  Reihe von Gr�nden, warum Nutzer ein ASCII-basiertes Format vorziehen:

  <\enumerate>
    <item>Der Code kann leicht so formatiert werden, da� man ihn besser lesen
    kann.

    <item>Kommentare k�nnen leicht hinzugef�gt werden.

    <item>Standard Editoren wie <name|Emacs> stellen Werkzeuge f�r
    automatische Hervorhebungen, Einz�ge usw. bereit.

    <item><label|structure-constraints>Man wird nicht durch irgendwelche
    \RStrukturen'' in der Entwicklungsphase behindert.
  </enumerate>

  Wir versuchen m�glichst viele dieser Vorteile in unser Konzept einer a
  strukturierten Dokument- Darstellung einflie�en zu lassen, obwohl es
  offensichtlich schwierig ist, Punkt 4 angemessen zu ber�cksichtigen. Wir
  glauben, da� die in ersten drei Punkten genannten Vorteile in einer solchen
  strukturierten Umgebungen sogar deutlicher ausgepr�gt sein werden. Jedoch
  erfordert die Verwirklichung dieses Konzeptes ein tiefes Verst�ndnis davon,
  wie Nutzer Quellcode tats�chlich formatieren und editieren.\ 

  Lassen Sie uns beispielsweise dieses St�ck formatierten Code betrachten:

  <\cpp-fragment>
    if (cond) hop \ \ = 2;

    else \ \ \ \ \ holala= 3;
  </cpp-fragment>

  Man erkennt, da� dieser Code mit in spezieller Weise formatiert ist. Die
  Formatierungsrichtlinien, die derjenige, der den Code formatierte, im Kopf
  hatte sind, im Code nicht enthalten. Wenn etwa die Variable cond in c
  umbenannt wird, oder wenn die Variable holala in hola umbenannt wird, mu�
  der Code manuell neu formatiert werden.

  Zum jetzigen Zeitpunkt stellt <TeXmacs> keine Werkzeuge bereit, die das
  Problem dieses \ Beispiels automatisch l�sen k�nnten, aber einige wichtige
  Werkzeuge sind schon vorhanden. So hat der Anwender viele M�glichkeiten,
  Quellcode durch Einz�ge zu gestalten und vern�nftige Standardformatierungen
  sind vorhanden. Weitere Werkzeuge sollen in Zukunft entwickelt werden. Wir
  sind f�r Anregungen dankbar.\ 

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
    <associate|preamble|false>
  </collection>
</initial>