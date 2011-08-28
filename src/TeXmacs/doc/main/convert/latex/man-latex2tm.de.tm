<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Konversion von <LaTeX> nach <TeXmacs>>

  Das der Konvertierung von <LaTeX> nach <TeXmacs>, ist es, <em|Ihnen zu
  helfen>, alte alte <LaTeX>-Dokumente nach <TeXmacs> zu �bertragen, damit
  sie f�r Ihre Arbeit nicht vollst�ndig verloren sind. Es ist nicht das Ziel,
  eine vollst�ndige �bertragung mit identischen Layout zu erreichen.

  Im allgemeinen ist die Umwandlung von \ <LaTeX> nach <TeXmacs> schwieriger
  als umgekehrt. Wenn Sie sich aber auf die gebr�uchlichsten \ <LaTeX>
  Kommandos beschr�nken, sollten Sie ihre Dokumente einigerma�en ordentlich
  �bertragen k�nnen. Beispielsweise wurden alle <TeXmacs>-Hilfe-Dateien in
  <LaTeX> geschrieben und dann nach <TeXmacs> konvertiert, um das
  Konversions-Programm zu testen.

  Sie k�nnen ein <LaTeX>-Dokument<verbatim|name.tex> mit dem Befehl
  <menu|File|Import|Latex> nach <TeXmacs> importieren und unter dem Namen
  <verbatim|name.tm> sichern. Wenn Ihr <LaTeX>-Dokument gut genug geschrieben
  wurde, dann sollte das Resultat der Konvertierung einigerma�en akzeptabel
  sein, von einigen unbekannten Befehlen abgesehen, die rot markiert sind.
  Ein gute L�sung daf�r, besteht darin, eine eigene Stil-Definition basierend
  auf dem Original Stil zu schreiben, in der die unbekannten Befehle
  definiert werden.

  Dennoch gibt es F�lle, in denen das ganze Dokument ein gro�es unlesbares
  Durcheinander ist. Das kommt meist davon, dass \ <TeX> und <LaTeX> es
  gestatten, dass der Parser w�hrend seiner Ausf�hrung dynamisch ver�ndert
  wird, indem Sie z.B. den <verbatim|\\catcode> Befehl verwenden. In solchen
  F�llen kann das Konvertierungs-Programm manchmal den Intentionen nicht
  folgen und macht unzutreffende Annahmen. Dann wird z.B. Text in
  mathematische Formeln umgewandelt, mathematischer Code in w�rtlichen Text
  usw.. Meist lassen sich die problematischen Befehle in <verbatim|name.tex>
  aber leicht herausfinden, wenn man \ <LaTeX> mit der <TeXmacs>-Version
  vergleicht. Dann kann man meist den problematischen <LaTeX>-Code ersetzen
  und so eine einigerma�en akzeptable Konvertierung erreichen.

  Wir planen auch eine Konverter f�r \ <TeX>-Stil-Dateien nach <TeXmacs>
  sowie einige andere Erweiterungen, die die Konversion von Anwender-Befehlen
  \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ zu erleichtern, die in einem
  anderen Dokument definiert sind, als dem, das Sie gerade konvertieren
  wollen.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
  </collection>
</initial>