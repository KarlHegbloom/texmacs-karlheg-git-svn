<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Struktur-Layout der <TeXmacs> Dokumentation>

  Als grunds�tzliche Regel gilt, da� sie keine Unterstrukturierungsbefehle
  innerhalb der <TeXmacs> Dokumentation benutzen sollten. Versuchen sie statt
  dessen lieber kleine Hilfe-Seiten zu schreiben, die auf gut abgegrenzte
  kleine Themen zugeschnitten sind. Auf einer �bergeordneten Ebene sollten
  sie rekursive "Meta-Hilfe-Dateien" schreiben, die anzeigen, wie man die
  Dokumentation auf automatische Weise durchqueren soll. Dies erlaubt die
  Wiederverwendung einer Hilfe-Seite f�r vielfache Zwecke (eine gedruckte
  Anleitung, ein Tutorial im Internet, etc.).

  Der <tmstyle|tmdoc> Stil enth�lt drei Markup-Makros, mit denen man anzeigen
  kann, wie die Dokumentation zu durchlaufen ist. Das <markup|traverse> Makro
  wird benutzt um Bereiche mit Durchlaufsinformationen zusammenzufassen. Das
  <markup|branch> Makro indiziert eine Hilfe-Seite als Unterabschnitt und das
  <markup|continue> Makro eine Folge-Seite. Beide, das <markup|branch> und
  das <markup|continue> Makro haben zwei Argumente. Das erste Argument
  beschreibt den Link und das zweite Argument enth�lt schlie�lich die
  Pfadadresse der verkn�pften Datei.

  Typischerweise werden sie am Ende einer Meta-Hilfs-Datei mehrere
  <markup|branch> oder <markup|continue> Makros finden, innerhalb eines
  <markup|traverse> Makros. Am Anfang des Dokuments sollten sie au�erdem
  einen Titel f�r das Dokument definieren, dies gesschieht mit Hilfe des
  <markup|tmdoc-title> Makros. Wenn man dann eine gedruckte Version der
  Anleitung erstellt, kann man automatisch eine
  Kapitel/Abschitt/Unterabschnitt-Struktur mit dieser Information erzeugen.
  Alternativ k�nnte man auch zus�tzliche Kn�pfe f�r eine Navigation mittels
  eines Browsers innerhalb der Dokumentation erzeugen.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
    <associate|language|german>
  </collection>
</initial>
