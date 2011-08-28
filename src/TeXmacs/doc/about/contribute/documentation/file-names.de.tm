<TeXmacs|1.0.0.7>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Konventionen f�r die Dateibenennung>

  Die meiste Dokumentation sollten als ein Unterpunkt zum Thema des
  Verzeichnisbaum organisiert werden. Die Unterverzeichnisse des
  Hauptverzeichnisses sind die folgenden:

  <\description>
    <expand|item*|devel>Dokumentation f�r Enwickler.

    <expand|item*|examples>Beispiele f�r <TeXmacs> Dokumente.

    <expand|item*|incoming>Neue Dokumentation, die noch etwas unausgereigt
    ist.

    <expand|item*|main>Der Hauptzweig der Dokumentation.

    <expand|item*|meta>Wie man Dokumentation schreibt und kompiliert.
  </description>

  Bitte versuchen sie Zahl der Eintr�ge pro Verzeichnis auf einem
  vern�nftigen kleinen Ma� zu halten.

  Dateinamen im Hauptverzeichnis sollten von der Form sein:
  <verbatim|typ-name.sprache.tm>. In anderen verzeichnissen sind sie von der
  Form: <verbatim|name.sprache.tm>. Hierbei ist <verbatim|typ> eine
  Hauptbezeichnung f�r den Typ der Dokumentation; es sollte eins der folgenen
  sein:

  <\description>
    <expand|item*|adv>Dokumentation f�r fortgeschrittene Anwender.

    <expand|item*|man>Zur Einf�gung in das <TeXmacs> manual.\ 

    <expand|item*|tut>Zur Einf�gung in das <TeXmacs>-Tutorial.
  </description>

  Sie sollten versuchen, Dokumentation �ber dasselbe Theme zusammen zuhalten,
  unabh�ngig vom Typ. Dies erlaubt ihnen bereits existierende Dokumentation
  �ber ein bestimmtes Gebiet einfacher zu finden. Auch mag es passieren, da�
  sie Dokumentation im manual einf�gen wollen, die eigentlich f�r das
  Tutorial gedacht war. Die <verbatim|Sprache> in der die Dokumentation
  geschrieben wurde, sollte mit dem 2-Buchstaben Code gekennzeichnet werden
  (wie <verbatim|en>, <verbatim|fr>, usw.). Die <verbatim|Hauptbezeichung>
  ihrer Datei sollte aber dieselbe bleiben wie in anderen �bersetzungen. Zum
  Beispiel sollte <verbatim|man-keyboard.en.tm> nicht in
  <verbatim|man-clavier.fr.tm> �bersetzt werden.

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
