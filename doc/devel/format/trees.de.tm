<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Dokumente sind Ba�me>

  <apply|TeXmacs> repr�sentiert alle Texte durch B�ume (f�r einen bestimmten
  Text wird der zugeh�rige Baum der <expand|def-index|Editionsbaum> genannt).
  Die inneren Knoten eines solchen Baum werden mit
  Standard-<expand|def-index|Operatoren> des Types <verbatim|tree_label>
  (siehe <verbatim|Basic/Data/tree.gen.h>) aufgebaut. Der Inhalt der
  "Bl�tter" des Baums sind Zeichenketten (Strings), welche entweder
  unsichtbar (z. B. L�ngen oder Makro-Definitionen), oder sichtbar (der
  richtige Text) erscheinen. <TeXmacs> B�ume k�nnen durch verschiedene
  Notationen beschrieben werden. Zum Beispiel, Baum

  <\expand|quote>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </expand>

  repr�sentiert die Formel

  <\expand|tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </expand>

  und kann auch geschrieben werden als

  <\expand|scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </expand>

  in der Notation von <value|scheme>.

  Die Bedeutung des Textes und die Art wie er gesetzt wird h�ngt essenziell
  von der aktuellen Umgebung ab. Die Umgebung besteht haupts�chlich aus einer
  Hash-Tabelle, welche die Umgebungsvariablen mit den Bauminhalten verkn�pft.
  Die aktuelle Sprache, aktuelle Schrift und aktuelle Farbe sind Beispiele
  f�r Systemumgebungsvariablen; neue Variablen k�nnen durch den Nutzer
  definiert werden. Zum Beispiel erzeugt der <value|scheme> Ausdruck

  <\expand|scheme-fragment>
    (concat

    \ \ "Ein "

    \ \ (with "color" "blue" "blauer")

    \ \ " Text.")
  </expand>

  das entsprechende Textfragment

  <\expand|tm-fragment>
    Ein <with|color|blue|blauer> Text.
  </expand>

  Der <TeXmacs>-Befehl <verbatim|with> beschreibt eine lokale �nderung der
  Umgebungsvariablen.

  Im nachfolgenden werden wir im Detail beschreiben, wie die verschiedenen
  Standard <apply|TeXmacs> Operatoren und Umgebungsvariablen funktionieren.
  Es sollte erw�hnt werden, da� sich das <apply|TeXmacs> Datenformat ein
  Punkt ist, an dem noch gearbeitet wird. Im letzten Abschnitt werden diese
  �nderungen beschrieben. F�r gew�hnlich wird der Anwender von einer
  Erweiterung des Datenformats nichts bemerken, da solch eine �nderung immer
  zusammen mit einem Konvertierungsprogramm entwickelt wird, das die
  bestehenden Dokumente automatisch auf das neue Format erg�nzt. Dennoch sind
  sie manchmal wichtig f�r die Entwickler, wenn auch die meisten �nderungen
  nur das Hinzuf�gen von neuen Grundbefehlen betreffen.

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|Editionsbaum>|<pageref|idx-1>>

      <tuple|<tuple|Operatoren>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
