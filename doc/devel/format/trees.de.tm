<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Dokumente sind B�ume>

  <TeXmacs> repr�sentiert alle Texte durch B�ume (f�r einen bestimmten Text
  wird der zugeh�rige Baum der <def-index|Editierbaum> genannt). Die inneren
  Knoten eines solchen Baum werden mit Standard-<def-index|Operatoren> des
  Typs <verbatim|tree_label> (siehe <verbatim|Basic/Data/tree.gen.h>)
  aufgebaut. Der Inhalt der "Bl�tter" des Baums sind Zeichenketten (Strings),
  welche entweder unsichtbar (z. B. L�ngen oder Makro-Definitionen), oder
  sichtbar (der richtige Text) erscheinen. <TeXmacs> B�ume k�nnen auf
  verschiedene Weise beschrieben werden. Zum Beispiel repr�sentiert der Baum

  <\quote-env>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </quote-env>

  die Formel

  <\tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </tm-fragment>

  und kann in der Notation von <value|scheme> geschrieben werden als:

  <\scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </scheme-fragment>

  \;

  Die Bedeutung des Textes und die Art, wie er gesetzt wird, h�ngt essenziell
  von der aktuellen Umgebung ab. Die Umgebung besteht haupts�chlich aus einer
  Hash-Tabelle, welche die Umgebungsvariablen mit den Baum-Inhalten
  verkn�pft. Die aktuelle Sprache, aktuelle Schrift und aktuelle Farbe sind
  Beispiele f�r Systemumgebungs-Variablen; neue Variablen k�nnen durch den
  Nutzer definiert werden. Zum Beispiel erzeugt der <value|scheme> Ausdruck

  <\scheme-fragment>
    (concat

    \ \ "Ein "

    \ \ (with "color" "blue" "blauer")

    \ \ " Text.")
  </scheme-fragment>

  das entsprechende Textfragment

  <\tm-fragment>
    Ein <with|color|blue|blauer> Text.
  </tm-fragment>

  Das <TeXmacs>-Konstrukt <verbatim|with> beschreibt eine lokale �nderung der
  Umgebungsvariablen.

  Im nachfolgenden werden wir im Detail beschreiben, wie die verschiedenen
  Standard <TeXmacs> Operatoren und Umgebungsvariablen funktionieren. Es
  sollte erw�hnt werden, dass sich das <TeXmacs> Datenformat ein Punkt ist,
  an dem noch gearbeitet wird. Im letzten Abschnitt werden diese �nderungen
  beschrieben. F�r gew�hnlich wird der Anwender von einer Erweiterung des
  Datenformats nichts bemerken, da solch eine �nderung immer zusammen mit
  einem Konvertierungs-Programm entwickelt wird, das die bestehenden
  Dokumente automatisch auf das neue Format erg�nzt. Dennoch sind sie
  manchmal wichtig f�r die Entwickler, wenn auch die meisten �nderungen nur
  das Hinzuf�gen von neuen Konstrukten betreffen.

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