<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Die Standard-<TeXmacs>-Stile>

  F�r jedes Dokument kann man einen Stil f�r ein Dokument im Men�
  <menu|Document|Style> ausw�hlen. Dieser Stil soll die wichtigsten Merkmale
  eines Dokuments bestimmen und hei�t deshalb Basis-Stil. Er entspricht im
  Allgemeinen der Art des Dokuments, das erstellt werden soll: ein Brief, ein
  Buch usw. oder einem bestimmten Layout, wie einem ein Artikel f�r ein
  bestimmtes Journal. Zus�tzlich zu einem Basis-Stil kann der Anwender ein
  oder mehrere Pakete im Men� <menu|Document|Add package>. Solche Pakete
  k�nnen den Basis-Stil modifizieren, zus�tzliche Hervorhebungen einf�hren,
  oder beides.

  In diesem Abschnitt werden wir einen �berblick �ber die
  Standard-Dokument-Stile geben, die von <TeXmacs> bereitgestellt werden. Die
  meisten Basis-Stile und Pakete haben eine abstrakte Schnittstelle, die
  <abbr|D.T.D.> (data domain definition), die festlegt, welche Makros von dem
  Stil oder dem Paket exportiert werden und wie sie zu benutzen sind.
  Unterschiedliche Stile oder Pakete wie z.B. Kopfzeile-Artikel,
  <tmpackage|header-article>, und Kopfzeile-Buch, <tmpackage|header-book>,
  k�nnen dieselbe <abbr|D.T.D.> benutzen aber in der Darstellung v�llig
  verschieden sein. Deshalb werden wir uns hier vorwiegend mit der
  Beschreibung der Standard-<abbr|D.T.D.>s besch�ftigen, au�er wenn wir uns
  auf die Darstellung konzentrieren. Man kann die Standard-Stile
  modifizieren, wenn man neue definiert, die der abstrakten Schnittstelle
  entsprechen (siehe auch den Abschnitt �ber <hyper-link|<TeXmacs>
  Stil-Definitionen|../../devel/style/style.de.tm>).

  <\traverse>
    <branch|Standard-<TeXmacs>-Basis-Stile|style-organize.de.tm>

    <branch|Die gemeinsame Basis der meisten Stile|std/std-dtd.de.tm>

    <branch|Standard-Kontexte|env/env-dtd.de.tm>

    <branch|Titel und Kopfzeilen|header/header-dtd.de.tm>

    <branch|Abschnitte|section/section-base-dtd.de.tm>
  </traverse>

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
    <associate|preamble|false>
  </collection>
</initial>