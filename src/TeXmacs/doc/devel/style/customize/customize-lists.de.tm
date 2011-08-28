<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Listenkontexte anpassen>

  Listen bestehen aus zwei verschiedenen Bestandteilen: der �u�eren
  Listenstruktur z.B. R�nder und der inneren Struktur, den einzelnen Punkten.
  Die Listenkontexte k�nnen durch Um- oder Neudefinition der
  Darstellungsmakros oder durch Definition zus�tzlicher Makros, die zur
  gleichen abstrakten Schnittstelle (D.T.D.) passen, ge�ndert werden.

  Die Darstellung der �u�eren Listenstruktur wird durch das
  <markup|render-list>-Makro gesteuert, das den Rumpf der Liste als Argument
  �bernimmt. Betrachten Sie die folgenden Umdefinition von
  <markup|render-list>:

  <\tm-fragment>
    <inactive*|<assign|render-list|<macro|body|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>>>
  </tm-fragment>

  Diese Umdefinition ver�ndert die Darstellung aller Listen
  (<translate|itemize|english|german>, <translate|enumerate|english|german>,
  usw.), indem der rechte Rand um <verbatim|3fn> reduziert wird:

  <\with|render-list|<macro|body|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>
    <\itemize>
      <item>Dieser Text, der zu lang ist, um auf eine einzelne Zeile zu
      passen, wird auf der rechten Seite um <verbatim|3fn> eingezogen.

      <\enumerate>
        <item>Dieser Text wird zus�tzlich um <verbatim|3fn> eingezogen, weil
        er sich in einer Unterliste befindet.
      </enumerate>

      <item>Und wiederum: Dieser Text, der zu lang ist, um auf eine einzelne
      Zeile zu passen, wird auf der rechten Seite um <verbatim|3fn>
      eingezogen.
    </itemize>
  </with>

  In �hnlicher Weise kann man die innere Listenstruktur, die einzelnen
  Punkte, konfigurieren, indem man die Makros <markup|aligned-item> und
  <markup|compact-item> benutzt. Beide Makros haben ein Argument, das die
  Kennzeichnung (kennzeichnenden Text) des Listenpunktes �bergibt.
  <markup|aligned-item> f�r \Rausgerichtete'' Punkte, setzt an einer festen
  Stelle in Bezug auf den Seitenrand die Trennmarke, stellt die Kennzeichnung
  rechtsb�ndig von der Trennmarke dar. Dadurch ist der Platz f�r die
  Kennzeichnung beschr�nkt. Dann positioniert es den Text rechts von der
  Trennmarke. Dagegen positioniert <markup|compact-item> die Kennzeichnung
  linksb�ndig an den Seitenrand, dann die Trennmarke und f�gt den Textinhalt
  daran rechts anschlie�end ein. So sind lange Kennzeichnungen m�glich. Die
  folgende Umdefinition von <markup|aligned-item>

  <\tm-fragment>
    <inactive*|<assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>>>
  </tm-fragment>

  stellt die Kennzeichnung aller Listen-Kontexte mit Punkten des
  \Rkompakten`` Typs rot dar:

  <\with|aligned-item|<macro|x|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>
    <\itemize>
      <item>Diese Liste und alle Listen mit \Rausgerichteten`` Punkten haben
      rote Kennzeichnung.

      <\description-aligned>
        <item*|C1>Erste Bedingung.

        <item*|C2>Zweite Bedingung.
      </description-aligned>

      <item>Die folgenden Punkte mit \Rkompakten`` Punkten benutzen
      <markup|compact-item> und bleiben unver�ndert.

      <\description-compact>
        <item*|Pferde und Hunde>Liebe Tiere.

        <item*|M�cken und Fliegen>Nicht so nett.
      </description-compact>
    </itemize>
  </with>

  <\remark>
    Die Makros <markup|aligned-item> und <markup|compact-item> m�ssen
    Zeileninhalt produzieren, damit man sie benutzen kann, um damit
    Blockinhalte zu umgeben. Eine Reihe von anderen internen Makros
    (<markup|aligned-space-item>, <markup|long-compact-strong-dot-item>,
    <abbr|usw.>) basieren auf <markup|aligned-item> und
    <markup|compact-item>, und werden f�r eine gro�e Zahl verschiedener Arten
    von Listen benutzt (<markup|itemize-arrow>, <markup|description-long>,
    <abbr|<localize|etc.>>). F�r die Zukunft planen wir, <markup|item> und
    <markup|item*> mit einem notwendigen Rumpfargument, <src-arg|body>, zu
    versehen. Man sollte das ber�cksichtigen, wenn man Listen-Kontexte
    entwirft, um den Code aufw�rts-kompatibel zu halten.
  </remark>

  Die <tmdtd|std-list> <abbr|D.T.D.> stellt ein Makro <markup|new-list>
  bereit, mit dem neue Listen definiert werden k�nnen. Seine Syntax ist
  <explain-macro|new-list|name|item-render|item-transform>. <src-arg|name>
  ist der Name des neuen Listen-Konstrukts, \ <src-arg|item-render> ein
  (Zeilen)-Makro zur Darstellung und <src-arg|item-transform> eine
  zus�tzliche Transformation, die auf den zu diesem Punkt geh�rigen Text
  angewendet wird. So kann man z.B. einen Kontext <markup|enumerate-roman>,
  wie folgt, definieren:

  <\tm-fragment>
    <\inactive*>
      <new-list|enumerate-roman|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>
    </inactive*>
  </tm-fragment>

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