<TeXmacs|1.0.4.5>

<style|<tuple|tmdoc|number-europe>>

<\body>
  <tmdoc-title|Nummerierte Text-Kontexte>

  <TeXmacs> verf�gt �ber drei verschiedene nummerierte Standard-Kontexte f�r
  Text: <translate|theorem|english|german>-�hnliche,
  <translate|remark|english|german>-�hnliche und
  <translate|exercise|english|german>-�hnliche. Die folgenden Aspekte k�nnen
  leicht angepasst werden:

  <\itemize>
    <item>Neue Konstrukte erzeugen.

    <item>Die Darstellung �ndern.

    <item>Die Nummerierung �ndern.
  </itemize>

  <paragraph|Neue Konstrukte erzeugen>

  Mit den Meta-Makros <markup|new-theorem>, <markup|new-remark> und
  <markup|new-exercise> k�nnen neue nummerierte Kontexte erzeugt werden. Sie
  alle haben zwei Argumente, den Namen des neuen Konstrukts und die
  Bezeichnung, die in der Darstellung auf dem Bildschirm verwendet werden
  soll. Wenn Sie beispielsweise einen Kontext f�r Experimente erzeugen
  wollen, der mit Experiment 1 usw. fortlaufend beschriftet wird, dann
  definieren Sie <markup|experiments> mit

  <\tm-fragment>
    <inactive*|<new-theorem|experiments|Experiment>>
  </tm-fragment>

  Wenn Experiment in dem geeigneten <TeXmacs>-W�rterbuch enthalten ist, wird
  der Text \RExperiment'' automatisch �bersetzt. Im Abschnitt
  <hyper-link|Definition neuer Kontexte|../../../main/styles/std-dtds/env-base-dtd.de.tm>
  wird u.a. beschrieben, wie man neue nummerierte Kontexte schreiben kann,
  die nicht zu den <translate|theorem|english|german>-�hnlichen,
  <translate|remark|english|german>-�hnlichen und
  <translate|exercise|english|german>-�hnlichen geh�ren.

  <paragraph|Die Darstellung �ndern>

  Die Darstellung dieser Kontexte kann mit den Konstrukten
  <markup|render-theorem>, <markup|render-remark> und
  <markup|render-exercise> beeinflusst werden. Diese Makros haben als
  Argumente den Namen des Kontexts (z.B. \Rtheorem 1.2'') und seinen Rumpf.
  Eine <translate|remark|english|german> wird gem�� Vorgabe genau wie ein
  <translate|theorem|english|german> gesetzt \ allerdings in der Schriftform
  \R<translate|upright|english|german>``. Daher beeinflussen �nderungen der
  Definition von <markup|render-theorem> auch die Darstellung einer
  <translate|remark|english|german>. Wenn man beipielsweise von einem
  <translate|theorem|english|german> verlangt, dass er etwas einger�ckt und
  <translate|slanted|english|german> dargestellt wird, dann kann man
  <markup|render-theorem> so umdefinieren:

  <\tm-fragment>
    <inactive*|<assign|render-theorem|<macro|which|body|<style-with|src-compact|none|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>>>
  </tm-fragment>

  Das f�hrt zu folgender Darstellung:

  <\with|render-theorem|<macro|which|body|<surround|<vspace*|1fn><no-indent><theorem-name|<arg|which><theorem-sep>>|<right-flush><vspace|1fn>|<with|font-shape|slanted|par-left|<plus|<value|par-left>|1.5fn>|<arg|body>>>>>
    <\theorem>
      Das ist ein <translate|theorem|english|german>, der
      <translate|slanted|english|german> und einger�ckt dargestellt wird.
    </theorem>

    <\remark>
      Die Darstellung einer <translate|remark|english|german> basiert auf der
      Darstellung des <translate|theorem|english|german>-Kontexts nur, dass
      die Schriftform \R<translate|upright|english|german>`` verwendet wird.
    </remark>
  </with>

  Manchmal m�chte man aber nur die Darstellung der Bezeichnung oder des
  Trennzeichens zwischen Bezeichnung und Text-Rumpf �ndern. Wie man im
  vorstehenden Beispiel erkennen kann, werden diese Aspekte durch die Makros
  <markup|theorem-name> und <markup|theorem-sep> gesteuert. Beispielsweise
  wird mit

  <\tm-fragment>
    <inactive*|<assign|theorem-name|<macro|name|<with|color|dark
    red|font-series|bold|<arg|name>>>>>

    <inactive*|<assign|theorem-sep|<macro|: >>>
  </tm-fragment>

  \ eine <translate|proposition|english|german>, wie folgt, dargestellt:\ 

  <\with|theorem-name|<macro|name|<with|color|dark
  red|font-series|bold|<arg|name>>>|theorem-sep|<macro|: >>
    <\proposition>
      Diese <translate|proposition|english|german> hat eine ungew�hnliche Art
      der Darstellung.
    </proposition>
  </with>

  <paragraph|Die Nummerierung �ndern>

  In den Abschnitten �ber <hyper-link|Z�hler und
  Z�hlergruppen|../../../main/styles/std/std-counter-dtd.de.tm> wird erkl�rt,
  wie man Z�hler zu einen bestimmten Zweck �ndern kann. Beispielsweise k�nnen
  Sie f�r den Kontext \R<localize|corollary>`` den Z�hler zur�cksetzen, indem
  Sie <markup|inc-theorem> neu definieren:

  <\tm-fragment>
    <inactive*|<style-with|src-compact|none|<quasi|<style-with|src-compact|none|<assign|inc-theorem|<macro|<compound|<unquote|<value|inc-theorem>>><reset-corollary>>>>>>>
  </tm-fragment>

  Beachten Sie den Trick mit <markup|quasi> und <markup|unquote>, um alle
  Aktionen zu ber�cksichtigen, die von den fr�heren Werten des Makros
  <markup|inc-theorem> stammen k�nnen.

  Der folgende Code von <verbatim|number-long-article.ts> dient dazu, allen
  Standard-Kontexten die Nummer des aktuellen Abschnitts als Praefix
  voranzustellen.

  <\tm-fragment>
    <inactive*|<assign|section-clean|<macro|<reset-subsection><reset-std-env>>>>

    <inactive*|<assign|display-std-env|<macro|nr|<section-prefix><arg|nr>>>>
  </tm-fragment>

  \;

  Beachten Sie auch, dass mit den Paketen <verbatim|number-europe.ts>,
  <verbatim|number-long-article.ts>, <verbatim|number-us.ts>,
  structured-list.ts (<localize|number-europe, number-long-article,
  number-us, structured-list und structured-section>) die Nummerierung im
  Men� <menu|View|Add package|Customize> angepasst werden kann.

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