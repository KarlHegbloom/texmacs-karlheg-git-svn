<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Anpassung der globalen Darstellung>

  In der <menu|Codeformatierung>-Gruppe des <menu|Dokument|Ansicht>-Men�s
  finden Sie verschiedene M�glichkeiten die Darstellung von Quellcode-B�umen
  in Ihrem Dokument an Ihre Bed�rfnisse azupassen. Wir empfehlen, da� Sie mit
  den verschiedenen M�glichkeiten an einem eigenen Dokument zu
  experimentieren, um die Vor- und Nachteile kennen zu lernen, nachdem Sie
  den Quellmodus mit <menu|Document|View|Source> eingestellt haben.

  Zuerst einmal k�nnen Sie zwischen verschiedenen Basis-Stilen w�hlen:
  \RAngular'', \RScheme'', \RFunctional'' und \R<LaTeX>''. Die verschiedenen
  Darstellungsweisen werden in Graphiken unten beispielhaft gezeigt: \ 

  <\big-figure|<with|font-size|0.84|par-width|<times|0.55|<value|par-width>>|<tabular*|<tformat|<table|<row|<cell|<with|font-size|0.71|Angular>>|<cell|>|<cell|<with|font-size|0.71|Scheme>>>|<row|<\cell>
    <\with|src-style|angular>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-style|scheme>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|Functional>>|<cell|>|<cell|<with|font-size|0.71|<LaTeX>>>>|<row|<\cell>
    <\with|src-style|functional>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-style|latex>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>>>>>>
    Verschiedene Basis-Stile zur Darstellung von Quellcode-B�umen.
  </big-figure>

  M�glicherweise wollen Sie, da� bestimmte Quellcode-Konstrukte wie z.B.
  <markup|concat> und <markup|document> in besonderer Weise dargestellt
  werden. Im Men� <menu|Document|View|Special> k�nnen Sie festlegen, in
  welchem Ausma� Sie solche <translate|special|english|german> zulassen
  wollen:

  <\description>
    <item*|<menu|None>>Kein Quellcode-Konstrukt erh�lt eine Sonderbehandlung.

    <item*|<menu|Formatting>>Nur die Formatierungskonstrukte <markup|concat>
    and <markup|document> werden ausgef�hrt.

    <item*|<menu|Normal>>Zus�tzlich zu den oben genannten
    Formatierungskonstrukten werden werden einige andere Quellcode-Konstrukte
    wie <markup|compound>, <markup|value> und <markup|arg> \ ausgef�hrt.

    <item*|<menu|Maximal>>Diese Option ist noch nicht implementiert. Sie soll
    den Anwender in die Lage versetzen, spezielle Darstellungen von
    Konstrukten wie \ <markup|plus> zu programmieren.
  </description>

  Die verschiedenen Optionen sind unten dargestellt:

  <\big-figure|<with|font-size|0.84|par-width|<times|0.55|<value|par-width>>|<tabular*|<tformat|<cwith|2|2|3|3|cell-valign|t>|<table|<row|<cell|<with|font-size|0.71|<menu|None>>>|<cell|>|<cell|<with|font-size|0.71|<item*|<menu|Formatting>>>>>|<row|<\cell>
    <\with|src-special|raw>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-special|format>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|<menu|Normal>>>|<cell|>|<cell|<with|font-size|0.71|<menu|Maximal>>>>|<row|<\cell>
    <\with|src-special|normal>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-special|maximal>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>>>>>>
    Verschiedene Optionen zur Darstellung von Quellcodekonstrukten.
  </big-figure>

  Dar�ber hinaus kann der Anwender noch kontrollieren, wie verdichtet die
  Darstellung von Quellcode-Konstrukten sein soll, wie stark also Konstrukte
  durch Zeilenumbr�che gegliedert werden sollen. Das Ausma� kann im Men�
  <menu|Document|View|Compactification> eingestellt werden:

  <\description>
    <item*|<menu|Minimal>>Alle Konstrukte werden durch Zeilenumbr�che
    gegliedert.

    <item*|<menu|Only inline tags>>Alle Konstrukte au�er <translate|inline
    tags|english|german> werden durch Zeilenumbr�che gegliedert.

    <item*|<menu|Normal>>Alle Zeilen-Argumente am Anfang des Konstrukts
    werden verdichtet dargestellt. Wenn ein \ Block-Argument angetroffen
    wird, wird der Rest der Argumente durch Zeilenumbr�che gegliedert.

    <item*|<menu|Inline arguments>>Alle Zeilen-Argumente werden verdichtet
    dargestellt. Nur \ Block-Konstrukte werden durch Zeilenumbr�che
    gegliedert.

    <item*|<menu|Maximal>>Der ganze Quellcode wird verdichtet dargestellt.
  </description>

  Die Optionen <menu|Normal> und <menu|Inline arguments> unterscheiden sich
  nur unwesentlich. Beispiele f�r den Effekt der verschiedenen Optionen sind
  unten zu sehen:

  <\big-figure|<with|font-size|0.84|par-width|<times|0.55|<value|par-width>>|<tabular*|<tformat|<cwith|2|2|3|3|cell-valign|t>|<cwith|5|5|3|3|cell-valign|t>|<table|<row|<cell|<with|font-size|0.71|<menu|Minimal>>>|<cell|>|<cell|<with|font-size|0.71|<menu|Only
  inline tags>>>>|<row|<\cell>
    <\with|src-compact|none>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-compact|inline>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|<menu|Normal>>>|<cell|>|<cell|<with|font-size|0.71|<menu|Maximal>>>>|<row|<\cell>
    <\with|src-compact|normal>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-compact|all>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>>>>>>
    Die verschiedenen Optionen f�r den Verdichtungsgrad.
  </big-figure>

  Schlie�lich kann der Anwender im Men� <menu|Document|View|Closing style>
  die Darstellung der Stoptags von Quellcode-Befehlen einstellen, wenn sie
  mehrzeilig gegliedert dargestellt werden. Die folgenden Optionen sind
  verf�gbar: <translate|minimal|english|german>,
  <translate|compact|english|german>, <translate|stretched|english|german>
  und <translate|repeat|english|german>. Diese Optionen werden unten in
  Beispielen gezeigt:

  <\big-figure|<with|font-size|0.84|par-width|<times|0.55|<value|par-width>>|<tabular*|<tformat|<cwith|2|2|3|3|cell-valign|t>|<cwith|5|5|3|3|cell-valign|t>|<table|<row|<cell|<with|font-size|0.71|<menu|Minimal>>>|<cell|>|<cell|<with|font-size|0.71|<menu|Compact>>>>|<row|<\cell>
    <\with|src-close|minimal>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-close|compact>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|<with|font-size|0.71|<menu|Stretched>>>|<cell|>|<cell|<with|font-size|0.71|<menu|Repeat>>>>|<row|<\cell>
    <\with|src-close|long>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>|<cell|>|<\cell>
    <\with|src-close|repeat>
      <\framed-fragment*>
        <inactive*|<assign|quick-theorem|<\macro|body>
          <surround|<no-indent>Theorem. ||<arg|body>>
        </macro>>>
      </framed-fragment*>
    </with>
  </cell>>>>>>>
    Die verschiedenen Optionen zur Darstellung von Stoptags.
  </big-figure>

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
    <associate|src-style|angular>
  </collection>
</initial>