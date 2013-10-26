<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Quellcode editieren>

  Die verschiedenen Darstellungsm�glichkeiten von Quellcode-B�umen werden
  <hyper-link|hier|../../style/presentation/src-present-global.de.tm>
  detailliert geschildert. An dieser Stelle werden nur die zugeh�rigen
  Kontextvariablen beschrieben.

  <\explain>
    <var-val|src-style|angular><explain-synopsis|Darstellungsstil f�r
    Quellcode>
  <|explain>
    Der Basis-Stil, wie er im Men� <menu|Document|Source|Style> eingestellt
    werden kann. M�gliche Werte sind <verbatim|angular>
    (<translate|angular|english|german>), <verbatim|scheme>
    (<translate|scheme|english|german>), <verbatim|functional>,
    (<translate|functional|english|german>) und <verbatim|latex>
    (<translate|latex|english|german>).
  </explain>

  <\explain>
    <var-val|src-special|normal><explain-synopsis|Darstellung spezieller
    Konstrukte>
  <|explain>
    Wie spezielle Konstrukte wie <markup|concat>, <markup|document>,
    <markup|compound>, usw. dargestellt werden sollen. Diese kann im Men�
    <menu|Document|Source|Special> eingestellt werden. M�gliche Werte sind
    <verbatim|raw> (<translate|none|english|german>), <verbatim|format>
    (<translate|formatting|english|german>), <verbatim|normal> und
    <verbatim|maximal>.
  </explain>

  <\explain>
    <var-val|src-compact|normal><explain-synopsis|Verdichtungsgrad>
  <|explain>
    Wie verdichtet sollen Quellcode-Konstrukte dargestellt werden. Das kann
    im Men� <menu|Document|Source|Compactification> eingestellt werden.
    M�gliche Werte sind <verbatim|none> (Minimal), <verbatim|inline>
    (<translate|only inline tags|english|german>), <verbatim|normal>,
    <verbatim|inline tags> (<translate|inline arguments|english|german>) und
    <verbatim|all> (maximal).
  </explain>

  <\explain>
    <var-val|src-close|compact><explain-synopsis|Darstellung der Stoptags>
  <|explain>
    Die Darstellung von Stoptags in mehrzeiligen Befehlen kann im Men�
    <menu|Document|Source|Closing style> eingestellt werden. M�gliche Werte
    sind <verbatim|repeat> (<translate|repeat|english|german>),
    <verbatim|long> (<translate|stretched|english|german>),
    <verbatim|compact> (<translate|compact|english|german>) und
    <verbatim|minimal> (<translate|minimal|english|german>).
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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