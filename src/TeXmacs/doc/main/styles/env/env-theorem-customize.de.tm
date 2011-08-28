<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Nummerierte Kontexte anpassen>

  Die folgenden Makros dienen zur Darstellung von Text-Kontexten. Sie k�nnen
  alle umdefiniert werden, um die Darstellung speziellen Bed�rfnissen
  anzupassen.

  <\explain|<explain-macro|render-theorem|name|body>>
    Dieses Makro dient zur Darstellung von nummerierten, Theorem-artigen
    Kontexten. Das erste Argument Name <src-arg|name> gibt den Namen des
    \RTheorem`` (den kennzeichnenden Text), z.B. \RTheorem 1.2'' und das
    zweite Argument enth�lt den Rumpf. Dieser Kontext wird in Konstrukten
    gebraucht, die mit <markup|new-theorem> definiert wurden.
  </explain>

  <\explain|<explain-macro|render-remark|name|body>>
    �hnlich <markup|render-theorem>, aber f�r <localize|remark>s-artige
    Kontexte.
  </explain>

  <\explain|<explain-macro|render-exercise|name|body>>
    �hnlich <markup|render-theorem>, aber f�r <localize|exercise>-artige
    Kontexte.
  </explain>

  <\explain|<explain-macro|render-proof|name|body>>
    �hnlich <markup|render-theorem>, aber f�r <localize|proof>. Dies wird
    haupts�chlich dazu benutzt, den Namen von eines Beweises anzupassen, z.B.
    wie in \REnde des Beweise von Satz 1.2''.
  </explain>

  Beachten Sie, dass Sie diese Makros dazu benutzen k�nnen, einen Kontext zu
  erzeugen, der nur sich im Namen unterscheidet, z.B. anstelle von Satz
  Korollar.

  Die folgenden Befehle geben weitere M�glichkeiten zur
  Darstellungs-Anpassung:

  <\explain|<explain-macro|theorem-name|name>>
    Diese Makro kontrolliert die Darstellung von Namen in Theorem-artigen und
    Bemerkungs-artigen Kontexten. Die meisten Basis-Stile benutzen
    <strong|fett> oder <with|font-shape|small-caps|Kapit�lchen>.
  </explain>

  <\explain|<explain-macro|exercise-name|name>>
    �hnlich <markup|theorem-name>, aber f�r <localize|exercise>.
  </explain>

  <\explain|<explain-macro|theorem-sep>>
    Das Trennzeichen zwischen dem Namen in einem Theorem-artigem und
    Bemerkungs-artigem Kontext und dem Rumpf. Die Voreinstellung ist ein
    Punkt mit anschlie�endem Leerzeichen.\ 
  </explain>

  <\explain|<explain-macro|exercise-sep>>
    �hnlich <markup|theorem-sep>, aber f�r <localize|exercise>.
  </explain>

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