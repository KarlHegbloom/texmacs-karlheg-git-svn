<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Kontexte f�r bewegliche Objekte nutzen>

  Die <tmdtd|env-float> <abbr|D.T.D.> erzeugt die folgenden Kontexte f�r
  bewegliche Objekte:

  <\explain|<explain-macro|small-figure|body|caption>>
    Dieses Makro erzeugt ein Zeilen-Objekt mit Abbildung <src-arg|body> und
    der Beschriftung <src-arg|caption>. Zeilen-Abbildungen k�nnen z.B.
    benutzt werden, um mehrere kleinere Bilder nebeneinander innerhalb eines
    beweglichen Objekts zu setzen.
  </explain>

  <\explain|<explain-macro|big-figure|body|caption>>
    Dieses Makro erzeugt eine gro�e Abbildung, die sich �ber die ganze Breite
    eines Absatzes erstrecken kann, mit der eigentlichen Abbildung
    <src-arg|body> und der Beschriftung <src-arg|caption>.
  </explain>

  <\explain|<explain-macro|small-table|body|caption>>
    �hnlich <markup|small-figure>, aber f�r <em|kleine Tabellen>.
  </explain>

  <\explain|<explain-macro|big-table|body|caption>>
    �hnlich <markup|big-figure>, aber f�r <em|gro�e Tabellen>.
  </explain>

  <\explain|<explain-macro|footnote|body>>
    Erzeugt eine Fu�note.
  </explain>

  Die Abbildungs-�hnlichen Kontexte haben auch unnummerierte Varianten
  <markup|small-figure*>, <markup|big-figure*>, <abbr|usw.>, zu denen man mit
  dem Kurzbefehl <key|A-*> wechseln kann und umgekehrt.\ 

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