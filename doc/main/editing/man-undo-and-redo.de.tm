<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|�nderungen zur�cknehmen oder wiederholen>

  Man kann die �nderungen, die man in einem Dokument vorgenommen hat,
  schrittweise zur�cknehmen und zwar von dem Moment an, an dem Sie <TeXmacs>
  gestartet haben. Man erreicht das mit <menu|Edit|Undo> bzw. <kbd-gen|[>
  oder <key|C-_>. Nach dem Zur�cksetzen kann man mit Zur�ckkehren in den
  urspr�nglichen Zustand wieder zur�ck: \ <menu|Edit|Redo> oder <kbd-gen|]>.

  Die Anzahl gespeicherter �nderungen ist standardm��ig auf 100 beschr�nkt.
  Man kann die Zahl aber in der pers�nlichen Initialisierungs-Datei z.B. auf
  1000 erh�hen, indem man die folgende Zeile einf�gt:

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  Wenn Sie eine negative Zahl angeben, gibt es kein obere Schranke au�er
  Ihrem Speicher.

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