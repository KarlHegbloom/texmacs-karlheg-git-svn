<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Konvertierung von <LaTeX> zu <TeXmacs>>

  Das momentane Ziel des Konvertierungsprogrammes von <LaTeX> zu <TeXmacs>
  ist es, bei der �bersetzung von alten Dokumenten zu <TeXmacs> zu
  <em|helfen>. In der Regel sind Konvertierungen von <LaTeX> zu <TeXmacs>
  wesentlich problematischer als umgekehrt, wenn Sie sich aber daran halten,
  nur die gebr�uchlichen <LaTeX>-Kommandos in Ihren Dokumenten zu benutzen,
  werden Sie Ihre alten Dokumente zufriedenstellend konvertieren k�nnen.
  Beispielsweise wurden alle <TeXmacs>-Hilfedateien in <LaTeX> geschrieben,
  um die Funktion des <LaTeX> zu <TeXmacs>-Konvertierungsprogrammes zu
  �berpr�fen.

  Sie k�nnen ein <LaTeX>-Dokument <kbd|name.tex> �ber
  <menu|File|Import|Latex> in <TeXmacs> importieren und es dann unter
  <kbd|name.tm> speichern. Wenn Ihr <LaTeX>-Dokument einigerma�en ordentlich
  geschrieben wurde, ist das Ergebnis der Konvertierung mehr oder weniger
  akzeptabel, abgesehen von manchen Kommandos die nicht erkannt wurden.
  Solche werden dann in roter Schrift dargestellt. Eine m�gliche L�sung ist
  es, selbst eine Style-Datei f�r konvertierte Dokumente zu schreiben, die
  auf dem originalen Style basiert und in der die unbekannten Kommandos
  definiert sind.

  In weniger gl�cklichen F�llen k�nnte das konvertierte Dokument wie ein
  gro�er M�llhaufen aussehen. Die Ursache darin liegt dann wahrscheinlich in
  M�glichkeit des Anwenders, den Parser dynamisch zu modifizieren,
  beispielsweise mit dem <kbd|<verbatim|\\catcode>>-Kommando. Damit bringen
  Sie das Konvertierungsprogramm durcheinander und es interpetiert den Modus
  oder die Umgebung nicht richtig. Im Ergebnis kann dann normaler Text als
  Mathematik erscheinen, Mathematik als Verbatim und �hnliches. Trotzdem
  k�nnen die Kommandos in der Quell-Datei <kbd|name.tex>, die das
  Konvertierungsprogramm verwirren, relativ leicht durch Vergleichen der
  <LaTeX>-Quelle mit dem <TeXmacs>-Ergebnis ermittelt werden. Wenn Sie in der
  Quelldatei entsprechend suchen, werden Sie die irref�hrenden Codezeilen
  entfernen k�nnen und das Dokument wird sich schlie�lich annehmbar
  konvertieren lassen.

  In naher Zukunft m�chten wir das Konvertierungsprogramm mit einem Filter
  f�r die Style-Dateien sowie weiteren Features, welche die �bersetung von
  selbstdefinierten Kommandos mit deren Definition aus einer dritten Datei
  erm�glichen, erweitern.

  <tmdoc-copyright|1998-2004|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|german>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
  </collection>
</references>