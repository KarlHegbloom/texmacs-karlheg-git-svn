<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Dynamische Objekte>

  Bestimmte etwas komplexere Objekte k�nnen verschiedene Zust�nde w�hrend der
  Editiervorg�nge einnehmen. Beispiele f�r <em|dynamischen Objekte> sind
  Textmarken und Referenzen, weil ihre Darstellung von Zahlen abh�ngt, die
  sich laufend �ndern k�nnen. Weitere Beispiele daf�r finden sich
  <hlink|hier|../../../devel/style/style.de.tm>.\ 

  Wenn man ein dynamisches Objekt, wie z.B. eine Textmarke (label) mit
  <key|M-!> einf�gt, ist der Vorgabe-Zustand \R<em|inaktiv>''. In diesem
  inaktiven Zustand k�nnen notwendige Parameter eingegeben werden, wie in
  unserem Fall die kennzeichnende Zeichenkette. Manche dynamischen Objekte
  k�nnen eine beliebige Anzahl Parameter aufnehmen. In diesem Fall k�nnen
  neue <key|Tab>-Taste eingef�gt werden.

  <\big-figure>
    <with|color|blue|<with|mode|math|\<langle\>>label<with|mode|math|\|>>pythagoras<with|color|blue|<with|mode|math|\<rangle\>>>
  </big-figure|Inaktive Textmarke>

  Wenn alle relevanten Informationen in das inaktive dynamische Objekt
  eingegeben sind, dann k�nnen sie es mit Wagenr�cklauf-Taste
  <shortcut|(kbd-return)> <em|aktivieren>. Ein aktives dynamisches Objekt kann
  deaktiviert werden, indem man den Cursor unmittelbar dahinter positioniert
  und dann die R�cktaste <key|backspace>bet�tigt.

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