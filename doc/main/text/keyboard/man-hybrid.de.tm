<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Hybridbefehle und <LaTeX>-Simulation>

  In <TeXmacs> k�nnen Sie <LaTeX>-Befehle direkt von der Tastatur eingeben.
  Das geht wie folgt: Sie tippen auf die \ <key|\\>-Taste, um in den
  <LaTeX>/<TeXmacs>-Befehl-Modus zu gelangen. Dann schreiben Sie den Befehl,
  den Sie eingeben m�chten. Danach zeigt die linke Fu�leiste in etwa
  folgendes an:

  <\verbatim>
    \ \ \ \ return: <em|Befehl>
  </verbatim>

  Wenn Sie <key|<key-return>> eingeben, wird Ihr Befehl ausgef�hrt.
  Beispielsweise k�nnen Sie mit <key|\\ f r a c <key-return>> einen Bruch
  erzeugen und werden anschlie�end zur Eingabe von Z�hler und Nenner �ber die
  Fu�zeile aufgefordert.

  Wenn der Befehl, den Sie eingegeben haben, keinem <LaTeX>-Befehl
  entspricht, der <TeXmacs> bekannt ist, dann wird gepr�ft, ob es sich um ein
  Makro, eine Funktion oder ein spezielles Layout/Hervorhebung (aus der
  Stil-Definitions-Datei/einer Paket-Datei) handelt. Ist dies der Fall, dann
  wird eine entsprechende <TeXmacs>-Befehlsumgebung mit den notwendigen
  Argumenten erzeugt. Andernfalls wird unterstellt, dass es sich um eine
  Umgebungsvariable handelt, deren Wert anschlie�end erfragt wird. \ Die
  <key|\\>-Taste ist einer der folgenden Tastenkombinationen �quivalent
  <kbd-ia|l>, <kbd-ia|e>, <kbd-ia|a>, <kbd-ia|#> oder <kbd-ia|v>.

  Um den Buchstaben <kbd|\\> (backslash) einzuf�gen, k�nnen Sie <kbd-symb|\\>
  benutzen.

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