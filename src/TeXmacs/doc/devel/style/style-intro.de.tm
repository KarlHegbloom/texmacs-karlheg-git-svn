<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs> Stil-Definitionen>

  Eine der grundlegenden St�rken von <TeXmacs> ist, dass Sie Ihre eigenen
  Stile definieren und vorhandene freiz�gig anpassen k�nnen. Dazu k�nnen Sie
  Stil-Definitions-Dateien und Stil-Pakete schreiben. Diese erf�llen
  gleichzeitig mehrere Aufgaben:

  <\itemize>
    <item>Sie dienen zur abstrakten Definition von repetitiven Elementen in
    Texten wie z.B. Abschnitte, Nummerierungen usw..

    <item>Sie bieten Mechanismen zur Text-Strukturierung. Man kann z.B. ein
    Textst�ck als Abk�rzung, als Zitat oder als \Rwichtig'' definieren.\ 

    <item>Mit den Standard-Basis-Stilen k�nnen Sie professionell gestaltete
    Dokumente schreiben, denn die zugeh�rigen Stil-Definitionen wurden mit
    gro�er Sorgfalt von Leuten geschrieben, die viel von Typographie und
    �sthetik verstehen.
  </itemize>

  Jedes Dokument kann mit mehreren Stilen assoziiert werden, die sowohl
  Standard-Stile sein k�nnen als auch vom Anwender selbst definiert sein
  k�nnen. Der Haupt-Stil, der Basis-Stil, wird im Men� <menu|Document|Style>
  ausgew�hlt. Er entspricht in der Regel dem Dokument, das Sie schreiben
  wollen: Brief, Buch, Ver�ffentlichung usw. oder eine bestimmende
  Layout-Politik, wie Sie z.B. viele Verlage vorgeben. Dazu k�nnen weitere
  Stil-Pakete hinzugef�gt werden, die im Men� <menu|Document|Use package>
  auszuw�hlen sind. Diese Stil-Pakete modifizieren den Basis-Stil. Z.B. dient
  das Paket <tmpackage|number-europe> Paket dazu, die europ�ische Art der
  Nummerierung von Abbildungen, Beweisen usw. einzuf�hren, bei jeder einzelne
  Typ auch einzeln gez�hlt wird. Das Paket <tmpackage|maxima> enth�lt Makros,
  um die Ausgabe des Computer Algebra Systems <name|Maxima> ansprechend zu
  formatieren, wenn man <TeXmacs> als Oberfl�che daf�r benutzt. Mehrere
  Pakete k�nnen gleichzeitig verwendet werden.

  Wenn Sie eigenes Layout schreiben wollen oder wenn Sie vorhandenes Layout
  Ihren Bed�rfnissen anpassen wollen, dann m�ssen Sie sich entscheiden, ob
  Sie einen vollst�ndig neuen Basis-Stil schreiben wollen oder ein
  Stil-Paket. In den meisten F�llen werden Sie m�glicherweise es vorziehen,
  ein Stil-Paket zu schreiben, denn dann k�nnen Sie dieses mit beliebigen
  anderen Paketen kombinieren. In einigen F�llen ist es vorteilhafter einen
  neuen Basis-Stil zu schaffen, meist, indem Sie einen vorhandenen ver�ndern.
  Das ist haupts�chlich dann der Fall, wenn man die Layout-Politik eine
  bestimmten Zeitschrift nach�ffen will.

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