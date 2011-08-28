<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Maxima>

  <name|Maxima> ist nicht nur eines der �ltesten und besten Computer Algebra
  Systeme. Es ist auch ein wirkliches Universal-System mit einer freien
  Implementation. Zu erhalten von:\ 

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  Die unterst�tzte Version ist das <name|GCL>-basierte <name|Maxima> 5.6. F�r
  <name|CLisp>-basiertes <name|Maxima> 5.6, m�ssen Sie <verbatim|tm_maxima>
  editieren und <verbatim|-load> durch <verbatim|-i> ersetzen. F�r
  <name|Maxima> 5.9-pre muss <verbatim|-load> durch <verbatim|-p> ersetzt
  werden. Bekannte Probleme:

  <\itemize>
    <item>Wenn man <shortcut|(kbd-return)> dr�ckt und der Befehl ist nicht
    komplett (weil z.B. ; oder <verbatim|$> fehlt), dann h�ngt sich die
    Schnittstelle auf.\ 

    <item>Wenn man einen Lisp break Prompt (Fehlerausgabe) erzeugt, h�ngt
    sich die Schnittstelle auf.

    <item>Das Kommando <verbatim|info> wird nicht unterst�tzt, da es in dem
    darunter liegenden Lisp implementiert ist und nicht Maxima selbst. Daher
    ist die Portierung schwierig.

    <item>Einige Befehle des Debuggers arbeiten korrekt, aber einige,
    einschlie�lich :c, arbeiten nicht und keiner wei� warum.

    <item>Der <verbatim|load>-Befehl benimmt sich manchmal eigenartig.
  </itemize>

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