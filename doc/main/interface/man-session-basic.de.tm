<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Sitzungen erzeugen>

  Eine Sitzung kann mit Befehlen aus dem Men� <menu|Text|Session> gestartet
  werden. Sie besteht aus einer Folge von Eingabe- und Ausgabe-Feldern. Wenn
  man <key|<key-return>> innerhalb eines Eingabefeldes benutzt, dann wird der
  Text innerhalb dieses Feldes von dem externen Programm evaluiert und in
  einem Ausgabefeld gezeigt.

  Wenn man einen Befehl in einer Sitzung eingibt, so versucht die externe
  Anwendung, diesen auszuf�hren. mehrere Befehle k�nnen innerhalb eines
  Dokuments auf einmal gestartet werden, die Ausgabe ist aber nur dort aktiv,
  wo sich der Cursor befindet und auch nur dort wird ausgegeben. Deshalb
  empfehlen wir, unterschiedliche Puffer zu verwenden, wenn mehrere Befehle
  parallel ausgef�hrt werden sollen.

  F�r jede externe Anwendung k�nnen Sie w�hlen, ob Sie einen Prozess f�r
  mehrere Sitzungen gemeinsam haben wollen, oder ob jede Sitzung seinen eigen
  Prozess haben soll. Genauer gesagt, wenn Sie eine Sitzung mit dem Befehl
  <menu|Insert|Session|Other> in ein Dokument einf�gen, dann k�nnen Sie die
  \RSitzungsart'' (Shell, Pari, Maxima <abbr|usw.>) angeben und einen
  \RSitzungsnamen'' (Vorgabe ist \Rdefault''). Sitzungen mit
  unterschiedlichen Namen entsprechen verschiedenen Prozessen und Sitzungen,
  die einen gemeinsamen Namen haben, geh�ren zum gleichen Prozess.

  Um den Prozess, der einer Sitzung zugrunde liegt, zu beenden, benutzen Sie
  <menu|Session|Close session>. Die <menu|Session>-Befehle erscheinen
  automatisch, wenn Sie sich in einer Sitzung befinden.Wenn man
  <key|<key-return>> in einer Sitzung dr�ckt, die nicht mehr verbunden ist,
  dann wird diese wieder automatisch gestartet. Sie k�nnen
  <menu|Session|Interrupt execution> benutzen, um die Ausf�hrung eines
  Befehls in der Sitzung zu unterbrechen, wenn die Anwendung dies erlaubt.

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