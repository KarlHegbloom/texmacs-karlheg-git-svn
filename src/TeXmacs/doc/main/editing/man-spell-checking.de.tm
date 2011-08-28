<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Rechtschreibpr�fung>

  Wenn Sie das Programm <verbatim|ispell> installiert haben, dann k�nnen sie
  es zur Rechtschreibpr�fung benutzen, indem Sie <shortcut|(spell-start)> oder den
  Men�befehl <menu|Edit|Spell> ausf�hren. Sie sollten sich aber vergewissern,
  dass die W�rterb�cher, der von Ihnen benutzten Sprachen vorhanden sind.

  Wenn Sie die Rechtschreibpr�fung gestartet haben, entweder f�r den gesamten
  Text oder f�r eine Auswahl, werden Sie bei jedem falsch geschriebenen Wort
  zu einer Aktion aufgefordert. Die m�glichen Aktionen finden Sie in der
  Fu�zeile:

  <\description>
    <item*|a)>Akzeptiert das Wort und jedes weitere Auftreten.

    <item*|r)>Ersetzt das falsch geschriebene Wort durch eine Korrektur, die
    Sie einzugeben haben.

    <item*|i)>Besagt, dass das \RFalsch geschriebene Wort`` korrekt
    geschrieben ist und so in das W�rterbuch aufgenommen werden soll.

    <item*|1-9)>Mehrere Vorschl�ge, die sie verwenden k�nnen.
  </description>

  Beachten Sie, dass <verbatim|ispell> nur nach falsch geschriebenen Worten
  sucht. Es werden keine grammatikalischen Fehler gefunden.

  Wenn Sie die Rechtschreibpr�fung starten, wird das W�rterbuch derjenigen
  Sprache verwendet, die an der Cursorposition bzw. am Beginn der Auswahl
  aktiv war. Nur Text in dieser Sprache wird gepr�ft. Wenn Ihr Text mehrere
  verschiedene Sprachen benutzt, d.h. das Textteile mit Befehlen des Men�s
  <menu|Format|Language> explizit als Text einer bestimmten Sprache
  formatiert wurden, dann m�ssen Sie die Rechtschreibpr�fung f�r jede dieser
  Sprachen einzeln durchf�hren.

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