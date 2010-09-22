<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Konversion von <TeXmacs> nach <LaTeX>>

  Die h�ufigste Aufgabe ist es, einen Artikel von <TeXmacs> nach <LaTeX> zu
  konvertieren, um ihn an eine Zeitschrift zur Ver�ffentlichung zu schicken.
  Aus einer <TeXmacs>-Datei, die sie (in den Puffer) geladen haben, k�nnen
  Sie mit dem Men�-Befehl <menu|File|Export|Latex> eine <TeX>-Datei machen.
  Ihnen wird \ Ihnen ein Name vorgeschlagen, den Sie belassen oder �ndern
  k�nnen. Am besten wenden Sie dann versuchsweise <LaTeX> auf die neue Datei
  an, um zu sehen, ob Sie ein ansprechendes Resultat erhalten. Wenn das
  gelingt, dann sollten Sie zusammen mit der <TeX>-Datei auch den die
  Stil-Datei <verbatim|TeXmacs.sty> weitergeben. Sie finden diese Stil-Datei
  in einem Unterverzeichnis ihres <TeXmacs>-Verzeichnisses
  (<verbatim|misc/latex>).

  Oft hat die Zeitschrift, bei der Sie einreichen wollen, ihren eigenen
  <LaTeX>-Stil, z.B. <verbatim|journal.sty>. In diesem Fall sollten Sie die
  Datei\ 

  <\verbatim>
    \ \ \ \ styles/article.ts
  </verbatim>

  , die sich in Unterverzeichnis ihres <TeXmacs>-Verzeichnisses befindet nach\ 

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  kopieren und als Ihren Basis-Stil im Men� <menu|Document|Style|Other>
  definieren. Dann k�nnen Sie <verbatim|journal.ts> so anpassen, dass das
  Layout so gut wie m�glich dem vorgeschriebenen Layout entspricht. In
  manchen F�llen muss man au�erdem auch <verbatim|TeXmacs.sty> kopieren und
  die Kopie anpassen, um Kompatibilit�t mit dem Stil <verbatim|journal.sty>
  zu erreichen.

  Wenn Ihr erster Versuch der Konvertierung nicht zu einem Ergebnis kam,
  werden Sie oft finden, dass nur ein kleiner Teil des Dokuments falsch
  konvertiert wurde. Das kann mehrere Gr�nde haben, wichtigsten sind die
  folgenden:

  <\itemize>
    <item>Ihr Text benutzte einige <TeXmacs>-spezifische Optionen, die in
    <LaTeX> nicht existieren. \ 

    <item>Sie benutzten <TeXmacs>-Optionen, die noch nicht in dem
    Konversions-Algorithmus implementiert sind.

    <item>Sie fanden einen Bug im Konversions-Algorithmus.
  </itemize>

  Dies wird im folgenden Abschnitt etwas genauer behandelt. Zun�chst jedoch
  soll ein Weg aufgezeigt werden, um geringf�gige Darstellungs-Probleme zu
  korrigieren:

  Eine naive Strategie zur Problem-Korrektur besteht darin, die <LaTeX>-Datei
  zu korrigieren und dann die korrigierte Datei an die Zeitschrift zu senden.
  Das hat aber den Nachteil, das man man diese Korrekturen jedes mal machen
  muss, wenn man nach �nderungen erneut exportiert. Eine bessere Strategie
  besteht darin die Men�befehle <menu|Insert|Specific|Latex> und
  <menu|Insert|Specific|Texmacs> zu benutzen, um Text zuschreiben, der nur in
  jeweils einer Version (entweder <TeXmacs> oder <LaTeX>) sichtbar ist.

  Nehmen wie einmal beispielhaft an, dass das Wort
  \Rfr�hkonstantinopolitanisch\R in <TeXmacs> korrekt getrennt wird aber
  nicht in der <LaTeX>-Version. Dann k�nnen sie so vorgehen:

  <\enumerate>
    <item>\Rfr�hkonstantinopolitanisch\R ausw�hlen.

    <item>Auf <menu|Insert|Specific|Texmacs> klicken, um es
    <TeXmacs>-spezifisch zu machen.

    <item>Auf <menu|Insert|Specific|Latex> klicken.

    <item>Dann <verbatim|fr�h\\-kon\\-stan\\-ti\\-no\\-po\\-li\\-ta\\-nisch>
    mit der korrekten Trennung ein.\ 

    <item>Dr�cken Sie <shortcut|(kbd-return)>, um den <LaTeX>-spezifischen Text zu
    aktivieren.
  </enumerate>

  In �hnlicher Weise k�nnen Sie <LaTeX>-spezifische Zeilen- und
  Seitenumbr�che, vertikalen Leerraum, Stil-Parameter usw. einf�gen.

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