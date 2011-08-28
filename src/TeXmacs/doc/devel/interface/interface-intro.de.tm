<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Einf�hrung>

  In diesem Kapitel beschreiben wir, wie <TeXmacs> als Schnittstelle zu
  anderen Programmen dienen kann und wie das zu erreichen ist. Solche
  Schnittstellen sollten als <hyper-link|Plugins|../plugin/plugins.de.tm>
  erstellt und geliefert werden. Das Plugin enth�lt entweder die externe
  Anwendung oder es dient als Vermittler zwischen <TeXmacs> und der
  Anwendung. Normalerweise werden Schnittstellen interaktiv in
  Systemumgebung-Sitzungen verwendet ( Men�befehl: <menu|Insert|Session>).
  Sie k�nnen aber auch f�r Hintergrund-Prozesse, wie Rechtschreibpr�fung und
  Schriftsatz benutzt werden.

  Die Kommunikation zwischen <TeXmacs> und der Anwendung verl�uft �ber ein
  anpassbares Eingabeformat und eine spezielles <em|<TeXmacs> Meta-Format>
  den die Ausgabe vom Plugins. Das Meta-Format erm�glicht eine strukturierte
  Ausgabe vom Plugins in <TeXmacs>, welche ein gemeinsames Format, wie z.B.
  w�rtliche Ausgabe, <LaTeX>, <name|PostScript>, <name|HTML,> oder <TeXmacs>
  selbst. Das ist sehr n�tzlich, wenn eine <TeXmacs>-Schnittstelle zu einer
  vorhandenen Anwendung eingerichtet werden soll, denn oftmals sind <LaTeX>
  oder <name|PostScript> Ausgabe-Routinen bereits implementiert. Zur
  Erst-Implementierung einer Schnittstelle reicht dann die Einf�gung von
  geeigneten Marken in die Ausgabe.

  Sobald eine erste Kommunikation zwischen der Anwendung und <TeXmacs>
  erreicht ist, k�nnen vielfache Verbesserungen eingef�hrt werden, z.B.
  Unterst�tzung f�r Eingabe-Aufforderungen, Vorgaben in der Eingabe,
  automatische Erg�nzung von Befehlen mit der <key|Tab>-Taste, mathematische
  und mehrzeilige Eingabe usw.. Ganz allgemein kann die Anwendung die
  Steuerung von <TeXmacs> �bernehmen und die Schnittstelle modifizieren, z.B.
  Men�s, Tastatur usw. oder \ neue <value|scheme>-Programme in <TeXmacs>
  einbringen. Ihre Anwendung kann sogar das Schriftsatz-Programm modifizieren
  oder erweitern.

  Im Verzeichnis <verbatim|examples/plugins>, k�nnen Sie Beispiele f�r
  einfache Plugins finden.\ 

  Im n�chsten Kapitel werden wir eine etwas eingehendere Beschreibung der
  F�higkeiten von <TeXmacs> anhand dieser Beispiele geben. Erinnern Sie sich,
  wenn Sie solch ein Beispiel ausprobieren m�chten, m�ssen Sie es in eins der
  beiden folgenden Verzeichnisse kopieren:

  <\verbatim>
    \ \ \ \ plugins

    \ \ \ \ $TEXMACS_HOME_PATH/plugins
  </verbatim>

  und <verbatim|make> durchf�hren, sofern ein Makefile vorhanden ist.

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