<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Stichwortverzeichnis erzeugen>

  Um ein Stichwortverzeichnis zu erstellen, m�ssen Sie erst einmal
  Stichwort-Eintr�ge (Index-Eintr�ge) in Ihrem Dokument machen. Dazu dient
  die Befehle aus dem Men� <menu|Insert|Link|Index entry>. Als zweites m�ssen
  Sie an der Stelle, an der das Stichwortverzeichnis erscheinen soll, den
  Men�befehl <menu|Text|Automatic-generated-lists|Index> ausf�hren. Das
  Stichwortverzeichnis wird dann in �hnlicher Weise erstellt wie das
  Inhaltsverzeichnis.

  Im Men� <menu|Insert|Link|Index entry> finden Sie verschiedene Arten von
  Stichwort-Eintr�gen. Die einfachsten sind \R<localize|main>'',
  \R<localize|sub>'' und \R<localize|subsub>'', welche Makros mit ein, zwei
  bzw. drei Argumenten sind. Eintr�ge der Formen \ \R<localize|sub>'' und
  \R<localize|subsub>'' werden benutzt um sie als Unter-Stichworte zu anderen
  einzuordnen.

  Ein <em|komplexer Index-Eintrag> hat vier Argumente. Das erste ist ein
  Schl�ssel, der die Sortierung angibt. Er muss ein Tupel sein und wird mit
  <kbd-ia|\<less\>> erzeugt. Die erste Komponente des Tupels ist die
  Haupt-Kategorie, die zweite eine Unter-Kategorie usw.. Das zweite Argument
  eines <em|komplexen Index-Eintrags> ist entweder leer oder es ist
  \Rstrong''. In diesem Fall wird der Eintrag <strong|fett> dargestellt. Das
  dritte Argument ist meist leer. Wenn sie dagegen zwei Index-Eintr�ge mit
  dem gleichen nicht leeren dritten Argument benutzen, dann erzeugt <TeXmacs>
  einen Seitenummern-Bereich. Das vierte Argument ist wieder ein Tupel; es
  ist der Eintrag selbst.

  Man kann auch Eintr�ge in das Stichwortverzeichnis vornehmen, ohne dass
  eine Zeilennummer erscheint. Benutzen Sie dazu <menu|Insert|Link|Index
  entry|Interjection>. Das erste Argument diese Makros ist ein Schl�ssel zur
  Einsortierung. Das zweite Argument enth�lt den eigentlichen Text. Damit
  kann man beispielsweise Kennzeichen f�r Abschnitte in das
  Stichwortverzeichnis einbringen, z.B. \RA'', \RB'' usw..

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