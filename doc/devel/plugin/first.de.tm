<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|So schreiben Sie Ihre erste Schnittstelle zu <TeXmacs>>

  Beachten Sie bitte, dass Sie den Quellcode des Anwenderprogramms haben
  m�ssen, denn die Schnittstelle erzeugen Sie in diesem Programm.\ 

  Wenn Sie eine Schnittstelle in <TeXmacs> schreiben wollen, empfehlen wir
  Ihnen die folgenden Schritte:

  <\enumerate>
    <item>Erzeugen Sie eine <verbatim|--texmacs> Option f�r das
    Anwenderprogramm, die benutzt wird, wenn das Programm von <TeXmacs> aus
    aufgerufen wird.

    <item>Modifizieren Sie die Ausgaberoutinen so, dass f�r <TeXmacs>
    verst�ndliche Ausgaben erzeugt werden, wenn das Programm mit der Option
    <verbatim|--texmacs> gestartet wurde.

    <item>Erzeugen Sie ein <verbatim|mycas> Skript, in einem Verzeichnis
    Ihres Suchpfades, das Ihr Anwenderprogramm mit der Option
    <verbatim|--texmacs> aufruft.
  </enumerate>

  danach wird Ihr Programm unter dem Namen <menu|Mycas> in dem Men�
  <menu|Insert|Session> zur Verf�gung stehen. Wir werden sp�ter erkl�ren, wie
  Sie daf�r sorgen k�nnen, dass das Programm unter seinem eigenen Namen
  erscheint, wie man es anpassen kann und wie man die Schnittstelle in die
  eigentliche <TeXmacs>-Distribution inkorporiert.

  Normalerweise ist es der zweite Schritt, der die meisten Schwierigkeiten
  macht und die meiste Zeit kostet. Wenn saubere Ausgabe-Routinen existieren,
  einschlie�lich derjenigen f�r die Fehlermeldungen, dann reicht es im
  allgemeinen aus, diese, dem <verbatim|mycas>-Beispiel folgend, zu
  modifizieren \ und die existierenden <LaTeX>-Ausgabe-Routinen zu benutzen,
  die die meisten Systeme haben.

  Zur Zeit haben wir nur <LaTeX>-als Standard-Format implementiert, da dieses
  Format das meist benutzte f�r mathematische Formeln ist. F�r die Zukunft
  planen wir die Implementierung weiterer, semantisch sichererer Formate. Wir
  empfehlen, daran zu denken, dass das Baum-Format benutzt werden kann, um
  die Ausgabe zu �bertragen.

  Zum Standard-<LaTeX>haben wir <verbatim|\\*> und <verbatim|\\bignone>
  Befehle f�r die Multiplikation und zum Abschluss von gro�en
  Operator-Symbolen. Damit kann unterschieden werden, zwischen

  <\verbatim>
    \ \ \ \ a \\* (b + c)
  </verbatim>

  (d.h. <with|mode|math|a> multipliziert mit <with|mode|math|b+c>)\ 

  und

  <\verbatim>
    \ \ \ \ f(x + y).
  </verbatim>

  (d.h <with|mode|math|f> angewendet auf <with|mode|math|x+y>).\ 

  Ganz �hnlich:

  <\verbatim>
    \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
  </verbatim>

  Hier wird der <verbatim|\\bignone> Befehl benutzt, um das Kontext-Ende f�r
  den <verbatim|\\sum> Operator zu markieren.

  Es zeigt sich, dass der systematische Gebrauch von <verbatim|\\*> und
  <verbatim|\\bignone> in Kombination mit sauberer <LaTeX>-Ausgabe f�r die
  verbleibenden Konstrukte, es <with|font-shape|italic|a priori> erm�glichst,
  der Ausgabe eine passende Bedeutung zu geben. Insbesondere k�nnen
  zus�tzliche Routinen zum Kopieren und Einf�gen zwischen verschiedenartigen
  Systemen formuliert werden.

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