<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Das Beispiel: \Rmycas''>

  Ganz zum Anfang eine Vorbemerkung: Wenn es darum geht, Schnittstellen zu
  einem Fremd-Programm zu entwerfen, �ndern Sie den Quellcode des
  Anwenderprogramms so, dass es mit <TeXmacs> zusammenarbeitet. Sie m�ssen
  den Quellcode des Anwenderprogramms also besitzen.

  Ein Beispiel zu analysieren, ist der einfachste Weg, um zu lernen, wie man
  eine neue Schnittstelle von <TeXmacs> zu einem Fremd-Programm installiert.
  In dem Unterverzeichnis Ihres <TeXmacs>-Verzeichnisses, plugins/mycas,
  finden Sie ein Beispiel f�r ein simples \RComputer Algebra System'' mit dem
  Namen <verbatim|mycas>. Dort finden sie auch die Datei <verbatim|mycas.cpp>
  mit diesem wirklich einfachen Programm. Das Programm finden Sie au�erdem am
  Ende dieses Abschnitts. Um es zu testen, m�ssen Sie es mit dem Befehl

  <\verbatim>
    \ \ \ \ g++ mycas.cpp -o mycas
  </verbatim>

  kompilieren und die so erzeugte ausf�hrbare Datei in ein Verzeichnis Ihres
  Suchpfads �bertragen. Wenn Sie dann <TeXmacs> neu starten, sollten Sie im
  Men� <menu|Insert|Session> den Befehl <menu|Mycas> finden.

  <section|Der Quellcode, Schritt f�r Schritt>

  Lassen Sie und den Quellcode von <verbatim|mycas> Schritt f�r Schritt
  durchgehen. Die Kommunikation geht �ber die normalen Eingabe- und
  Ausgabekan�le. Damit <TeXmacs> aber wei� wann die Ausgabe des Programms
  beendet ist, muss der Output in Bl�cke eingekapselt werden und zwar mit
  drei speziellen Steuer-Buchstaben, die Sie so definieren:t\ 

  <\verbatim>
    \ \ \ \ #define DATA_BEGIN \ \ ((char) 2)<next-line> \ \ \ #define
    DATA_END \ \ \ \ ((char) 5)<next-line> \ \ \ #define DATA_ESCAPE
    \ ((char) 27)
  </verbatim>

  Der <verbatim|DATA_ESCAPE>-Buchstabe dient zur Maskierung. Ein unmittelbar
  folgender Buchstabe <em|c> bleibt c, auch wenn es einer der
  Steuerbuchstaben ist. Ein Beispiel, wie <verbatim|DATA_BEGIN> und
  <verbatim|DATA_END> gebraucht werden, zeigt der Begr��ungstext beim Start
  des Programms:\ 

  <\verbatim>
    \ \ \ \ int<next-line> \ \ \ main () {<next-line> \ \ \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";<next-line>
    \ \ \ \ \ cout \<less\>\<less\> "------------------------------------------------------\\n";<next-line>
    \ \ \ \ \ cout \<less\>\<less\> "Welcome to my test computer algebra
    system for TeXmacs\\n";<next-line> \ \ \ \ \ cout \<less\>\<less\> "This
    software comes with no warranty whatsoever\\n";<next-line> \ \ \ \ \ cout
    \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";<next-line>
    \ \ \ \ \ cout \<less\>\<less\> "------------------------------------------------------\\n";<next-line>
    \ \ \ \ \ next_input ();<next-line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_END;<next-line> \ \ \ \ \ fflush (stdout);
  </verbatim>

  Die erste Zeile von <verbatim|main> sorgt daf�r, dass in <TeXmacs> das
  Format \R<localize|verbatim>`` zur Ausgabe benutzt wird. Die Funktion
  <verbatim|next_input> die nach dem Begr��ungstext erscheint, erzeugt eine
  Eingabe-Aufforderung. Sie wird unten genauer erkl�rt. <verbatim|DATA_END>
  schlie�t den Begr��ungsblock und teilt <TeXmacs> mit, dass <verbatim|mycas>
  auf Eingaben wartet. Vergessen Sie nicht den Puffer mit fflush zu leeren,
  damit <TeXmacs> die gesamte Information auch wirklich erh�lt.

  Die Hauptschleife, main loop, beginnt mit der Abfrage, ob im Eingaben im
  Input-Kanal \ vorhanden sind:\ 

  <\verbatim>
    \ \ \ \ \ \ while (1) {<next-line> \ \ \ \ \ \ \ char
    buffer[100];<next-line> \ \ \ \ \ \ \ cin \<gtr\>\<gtr\>
    buffer;<next-line> \ \ \ \ \ \ \ if (strcmp (buffer, "quit") == 0) break;
  </verbatim>

  Haben Sie \Rquit'' eingegeben, wird das Programm beendet.

  Die Ausgabe von <verbatim|mycas>, die als Reaktion auf eine Ihre Eingabe
  erfolgt, muss wieder in einen <verbatim|DATA_BEGIN>-<verbatim|DATA_END>-Block.
  eingeschlossen werden:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<next-line> \ \ \ \ \ \ \ cout \<less\>\<less\> "You typed "
    \<less\>\<less\> buffer \<less\>\<less\> "\\n";
  </verbatim>

  Innerhalb eines solchen Blocks, kann man rekursiv weitere Bl�cke senden,
  die unterschiedliche Formate haben d�rfen. Z.B. schickt der folgende Code
  eine <LaTeX>-Formel:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula:
    ";<next-line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "latex:" \<less\>\<less\> "$x^2+y^2=z^2$"
    \<less\>\<less\> DATA_END;<next-line> \ \ \ \ \ \ \ cout \<less\>\<less\>
    "\\n";
  </verbatim>

  Manchmal kann es n�tzlich sein, direkt im <TeXmacs>-Format zu schicken,
  indem die \ <scheme>- Darstellung benutzt wird:

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And finally a fraction
    ";<next-line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "scheme:" \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
    \<less\>\<less\> DATA_END;<next-line> \ \ \ \ \ \ \ cout \<less\>\<less\>
    ".\\n";
  </verbatim>

  Zum Schluss muss ein abschlie�endes <verbatim|DATA_END> gesendet und der
  Puffer geleert werden:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ next_input ();<next-line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<next-line> \ \ \ \ \ \ \ fflush
    (stdout);<next-line> \ \ \ \ \ }<next-line> \ \ \ \ \ return
    0;<next-line> \ \ \ }
  </verbatim>

  Es ist zu beachten, dass Sie niemals mehr als einen
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>-Block senden d�rfen. Denn
  <TeXmacs> unterstellt, wenn es einen solchen Block erhalten hat, dass Ihr
  Programm auf Eingaben wartet. Wenn Sie mehrere
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>-Bl�cke senden m�ssen, dann m�ssen
  Sie diese zusammen in einen gemeinsamen Block einschlie�en.

  Ein spezieller \RKanal`` wird f�r die �bertragung der Eingabeaufforderung
  benutzt. Kan�le werden als spezielle <verbatim|DATA_BEGIN>-<verbatim|DATA_END>-Bl�cke
  definiert:

  <\verbatim>
    \ \ \ \ static int counter= 0;<next-line><next-line>
    \ \ \ void<next-line> \ \ \ next_input () {<next-line>
    \ \ \ \ \ counter++;<next-line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_BEGIN \<less\>\<less\> "channel:prompt" \<less\>\<less\>
    DATA_END;<next-line> \ \ \ \ \ cout \<less\>\<less\> "Input "
    \<less\>\<less\> counter \<less\>\<less\> "] ";<next-line> \ \ \ }
  </verbatim>

  Innerhalb des \Rprompt`` -Kanals, dem Eingabeaufforderungs-Kanal, kann man
  wiederum \ verschachtelte <verbatim|DATA_BEGIN>-<verbatim|DATA_END>-Bl�cke
  verwenden. Somit kann man auch Formeln f�r die Eingabeaufforderung
  verwenden. Es gibt drei Standardkan�le:

  <\description>
    <item*|<verbatim|output>>Standard-Ausgabe-Kanal.

    <item*|<verbatim|prompt>>Eingabeaufforderungs-Kanal.

    <item*|<verbatim|input>>Damit l�sst sich eine Vorgabe f�r die n�chste
    Eingabe erzeugen.
  </description>

  <section|Graphik-Ausgabe>

  Man kann PostScript-Graphik als Ausgabe schicken. Nehmen wir einmal an,
  dass Sie in Ihrem Heimat-Verzeichnis ein Bild <verbatim|picture.ps> haben.
  Wenn Sie die Zeilen

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";<next-line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "ps:";<next-line> \ \ \ \ \ \ \ fflush (stdout);<next-line>
    \ \ \ \ \ \ \ system ("cat $HOME/picture.ps");<next-line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_END;<next-line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> "\\n";
  </verbatim>

  an einer passenden Stelle der Hauptschleife einf�gen, wird ihr Bild als
  Teil der Programm-Ausgabe auf dem Bildschirm erscheinen.

  <section|Das komplette Programm>

  <\verbatim>
    #include \<less\>stdio.h\<gtr\><next-line>#include
    \<less\>stdlib.h\<gtr\><next-line>#include
    \<less\>string.h\<gtr\><next-line>#include
    \<less\>iostream.h\<gtr\><next-line><next-line>#define DATA_BEGIN
    \ \ ((char) 2)<next-line>#define DATA_END \ \ \ \ ((char)
    5)<next-line>#define DATA_ESCAPE \ ((char)
    27)<next-line><next-line>static int counter=
    0;<next-line><next-line>void<next-line>next_input () {<next-line>
    \ counter++;<next-line> \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "channel:prompt" \<less\>\<less\> DATA_END;<next-line>
    \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";<next-line>}<next-line><next-line>int<next-line>main
    () {<next-line> \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<next-line> \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<next-line>
    \ cout \<less\>\<less\> "Welcome to my test computer algebra system for
    TeXmacs\\n";<next-line> \ cout \<less\>\<less\> "This software comes with
    no warranty whatsoever\\n";<next-line> \ cout \<less\>\<less\> "(c) 2001
    \ by Joris van der Hoeven\\n";<next-line> \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<next-line>
    \ next_input ();<next-line> \ cout \<less\>\<less\> DATA_END;<next-line>
    \ fflush (stdout);<next-line><next-line> \ while (1) {<next-line>
    \ \ \ char buffer[100];<next-line> \ \ \ cin \<gtr\>\<gtr\>
    buffer;<next-line> \ \ \ if (strcmp (buffer, "quit") == 0)
    break;<next-line> \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<next-line> \ \ \ cout \<less\>\<less\> "You typed "
    \<less\>\<less\> buffer \<less\>\<less\> "\\n";<next-line><next-line>
    \ \ \ cout \<less\>\<less\> "And now a LaTeX formula: ";<next-line>
    \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:"
    \<less\>\<less\> "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;<next-line>
    \ \ \ cout \<less\>\<less\> "\\n";<next-line><next-line> \ \ \ cout
    \<less\>\<less\> "And finally a fraction ";<next-line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:" \<less\>\<less\>
    "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;<next-line> \ \ \ cout
    \<less\>\<less\> ".\\n";<next-line><next-line> \ \ \ next_input
    ();<next-line> \ \ \ cout \<less\>\<less\> DATA_END;<next-line>
    \ \ \ fflush (stdout);<next-line> \ }<next-line> \ return 0;<next-line>}
  </verbatim>

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