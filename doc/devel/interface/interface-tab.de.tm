<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Eingabe-Erg�nzung>

  Vorgabem��ig �berpr�ft <TeXmacs> Ihr Dokument nach m�glichen Erg�nzungen
  Ihrer Eingabe, wenn Sie auf die <key|tab>-Taste dr�cken. Innerhalb einer
  Sitzung m�chten Sie das Verhalten vielleicht anpassen, z.B. um vorhandene
  Befehle zu vervollst�ndigen. daf�r m�ssen Sie nur die Konfigurations-Option

  <\scheme-fragment>
    (:tab-completion #t)
  </scheme-fragment>

  in der Initialisierungs-Datei <verbatim|init-<em|myplugin>.scm> so setzen,
  dass <TeXmacs> geeignete Anforderungen an die Anwendung richtet, wenn die
  <key|<key-tab>>-Taste innerhalb einer solchen Sitzung gedr�ckt wird. Diese
  Befehle haben die Form

  <\quotation>
    <\framed-fragment>
      <\verbatim>
        <key|DATA_COMMAND>(complete <em|input-string>
        <em|cursor-position>)<key|<key-return>>
      </verbatim>
    </framed-fragment>
  </quotation>

  Hier steht <verbatim|DATA_COMMAND> f�r das Zeichen <verbatim|'\\20'> (ASCII
  16). Der <verbatim|<em|input-string>> ist die komplette Zeichenkette, in
  der <key|<key-tab>> auftrat und <verbatim|<em|cursor-position>> eine
  Ganzzahl, die die Position des Cursors angibt, bei der <key|<key-tab>>
  gedr�ckt wurde. <TeXmacs> erwartet von der Anwendung ein Tupel mit allen
  m�glichen Erg�nzungen der Form

  <\quotation>
    <\framed-fragment>
      <verbatim|<key|DATA_BEGIN>scheme:(tuple <em|root> <em|completion-1>
      ><with|mode|math|\<cdots\>><verbatim| <em|completion-n>)><key|DATA_END>
    </framed-fragment>
  </quotation>

  Hier ist <verbatim|<em|root>> die Teil-Zeichenkette bis zur Cursorposition
  und <verbatim|<em|completion-1>> bis <verbatim|<em|completion-n>> ist eine
  Liste von m�glichen Erg�nzungen, die an der aktuellen Cursorposition
  eingef�gt werden k�nnten. Wenn keine gefunden werden k�nnen Sie eine leere
  Kette zur�ckgeben.

  <\remark>
    Im Prinzip sollte der Eingabe-Erg�nzungs-Mechanismus auch im
    Mathematik-Modus arbeiten. Aber <verbatim|<em|input-string>> entspricht
    dann der Linearisierung der <TeXmacs> Eingabe.
  </remark>

  <\remark>
    Die Art und Weise wie <TeXmacs> Befehle an die Anwendung schickt, kann
    ganz �hnlich wie die Eingabe angepasst werden. Daf�r gibt es die
    Konfigurations-Option <verbatim|:commander>, die ganz �hnlich wie die
    <verbatim|:serializer> Option arbeitet.
  </remark>

  <paragraph|Das plugin <verbatim|complete>>

  Ein sehr rudiment�res Beispiel daf�r, wie der Erg�nzungs-Mechanismus
  arbeitet, findet sich in dem <verbatim|complete> Plugin, das die folgenden
  Dateien umfasst:

  <\verbatim>
    \ \ \ \ <example-plugin-link|complete/Makefile>

    \ \ \ \ <example-plugin-link|complete/progs/init-complete.scm>

    \ \ \ \ <example-plugin-link|complete/src/complete.cpp>
  </verbatim>

  Die Begr��ungs-Botschaft in <verbatim|complete.cpp> sorgt f�r einen Teil
  der Konfiguration:t

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    format_plugin ();

    cout \<less\>\<less\> "We know how to complete 'h'";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Hier lautet <cpp-code|format_plugin> so

  <\cpp-fragment>
    void

    format_plugin () {

    \ \ // The configuration of a plugin can be completed at startup time.

    \ \ // This may be interesting for adding tab-completion a posteriori.

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "command:";

    \ \ cout \<less\>\<less\> "(plugin-configure complete (:tab-completion
    #t))";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-fragment>

  In der Hauptschleife k�mmern wir uns zuerst um die normale Eingabe:

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    if (buffer[0] != DATA_COMMAND) {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-fragment>

  Dann behandeln wir den Fall, dass eine Aufforderung zur Erg�nzung kommt:

  <\cpp-fragment>
    else {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:";

    \ \ cout \<less\>\<less\> "(tuple \\"h\\" \\"ello\\" \\"i there\\"
    \\"ola\\" \\"opsakee\\")";

    \ \ cout \<less\>\<less\> DATA_END;

    }

    fflush (stdout);
  </cpp-fragment>

  Wie Sie sehen, wird der eigentliche Befehl ignoriert. Unser Beispiel ist
  wirklich sehr rudiment�r.

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