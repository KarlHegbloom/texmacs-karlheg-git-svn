<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Ein- und Ausgabe �ber Pipelines, Grundlagen>

  Konfiguration und Kompilierung eines sehr einfachen Plugins,
  <verbatim|minimal>, findet sich in <hyper-link|Beispiel eines Plugin in
  <name|C++>|../plugin/plugin-binary.de.tm> im Kapitel �ber
  <hlink|Plugins|../plugin/plugins.de.tm>. \ Wir beginnen mit der Analyse des
  Quellcodes <example-plugin-link|minimal/src/minimal.cpp>. Das
  Hauptprogramm, <verbatim|main>, besteht im wesentlichen aus:

  <\cpp-fragment>
    int

    main () {

    \ \ <em|display-startup-banner>

    \ \ while (true) {

    \ \ \ \ <em|read-input>

    \ \ \ \ <em|display-output>

    \ \ }

    \ \ return 0;

    }
  </cpp-fragment>

  Gem�� Vorgabe sendet <TeXmacs> eine Zeichenkette, die mit <verbatim|'\\n'>
  endet an die Anwendung. Dem entsprechend besteht der Code f�r
  <verbatim|<em|read-input>> aus folgenden Zeilen

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');
  </cpp-fragment>

  Die Ausgabe ist etwas komplizierter, denn <TeXmacs> muss eindeutig
  feststellen k�nnen, dass die Ausgabe beendet ist. Das wird erreicht, in dem
  jedwede Ausgabe, in Blockform eingekapselt wird. Das ist im vorliegenden
  Fall ein Begr��ungstext, banner, und die interaktive Reaktion der
  Anwendung. Die Kapselung sieht folgenderma�en aus:

  <\quotation>
    <framed-fragment|<verbatim|<key|DATA_BEGIN><em|format>:<em|message><key|DATA_END>>>
  </quotation>

  Hier stehen <verbatim|DATA_BEGIN> und <verbatim|DATA_END> f�r spezielle
  Steuer-Buchstaben:

  <\cpp-fragment>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </cpp-fragment>

  Der Steuer-Buchstabe <verbatim|DATA_ESCAPE> dient zur Maskierung
  <verbatim|DATA_BEGIN> und <verbatim|DATA_END> innerhalb des kommunizierten
  Textes

  <\quotation>
    <\framed-fragment>
      <\with|font-family|tt>
        <tabular|<tformat|<table|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_BEGIN>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_BEGIN>>>|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_END>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_END>>>|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_ESCAPE>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_ESCAPE>>>>>>
      </with>
    </framed-fragment>
  </quotation>

  <verbatim|<em|format>> spezifiziert das Format f�r den zu �bertragenden
  Text, <verbatim|<em|message>>. In unserem Beispiel ist der Code,
  <verbatim|<em|display-startup-banner>,> �bertragung und Darstellung der
  Begr��ungs-Botschaft in <TeXmacs> folgender

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "Hi there!";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  Entsprechend wurde <verbatim|<em|display-output>> so programmiert

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  <\remark>
    <TeXmacs> geht davon aus, dass keine Ausgabe mehr kommt, wenn ein
    <key|DATA_END> ein <key|DATA_BEGIN> abschlie�t. Deshalb muss jegliche
    Ausgabe in einen <em|einzigen> �u�eren
    <key|DATA_BEGIN>-<key|DATA_END>-Block eingeschlossen werden. Es ist
    m�glich, solche abgeschlossenen <key|DATA_BEGIN>-<key|DATA_END> Bl�cke in
    einander zu verschachteln. Sie d�rfen aber keinesfalls mehr als einen in
    sich geschlossenen Block senden, da <TeXmacs> sofort die Kontrolle
    �bernimmt, wenn der �u�ere Block abgeschlossen ist.
  </remark>

  <\remark>
    In unserem Beispiel wurde der <value|cpp> Code der Anwendung in die
    Schnittstelle �bernommen. Der �bliche Weg, wenn Sie eine
    <TeXmacs>-Schnittstelle f�r eine bereits existierende Anwendung
    <verbatim|<em|myapp>> schreiben, besteht darin, in der Anwendung eine
    Start-Option <verbatim|--texmacs> zu implementieren. Dann braucht man die
    Verzeichnisse <verbatim|<em|myapp>/src> und <verbatim|<em|myapp>/bin>
    nicht mehr. Es reicht das Plugin zu konfigurieren, indem man z.B. so
    etwas wie die folgenden Zeilen in eine Initialisierungs-Datei
    <verbatim|<em|myapp>/progs/init-<em|myapp>.scm> einf�gt:

    <\scheme-fragment>
      (plugin-configure <em|myapp>

      \ \ (:require (url-exists-in-path? "<em|myapp>"))

      \ \ (:launch "<em|myapp> --texmacs")

      \ \ (:session "<em|Myapp>"))
    </scheme-fragment>

    Wenn der Quellcode der Anwendung <verbatim|<em|myapp>> nicht ver�ndert
    werden kann oder darf, dann bleibt immer noch die M�glichkeit, ein
    Ein-/Ausgabe-Filter <verbatim|tm_<em|myapp>> zu schreiben, der die
    Umsetzung vornimmt. Unter den Standard-Plugins im Verzeichnis\ 

    <\verbatim>
      \ \ \ \ plugins
    </verbatim>

    finden Sie mehrere Beispiele.
  </remark>

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