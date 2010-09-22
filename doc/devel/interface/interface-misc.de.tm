<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Weitere F�higkeiten>

  <TeXmacs> hat noch einige andere Besonderheiten aufzuweisen, die zur
  Programmierung von Schnittstellen beitragen. Einige davon sind sehr
  speziell.

  <paragraph|Interrupts>

  Das \R<localize|Stop>''-Icon kann dazu benutzt werden, die Evaluierung
  abzubrechen. Es unterbricht die Evaluierung, ebenso wie der Men�befehl
  <menu|Session|Interrupt execution>, indem es <verbatim|SIGINT> an die
  Anwendung schickt. Es erwartet, dass die Anwendung normal beendet wird, vor
  allem, dass sie alle offenen <render-key|DATA_BEGIN>-Bl�cke abschlie�t.

  <paragraph|Testen, ob der Input vollst�ndig ist.>

  Einige Anwendungen starten in einem Eingabe-Modus, der mehrzeilige Eingabe
  erm�glicht oder erwartet, sobald Sie damit beginnen, eine Funktion zu
  definieren, oder eine �ffnende Klammer setzen ohne eine entsprechende
  schlie�ende Klammer. <TeXmacs> hat die M�glichkeit, in Ihrer Anwendung ein
  spezielles Pr�dikat zu definieren, das abfragen kann, ob die Eingabe
  abgeschlossen ist. Dazu m�ssen Sie die Konfigurations-Option

  <\scheme-fragment>
    (:test-input-done #t)
  </scheme-fragment>

  spezifizieren. Wenn Sie nun <shortcut|(kbd-return)> in Ihrer Eingabe dr�cken,
  dann sendet <TeXmacs> den Befehl

  <\quotation>
    <\framed-fragment>
      <\verbatim>
        <render-key|DATA_COMMAND>(input-done? <em|input-string>)<shortcut|(kbd-return)>
      </verbatim>
    </framed-fragment>
  </quotation>

  und Ihre Anwendung sollte mit

  <\quotation>
    <\framed-fragment>
      <verbatim|<render-key|DATA_BEGIN>scheme:<em|done><render-key|DATA_END>>
    </framed-fragment>
  </quotation>

  antworten, worin <verbatim|<em|done>> entweder <verbatim|#t> oder
  <verbatim|#f> ist. Das Plugin <verbatim|multiline> ist ein Beispiel f�r
  dieses Vorgehen: <example-plugin-link|multilinea/src/multiline.cpp>.

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