<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Kontext-Konstrukte>

  Der aktuelle Kontext definiert alle Stil-Parameter, die den Prozess des
  Schriftsetzens beeinflussen, sowie alle zus�tzlichen Anwender-Makros mit
  dem aktuellen Basis-Stil. Die Konstrukte in diesem Abschnitt dienen dazu,
  die Kontextvariablen zu ermitteln und zu ver�ndern.

  <\explain>
    <explain-macro|assign|var|val><explain-synopsis|Variablen global setzen>
  <|explain>
    Dieses Konstrukt setzt die Kontextvariable <src-arg|var> (Zeichenkette)
    auf den Wert <src-arg|val>, der das Ergebnis eines evaluierten Ausdrucks
    sein kann. Er wird eingesetzt, um den Kontext zu �ndern, wie z.B. zur In-
    oder Dekrementierung von Z�hlern.

    Der Evaluierungs-Prozess kann �ber <markup|value>, <markup|provides> und
    Makro-Definitionen beeinflusst werden, desgleichen der Schriftsatz durch
    spezielle Schriftsatz-Variablen.

    <\example>
      Seitenumbruch durch den Stil einschalten.

      Die Variable <verbatim|page-medium> wird genutzt, um den Seitenumbruch
      einzuschalten. Da nur der urspr�ngliche Kontext-Wert benutzt wird, muss
      die Zuordnung in einer Stil-Definition erfolgen. Sie kann in einem
      Dokument nicht ge�ndert werden.

      <\tm-fragment>
        <inactive*|<assign|page-medium|paper>>
      </tm-fragment>
    </example>

    <\example>
      Den Kapitel-Z�hler setzen.

      Das folgende Codest�ck sorgt daf�r, dass das folgende Kapitel die
      Nummer 3 bekommt. Das kann sinnvoll sein, um korrekte Nummerierung im
      Buch-Stil zu erreichen, wenn man in Projekten mit <markup|include>
      arbeitet.

      <\tm-fragment>
        <inactive*|<assign|chapter-nr|2>>
      </tm-fragment>
    </example>
  </explain>

  <\explain>
    <explain-macro|with|var-1|val-1|<with|mode|math|\<cdots\>>|var-n|val-n|body><explain-synopsis|Variablen
    lokal setzen>
  <|explain>
    Dieses Konstrukt setzt die Kontextvariablen <src-arg|var-1> bis
    <src-arg|var-n> (in dieser Reihenfolge) auf die evaluierten Werte
    <src-arg|val-1> bis <src-arg|val-n> und setzt <src-arg|body> in dem
    modifizierten Kontext. Alle mit <markup|assign> erfolgten �nderungen der
    <src-arg|var-1> bis <src-arg|var-n> in <src-arg|body> werden beim
    Verlasen von <markup|with> zur�ckgesetzt.

    Dieses Konstrukt wird in gro�em Umfang in Stil-Definitionen eingesetzt,
    um den Kontext f�r den Schriftsatz zu �ndern, beispielsweise um die
    Schriftart, den Absatz-Stil zu �ndern und den Modus f�r Mathematik
    einzuschalten.
  </explain>

  <\explain>
    <explain-macro|value|var><explain-synopsis|Wert einer Variablen>
  <|explain>
    Dieses Konstrukt evaluiert zu dem aktuellen Wert der Variablen
    <src-var|var> (Zeichenkette). Das wird genutzt, um Z�hler anzuzeigen und
    generell, um Kontext-abh�ngiges Verhalten zu implementieren.

    Dieses Konstrukt wird h�ufig in Stil-Definitionen genutzt, um den Kontext
    zu ver�ndern. Z.B., um lokal die Schriftart, den Absatz-Stil usw. zu
    �ndern.
  </explain>

  <\explain>
    <explain-macro|provides|var><explain-synopsis|definiert?>
  <|explain>
    Dieses Konstrukt ist ein Pr�dikat, dass wahr, <verbatim|true>, ergibt,
    wenn die Kontextvariable <src-var|var> (eine Zeichenkette) definiert ist
    und sonst falsch, <verbatim|false>.

    Das ist n�tzlich, um eine vern�nftige Fehlerbehandlung zu erzeugen, wenn
    beispielsweise ein notwendiges Paket nicht vorhanden ist.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

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