<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Interne Konstrukte>

  Die hier pr�sentierten Konstrukte sind nur f�r den internen Gebrauch von
  <TeXmacs> gedacht. Sie werden �berhaupt nur der Vollst�ndigkeit wegen
  erw�hnt. Sie sollten sie nur gebrauchen, wenn Sie wirklich wissen, was sie
  tun und auch dann nur mit gro�er Sorgfalt.

  <\explain|<explain-macro|unknown><explain-synopsis|Unbekannter Inhalt oder
  uninitialisierte Daten>>
    Dieses Konstrukt dient dazu, uninitialisierte Kontextvariablen zu
    kennzeichnen.
  </explain>

  <\explain|<explain-macro|error|message><explain-synopsis|Fehlermeldung>>
    Dies sollte in Dokumenten nie erscheinen. Es dient dazu, falsche Fehler
    in Konstrukten aufzufinden. Es wird w�hrend der Evaluierung von
    Konstrukten generiert, denen unzul�ssige Operanden �bergeben werden.
  </explain>

  <\explain>
    <explain-macro|collection|binding-1|<with|mode|math|\<cdots\>>|binding-n>

    <explain-macro|associate|key|value><explain-synopsis|Hash-Tabellen>
  <|explain>
    Das <markup|collection>-Konstrukt dient zur Definition von Hash-Tabellen
    mit den Elementen <src-arg|binding-1> bis <src-arg|binding-n>. Jedes
    Element hat die Form <explain-macro|associate|key|value> mit einem
    Schl�ssel <src-arg|key> und dem dazugeh�rigen Wert <src-arg|value>.
  </explain>

  <\explain>
    <explain-macro|attr|key-1|val-1|<with|mode|math|\<cdots\>>|key-n|val-n><explain-synopsis|XML-artige
    Attribute>
  <|explain>
    Dieses Konstrukt wurde eingef�gt, um in der Zukunft Kompatibilit�t mit
    XML zu haben. Es dient zur Codierung von XML-Stil-Attributen durch
    <TeXmacs>-B�ume. Das XML-Fragment

    <\quote-env>
      <framed-fragment|<\verbatim>
        \<less\>blah color="blue" emotion="verbose"\<gtr\>

        \ \ Some XML stuff

        \<less\>/blah\<gtr\>
      </verbatim>>
    </quote-env>

    w�rde beispielsweise durch

    <\tm-fragment>
      <inactive*|<blah|<attr|color|blue|emotion|verbose>|Some XML stuff>>
    </tm-fragment>
  </explain>

  in <TeXmacs> codiert.

  <\explain>
    <explain-macro|tag|content|annotation>

    <explain-macro|meaning|content|annotation><explain-synopsis|Einen Inhalt
    mit einer Bedeutung versehen>
  <|explain>
    Einem Inhalt <src-arg|content> eine bestimmte Bedeutung hinzuf�gen. Zur
    Zeit werden diese Konstrukte praktisch nicht benutzt.
  </explain>

  <\explain>
    <explain-macro|backup|save|stack><explain-synopsis|Werte auf dem Stack
    sichern>
  <|explain>
    Dient zur zeitweiligen Sicherung von Werten auf dem Stack.
  </explain>

  <\explain>
    <explain-macro|dbox><explain-synopsis|Markierung f�r Dekorationen>
  <|explain>
    Dieses Konstrukt ist f�r den ausschlie�lichen internen Gebrauch durch die
    Konstrukte <markup|datoms>, <markup|dlines> und <markup|dpages> gedacht.
  </explain>

  <\explain>
    <explain-macro|rewrite-inactive|t|var><explain-synopsis|Internes
    Konstrukt zur Darstellung von inaktiven Befehlen>
  <|explain>
    Dieses interne Konstrukt schreibt inaktive B�ume in neue B�ume um, deren
    Darstellung dem inaktiven B�umen entspricht.
  </explain>

  <\explain>
    <explain-macro|new-dpage>

    <explain-macro|new-dpage*><explain-synopsis|Neue Doppelseite>
  <|explain>
    Konstrukt zur Erzeugung einer neuen Doppelseite. Muss erst implementiert
    werden.
  </explain>

  <\explain>
    <explain-macro|identity|markup><explain-synopsis|Identit�ts-Makro>
  <|explain>
    Das Identit�ts-Makro ist ein Teil von <TeXmacs>. Es sollte aber
    eigentlich nicht als fundamentales Konstrukt verstanden werden, obwohl es
    kein Teil von einer Stil-Definition ist.
  </explain>

  Au�er diesen Konstrukten gibt es noch weitere, die veraltet sind und nicht
  mehr von <TeXmacs> benutzt werden. Man sollte aber vermeiden, ihre Namen
  bei der Erstellung eigener Makros zu benutzen. Es sind dies:
  <markup|format>, <markup|line-sep>, <markup|with-limits>, <markup|split>,
  <markup|old-matrix>, <markup|old-table>, <markup|old-mosaic>,
  <markup|old-mosaic-item>, <markup|set>, <markup|reset>, <markup|expand>,
  <markup|expand*>, <markup|hide-expand>, <markup|apply>, <markup|begin>,
  <markup|end>, <markup|func>, <markup|env>, <markup|authorize>.

  <tmdoc-copyright|2004|Joris van der Hoeven>

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