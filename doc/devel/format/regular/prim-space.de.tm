<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Leerraum-Konstrukte>

  <\explain>
    <explain-macro|vspace|len>

    <explain-macro|vspace|len|min|max><explain-synopsis|Vertikaler Abstand
    danach>
  <|explain>
    Dieses Konstrukt f�gt einen vertikalen variablen Abstand nach dem
    aktuellen Absatz ein. Alle Operanden m�ssen
    <hyper-link|L�ngeneinheiten|../basics/lengths.de.tm> sein. Das
    <src-arg|len>-Argument spezifiziert die Vorgabel�nge und <src-arg|min>
    bzw. <src-arg|max> die untere und obere Grenze der Dehnbarkeit. Werden
    <src-arg|min> und <src-arg|max> nicht angegeben, dann wir \ aus
    <src-arg|len> nach impliziten Vorgaben eine obere und untere Grenze
    berechnet.

    Beachten Sie, dass die Operanden nicht evaluiert werden. Sie m�ssen daher
    Zeichenketten sein.\ 
  </explain>

  <\explain>
    <explain-macro|vspace*|len>

    <explain-macro|vspace*|len|min|max><explain-synopsis|Vertikaler Abstand
    davor>
  <|explain>
    Dieses Konstrukt �hnelt <markup|vspace> bis auf die Tatsache, dass der
    vertikale Abstand vor dem aktuellen Absatz eingef�gt wird. Der daraus
    resultierende wirkliche Abstand ist das Maximum der mit <markup|vspace>
    und <markup|vspace*> spezifizierten Abst�nde zweier auf einander
    folgender Abs�tze - nicht die Summe.
  </explain>

  <\explain>
    <explain-macro|space|len>

    <explain-macro|space|len|bot|top><explain-synopsis|Fixer horizontaler
    Abstand>
  <|explain>
    Dieses Konstrukt f�gt eine leere Box ein, deren L�nge <src-arg|len> ist
    und deren Untergrenze bzw. Obergrenze um <src-arg|bot> und <src-arg|top>
    oberhalb der Basislinie liegen.

    Wenn <src-arg|bot> und <src-arg|top> nicht spezifiziert werden, dann wird
    eine leere Box eingef�gt, deren Untergrenze die Basislinie ist und deren
    Obergrenze auf der H�he des Kleinbuchstabens x in der aktuellen
    Schriftart und -gr��e ist.

    Beachten Sie, dass die Operanden nicht evaluiert werden. Sie m�ssen daher
    Zeichenketten sein.\ 
  </explain>

  <\explain>
    <explain-macro|hspace|len>

    <explain-macro|hspace|len|min|max><explain-synopsis|Variabler
    horizontaler Abstand>
  <|explain>
    Dieses Konstrukt f�gt einen variablen horizontalen Abstand der Nennl�nge
    <src-arg|len> ein. <src-arg|len> muss eine
    <hyper-link|L�ngeneinheit|../basics/lengths.de.tm> sein. <src-arg|min>
    und <src-arg|max> spezifizieren Ober- und Untergrenzen der Dehnbarkeit.
    Wenn <src-arg|min> und <src-arg|max> nicht angegeben werden, dann werden
    sie aus <src-arg|len> nach impliziten Vorgaben berechnet.

    Beachten Sie, dass die Operanden nicht evaluiert werden. Sie m�ssen daher
    Zeichenketten sein.\ 
  </explain>

  <\explain>
    <explain-macro|htab|min>

    <explain-macro|htab|min|weight><explain-synopsis|Horizontaler Sprung>
  <|explain>
    Spr�nge sind horizontaler Leerraum, der so gedehnt wird, dass er den
    ganzen verf�gbaren horizontalen Raum einnimmt. Wenn ein Absatz
    umgebrochen wird, also in mehrere sichtbare Zeilen zerlegt wird, dann
    werden nur Spr�nge in der letzten Zeile gedehnt.

    Ein Sprung hat eine <em|Minimall�nge> (<with|color|brown|min>)und ein
    <em|Gewicht> (<with|color|brown|weight>). Wenn das Gewicht 0 ist, dann
    handelt sich um einen schwachen Sprung, sonst um einen starken. Wenn eine
    Zeile sowohl starke wie schwache Spr�nge enth�lt, dann werden nur die
    starken gedehnt.\ 

    Der Bruchteil des vorhandenen horizontalen Leerraums, der jedem starken
    Sprung zugeteilt wird, ist seinem Gewicht proportional. Wenn nur schwache
    Spr�nge existieren, erh�lt jeder den gleichen Anteil.

    <\indent>
      <explain-macro|htab|min> f�gt einen starken Sprung der Minimall�nge
      <src-arg|min> und Gewicht 1 ein. Der <src-arg|min> -Operand muss eine
      <hyper-link|L�ngeneinheit|../basics/lengths.de.tm> sein.

      <explain-macro|htab|min|weight> spezifiziert ein Gewicht, das entweder
      eine positive Dezimalzahl ist oder eine von den unten angegebenen
      Werten.

      <explain-macro|htab|min|<src-value|first>> f�gt einen <em|hinten
      schwachen> Sprung ein, signifikant ist nur der erste in einem Absatz.

      <explain-macro|htab|min|<src-value|last>> f�gt einen <em|vorne
      schwachen> Absatz ein, nur der letzte in einem Absatz ist signifikant.
    </indent>

    Operanden werden nicht evaluiert, sie m�ssen daher Zeichenketten sein.

    Schwache Spr�nge sind in Stildefinitionen sehr n�tzlich. Beispielsweise
    werden hinten schwache Spr�nge benutzt, damit Listen sich �ber eine
    vollen Absatz ausdehnen k�nnen. Das ist n�tig, damit in verschachtelten
    Listen die vertikalen Abstands-Konstrukte richtig funktionieren. In
    normalen Dokumenten werden Spr�nge oft dazu benutzt, um ein Textst�ck auf
    die rechte Seite einer Seite zu platzieren und ein anderes auf die linke
    Seite.
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
    <associate|preamble|false>
  </collection>
</initial>