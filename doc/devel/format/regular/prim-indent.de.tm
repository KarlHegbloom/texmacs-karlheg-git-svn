<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Einz�ge, Grundformen>

  Gew�hnlich wird eine von den folgenden beiden Weisen genutzt, um den
  �bergang von einem Absatz zum n�chsten zu markieren: Die Abs�tze werden
  durch einen kleinen zus�tzlichen Abstand von einander getrennt oder die
  erste Zeile des neuen Absatzes wird eingezogen. Der Einzug kann explizit
  mit den folgenden Marken gesteuert werden: \ <markup|no-indent>,
  <markup|yes-indent>, <markup|no-indent*> und <markup|yes-indent*>. Die
  Grundformen <markup|no-indent> und <markup|yes-indent> werden auf den
  aktuellen Absatz angewandt, w�hrend <markup|no-indent*> und
  <markup|yes-indent*> erst im folgenden Absatz wirksam werden.

  <\explain>
    <explain-macro|no-indent>

    <explain-macro|yes-indent>
  <|explain>
    Aktiviere bzw. deaktiviere den Erstzeileneinzug im aktuellen Absatz. Z.B.
    erzeugt der folgende Code

    <\tm-fragment>
      <\inactive*>
        <no-indent>Dies ist eine langer Absatz, der zeigt wie der
        Erstzeilenabzug durch <active*|<markup|no-indent>> abgeschaltet wird.

        <yes-indent>Dies ist eine langer Absatz, der zeigt wie der
        Erstzeilenabzug durch <active*|<markup|yes-indent>> eingeschaltet
        wird.
      </inactive*>
    </tm-fragment>

    gew�hnlich

    <\tm-fragment>
      <\with|par-first|2fn>
        <active*|<\inactive*>
          <active*|<no-indent><arg|>>Dies ist eine langer Absatz, der zeigt
          wie der Erstzeilenabzug durch <active*|<markup|no-indent>>
          abgeschaltet wird.

          <style-with|src-compact|none|<active*|<yes-indent>>>Dies ist eine
          langer Absatz, der zeigt wie der Erstzeilenabzug durch
          <active*|<markup|yes-indent>> eingeschaltet wird.
        </inactive*>>
      </with>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|no-indent*>

    <explain-macro|yes-indent*>
  <|explain>
    Aktiviere bzw. deaktiviere den Erstzeileneinzug im nachfolgenden Absatz.
    Beispielsweise erzeugt

    <\tm-fragment>
      <\inactive*>
        Ein erster Absatz.<yes-indent*>

        Ein zweiter Absatz.
      </inactive*>
    </tm-fragment>

    gew�hnlich

    <\tm-fragment>
      <\with|par-first|2fn>
        Ein erster Absatz.<yes-indent*>

        Ein zweiter Absatz.
      </with>
    </tm-fragment>

    Es bleibt anzumerken, dass <markup|no-indent> und <markup|yes-indent>
    Vorrang vor <markup|no-indent*> und <markup|yes-indent*>-Konstrukten im
    vorg�ngigen Absatz haben.

    Derzeit werden die <markup|no-indent*> und <markup|yes-indent*>
    -Anweisungen vor allem benutzt, um den Einzug nach
    Abschnitt-�berschriften oder Abs�tzen mit speziellen Layout zu steuern (
    z.B. <markup|equation>). Es ist geplant, dass zuk�nftig Abschnitt-Marken
    den Abschnittsrumpff als Argument erhalten. Wenn das geschehen ist, wird
    die Absatz-Marke (<markup|paragraph> tag) korrekt implementiert und
    \ <markup|no-indent*> sowie <markup|yes-indent*> werden �berfl�ssig.
  </explain>

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
    <associate|info-flag|short>
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>