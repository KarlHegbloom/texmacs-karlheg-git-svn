<TeXmacs|1.0.4.2>

<style|<tuple|tmdoc|number-europe>>

<\body>
  <tmdoc-title|Z�hler und Z�hlergruppen>

  <TeXmacs> benutzt zum automatischen Nummerieren von Abschnitten, S�tzen
  usw. Z�hler \Rcounter''. Solche Z�hler k�nnen individuelle Z�hler sein wie
  z.B. <src-var|equation-nr> oder zu einer Gruppe �hnlicher Z�hler geh�ren
  wie etwa <src-var|theorem-nr>. <TeXmacs> erlaubt die Anpassung von Z�hlern
  auf individueller oder auf Gruppenbasis. Man kann die Darstellung des
  Z�hlers �ndern, diesen beispielsweise mit r�mischen Zahlen ausgeben, oder
  man kann spezielle Aktionen durchf�hren, dem Z�hler z.B. erh�hen oder einen
  Unterz�hler neu initialisieren.

  Neue individuelle Z�hler k�nnen mit dem folgenden Makro definiert werden:

  <\explain|<explain-macro|new-counter|x>>
    definiert einen neuen Z�hler mit dem Namen <src-arg|x>. Der Z�hler wird
    in dem numerischen Kontext <src-var|x-nr> gespeichert. Gleichzeitig
    werden die folgenden Makros erzeugt:

    <\explain|<explain-macro|the-<em|x>>>
      Greife auf den Z�hler zu, so wie er auf dem Bildschirm erscheint.
    </explain>

    <\explain|<explain-macro|reset-<em|x>>>
      Setze den Z�hler auf 0.
    </explain>

    <\explain|<explain-macro|inc-<em|x>>>
      Inkrementiere den Z�hler um 1. Dieses Makro kann so ge�ndert werden,
      dass es auch andere Z�hler inkrementiert, auch wenn das so in den
      Standard-Stil-Definitionen nicht gemacht wird.
    </explain>

    <\explain|<explain-macro|next-<em|x>>>
      Inkrementiere den Z�hler um 1, zeige den Z�hler auf dem Bildschirm und
      setze das aktuelle Label.
    </explain>

    Um Anpassungen zu erleichtern, definiert <markup|new-counter> zus�tzlich
    die folgenden beiden Makros:

    <\explain|<explain-macro|display-<em|x>|nr>>
      Dieses Makro transformiert den numerischen Wert des Z�hlers in
      denjenigen, der auf dem Bildschirm gezeigt wird.
    </explain>

    <\explain|<explain-macro|counter-<em|x>|x>>
      Dieses interne Makro gibt den Namen desjenigen Kontexts zur�ck, dem der
      Z�hler angeh�rt. Normalerweise ist dies \Rnr-x'', es kann aber auch
      umdefiniert worden sein, wenn der Z�hler zu einer Gruppe geh�rt.
    </explain>
  </explain>

  Wie bereits gesagt benutzt <TeXmacs> <em|Z�hlergruppen>, um �hnliche Z�hler
  auf �hnliche Weise zu behandeln. Z.B. geh�ren zu Z�hlergruppe
  <verbatim|theorem-env> die Z�hler <verbatim|theorem>,
  <verbatim|proposition>, <verbatim|lemma>, <abbr|usw.>. Man definiert neue
  Z�hlergruppen werden mit:

  <\explain|<explain-macro|new-counter-group|g>>
    erzeugt eine neue Z�hlergruppe mit dem Namen <src-arg|g> und gleichzeitig
    folgende Makros:

    <\explain>
      <explain-macro|display-in-<em|g>|x|nr>

      <explain-macro|counter-in-<em|g>|x>
    <|explain>
      Diese Makros verhalten sich analog zu den oben beschriebenen Makros
      <markup|display-<em|x>> und <markup|counter-<em|x>> jedoch f�r Z�hler
      der Gruppe <src-arg|g>. Sie �bernehmen als Argument den Namen
      <src-arg|x> des Z�hlers.
    </explain>
  </explain>

  Neue Z�hler k�nnen zur Gruppe zugef�gt werden mit:

  <\explain|<explain-macro|add-to-counter-group|x|g>>
    Damit wird ein neuer Z�hler <src-arg|x> definiert und zur Gruppe
    <src-arg|g> hinzugef�gt. Die Rolle der Makros \ <markup|display-<em|x>>
    und <markup|counter-<em|x>> wird bei Gruppen von den Makros
    <markup|display-in-<em|g>> und <markup|counter-in-<em|g>> �bernommen.
    Zus�tzlich werden aber zwei weitere Makros definiert
    <markup|ind-display-<em|x>> und <markup|ind-counter-<em|x>>, die die
    Rolle von <markup|display-<em|x>> und <markup|counter-<em|x>> in
    denjenigen F�llen �bernimmt, in denen die Gruppe aus individuellen
    Z�hlern besteht.
  </explain>

  Jederzeit kann man entscheiden, ob die Z�hler einer Gruppe einen
  gemeinsamen Gruppenz�hler oder sie individuelle Z�hler benutzt. Dies wird
  z.B. gebraucht, wenn man zwischen dem Amerikanischen Nummerierungsstil und
  dem europ�ischen wechselt:

  <\explain|<explain-macro|group-common-counter|g>>
    Benutze einen gemeinsamen Z�hler f�r die Z�hlergruppe. Dieser wird in der
    Kontextvariablen <src-var|g-nr> gespeichert.
  </explain>

  <\explain|<explain-macro|group-individual-counters|g>>
    Benutze einen individuellen Z�hler f�r jedes Gruppenmitglied. Das ist die
    Vorgabe.
  </explain>

  Es sei darauf hingewiesen, dass Gruppenz�hler rekursiv zu Obergruppen
  geh�ren k�nnen. Das zeigen z.B. die folgenden Ausdr�cke aus
  <verbatim|env-base.ts>:

  <\tm-fragment>
    <\inactive*>
      <new-counter-group|std-env>
    </inactive*>

    <inactive*|<new-counter-group|theorem-env>>

    <inactive*|<add-to-counter-group|theorem-env|std-env>>

    <inactive*|<group-common-counter|theorem-env>>
  </tm-fragment>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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