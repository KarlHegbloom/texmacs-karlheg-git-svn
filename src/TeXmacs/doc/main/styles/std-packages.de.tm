<TeXmacs|1.0.4.2>

<style|<tuple|tmdoc|>>

<\body>
  <tmdoc-title|<TeXmacs>-Standard-Pakete>

  <TeXmacs> stellt eine Anzahl von Paketen bereit, mit denen sich das
  Verhalten der Standard-Stile �ndern l�sst:

  <\description>
    <item*|<tmpackage|number-long-article>><strong|=
    <localize|number-long-article>:> Dieses Paket sorgt daf�r, dass in allen
    nummerierten Kontexten (<localize|<active*|theorem, remark, equations,
    figure, etc.>>) die Nummern mit der aktuellen Abschnitts-Nummer als
    Pr�fix versehen werden. Dieses Paket wird meist mit dem Basis-Stil
    <compound|localize|article>, <tmstyle|article>, oder <localize|book>,
    <tmstyle|book>, benutzt.

    <item*|<tmpackage|number-europe>><strong|= <localize|number-europe>:>
    Normalerweise benutzt <TeXmacs> den Amerikanischen Nummerierung-Stil,
    d.h., dass ein und derselbe Z�hler f�r alle �hnlichen Z�hler wie z.B.
    <localize|theorem> <inactive|>oder <localize|proposition> verwendet
    werden. In anderen Worten eine <localize|remark>, die auf
    \R<localize|theorem> 3`` folgt, hat die Nummerierung \R<localize|remark>
    4``. Wenn Sie f�r jeden dieser F�lle einen eigenen Z�hler haben wollen,
    m�ssen Sie das Paket <localize|number-europe>,
    \ <tmpackage|number-europe>, w�hlen.

    <item*|<tmpackage|number-us>><strong|= <localize|number-us>:> Dieses
    Paket kann benutzt werden, um zum amerikanischen Stil der Nummerierung
    zur�ckzukehren, wenn ein von Dritten stammendes Stil-Paket europ�ische
    Nummerierung erzwingt.

    <item*|<tmpackage|structured-list>><strong|= <localize|structured-list>:>
    Das ist ein noch ein Experiment. Normalerweise haben unnummerierte Listen
    keine Argumente und Punkte in Beschreibungen ein Argument. Wenn man das
    Paket <tmpackage|structured-list> verwendet, dann sie k�nnen ein weiteres
    optionales Argument erhalten.

    <item*|<tmpackage|structured-section>><strong|=
    <localize|structured-section>:> Das ist ein noch ein Experiment.
    Normalerweise haben Abschnitte nur den Titel als Argument. Wenn man
    <tmpackage|structured-section> verwendet, k�nnen sie ein weiteres
    Argument annehmen. Au�erdem kann das Konstrukt <markup|rsection> rekursiv
    verwendet werden.

    <item*|<tmpackage|varsession>><strong|= <localize|varsession>:> Dieses
    Paket dient dazu, interaktive Sitzungen, bei denen <TeXmacs> als
    Schnittstelle und Oberfl�che f�r andere Programme dient, anders
    darzustellen. Die Darstellung ist f�r interaktive Sitzungen geeignet,
    aber weniger gut f�r den Druck.
  </description>

  Zus�tzlich zu den genannten Paketen und den vielen Paketen f�r den internen
  <TeXmacs>-Gebrauch, gibt es in <TeXmacs> ein paar pers�nliche
  Beispiel-Pakete: <tmpackage|allouche>, <tmpackage|bpr> und <tmpackage|vdh>
  sowie verschiedene Pakete Stil-Pakete zur Benutzung mit externen Plug-Ins
  (<tmpackage|axiom>, <tmpackage|giaca>, <tmpackage|macaulay2>,
  <abbr|<localize|etc.>>).

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
    <associate|preamble|false>
  </collection>
</initial>