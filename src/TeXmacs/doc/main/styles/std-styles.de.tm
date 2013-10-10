<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|<TeXmacs>-Standard-Basis-Stile>

  Die wichtigsten Stile von <TeXmacs> sind folgende:

  <\explain|<tmstyle|generic>>
    <strong|= <localize|generic>:> Dieser Basis-Stil ist die Vorgabe, wenn
    Sie ein neues Dokument erzeugen. Der Sinn dieses Stils ist es, einfach
    simple Dokumente zu schreiben. Darum gibt es keine Nummerierung von
    Abschnitten. Die Abs�tze werden durch einen zus�tzlichen vertikalen
    Leerraum gekennzeichnet, nicht durch Einr�ckungen.
  </explain>

  <\explain|<tmstyle|article>>
    <strong|= <localize|article>:> Dieser Basis-Stil dient zum Schreiben
    kurzer wissenschaftlicher Ver�ffentlichungen, die in Abschnitte
    gegliedert sind. Die Nummerierung von nummerierten Kontexten, wie
    \ <localize|theorem>, <localize|remark>, <active*|<abbr|<localize|etc>.>>
    ist fortlaufend im ganzen Dokument. Wenn man das Paket
    \ <tmpackage|number-long-article> (= <localize|number-long-article>)
    benutzt, dann erh�lt die fortlaufende Nummer als Pr�fix die
    Abschnitts-Nummer.
  </explain>

  <\explain|<tmstyle|book>>
    <strong|= <localize|book>:> Dies ist der Basis-Stil zum Schreiben von
    B�chern. B�cher sind gegliedert in Kapitel. Der Nummerierung in den
    einzelnen Kapiteln wird die Kapitel-Nummer als Pr�fix vorangestellt. Im
    allgemeinen ist es besser, jedes Kapitel in einer eigenen Datei
    abzuspeichern. Die Editierung wird so viel effizienter. Die damit
    zusammenh�ngenden Fragen werden im Abschnitt <hlink|B�cher, aus mehreren
    Dateien bestehende Dokumente|../links/man-multifile.de.tm> ausf�hrlicher
    behandelt.
  </explain>

  <\explain|<tmstyle|seminar>>
    <strong|= <localize|seminar>:> Dokumente, die auf diesem Stil basieren,
    dazu gedacht, auf Folien ausgedruckt und mit dem Overhead-Projektor
    pr�sentiert zu werden. Es kann auch sein, dass Sie sie direkt vom Laptop
    aus, mit dem Beamer projizieren m�chten, indem Sie
    <menu|View|Presentation mode> w�hlen. Beachten Sie aber, dass Folien
    realen Seiten entsprechen, w�hrend man im Pr�sentationsmodus eher
    <em|Schalter> verwenden sollte.
  </explain>

  <\explain|<tmstyle|source>>
    <strong|= <localize|source>:> Das ist der spezielle Stil zum Editieren
    von Stil-Definitionen und -<no-break>Paketen. Er schaltet auf den
    Quellmodus um, damit der Quellcode in einer Weise dargestellt wird, der
    die Struktur erkennbar macht. Genaueres findet man im Abschnitt
    <hlink|Darstellung von Basis-Stil-Dateien und
    Paketen|../../devel/style/presentation/src-present.de.tm>.
  </explain>

  Der Basis-Stil <tmstyle|article> (<localize|article>) besitzt verschiedene
  Varianten, um der unterschiedlichen Layout-Politik verschiedener Journale
  Rechnung zu tragen. Bisher sind die folgenden Analoga von <LaTeX> -Stilen
  implementiert worden.\ 

  <\explain|<tmstyle|amsart>>
    Basis-Stil der American Mathematical Society. Weitere Informationen
    unter: http://www.ams.org/tex/author-info.html
  </explain>

  <\explain|<tmstyle|acmconf>>
    Basis-Stil der Association for Computing Machinery. Weitere Informationen
    unter: http://www.ams.org/tex/author-info.htmlhttp://www.portal.acm.org
  </explain>

  <\explain|<tmstyle|jsc>>
    Basis-Stil des Journal of Symbolic Computation. Weitere Informationen
    unter: http://www.elsevier.com
  </explain>

  Wir wollen au�erdem Basis-Stile <tmstyle|tmarticle> und <tmstyle|tmbook>
  als Alternativen zu <tmstyle|article> und <tmstyle|book> entwickeln.

  Zus�tzlich zu den Varianten der Basis-Stile <tmstyle|article> und
  <tmstyle|book> liefert <TeXmacs> \ noch einige andere Stil-Varianten:

  <\explain|<tmstyle|letter>>
    <strong|= <localize|letter>:> Dieser Stil basiert auf <tmstyle|generic>,
    besitzt einige zus�tzliche Makros haupts�chlich f�r Briefkopf und
    -Schlu�.
  </explain>

  <\explain|<tmstyle|exam>>
    <strong|= <compound|localize|exam>:> Dieser Basis-Stil basiert auch auf
    <tmstyle|generic>, hat aber zus�tzliche Makros f�r den Kopf und f�r die
    Darstellung von �bungen.
  </explain>

  <\explain|<tmstyle|tmdoc>>
    <strong|= <localize|tmdoc>:> Dieser Stil dient zum Schreiben der
    <TeXmacs>-Dokumentation und liefert eine Anzahl spezieller Makros. Einige
    Aspekte sind noch voll in der Entwicklung.\ 
  </explain>

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