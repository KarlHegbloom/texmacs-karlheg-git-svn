<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Definition neuer Kontexte>

  Die <tmdtd|env-base> <abbr|D.T.D.> enth�lt Konstrukte, die man benutzen
  kann, um neue nummerierte Kontexte, wie z.B. <localize|theorem>,
  <localize|remark>, <localize|exercise> und <localize|figure>:

  <\explain|<explain-macro|new-theorem|env-name|display-name>>
    Dieses Meta-Makro wird zur Definition neuer nummerierter
    (Theorem-�hnlicher) Kontexte benutzt. Das erste Argument
    <src-arg|env-name> spezifiziert den Namen des Kontextes z.B.
    \RExperimente'' und <src-arg|display-name> den dazugeh�rigen Text z.B.
    \RVersuch''. Wenn ein Theorem-�hnlicher Kontext definiert wird, z.B.
    <markup|Experimente>, wird gleichzeitig eine nicht nummerierte Variante
    <markup|Experimente*> automatisch erzeugt.
  </explain>

  <\explain|<explain-macro|new-remark|env-name|display-name>>
    �hnlich <markup|new-theorem>, aber f�r <localize|remark>.
  </explain>

  <\explain|<explain-macro|new-exercise|env-name|display-name>>
    �hnlich <markup|new-theorem>, aber f�r <localize|exercise>.
  </explain>

  <\explain|<explain-macro|new-figure|env-name|display-name>>
    �hnlich <markup|new-theorem>, aber f�r <localize|figure>. Wenn man einen
    neuen Abbildungs-Typ definiert, z.B. \RGem�lde'', dann erzeugt das
    <markup|new-figure>-Makro gleichzeitig einen Zeilen-Kontext
    <markup|small-picture> und einen Block-Kontext <markup|big-picture>,
    sowie die nicht nummerierten Varianten <markup|small-picture*> und
    <markup|big-picture*>.
  </explain>

  Die nummerierten (Theorem-�hnlichen) Kontexte geh�ren alle zu der
  Haupt-Gruppe <verbatim|theorem-env>. Voreinstellung ist die amerikanische
  Art der Nummerierung: ein gemeinsamer Z�hler f�r alle Kontexte. Wenn man
  aber das Paket <tmpackage|number-europe>, <localize|number-europe>, w�hlen,
  hat jeder Kontext einen eigenen Z�hler. Jede <localize|exercise> und jede
  <localize|figure> benutzt eine eigene Z�hlergruppe.

  Allgemein gilt, dass die <verbatim|std-env> Z�hlergruppe die Z�hler f�r
  alle <TeXmacs>-Standard-Kontexte umgruppiert. Typischerweise werden alle
  Gruppen auf �hnliche Weise mit einem Pr�fix, z.B. der Kapitel-Nummer,
  versehen. Abbildung <reference|std-env-fig> zeigt die hierarchische
  Struktur dieser Z�hlergruppe.

  <\big-figure|<tree|<verbatim|std-env>|<tree|<verbatim|theorem-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|theorem>>>|<row|<cell|<verbatim|proposition>>>|<row|<cell|<verbatim|remark>>>|<row|<cell|<with|mode|math|\<vdots\>>>>>>>>|<tree|<verbatim|exercise-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|exercise>>>|<row|<cell|<verbatim|problem>>>>>>>|<tree|<verbatim|figure-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|figure>>>|<row|<cell|<verbatim|table>>>>>>>|<verbatim|equation>|<verbatim|footnote>>>
    <label|std-env-fig>Organisation der Z�hler von
    <TeXmacs>-Standard-Kontexten.
  </big-figure>

  Zus�tzlich zu den Standard-Typen, <localize|theorem>-�hnliche,
  <localize|remark>-�hnliche, <localize|exercise>-�hnliche und
  <localize|figure>-�hnliche Kontexte, k�nnen weitere nummerierte
  Text-Kontexte mit dem <markup|new-env>-Makro erzeugt werden. Diese k�nnen
  auf beliebigen Z�hlergruppen basieren:

  <\explain|<explain-macro|new-env|group|env|env-name|display-name>>
    Das erste Argument ist der Name der Z�hlergruppe <src-arg|group>, zu der
    der neue Kontext geh�ren soll. Das zweite Argument <src-arg|env> ist der
    Name eines bin�ren Makros zur Darstellung des Kontexts. Die Argumente des
    darstellenden Makros sind: ein Name (z.B. \RTheorem 3.14'') und sein
    Rumpf. Die verbleibenden Argumente entsprechen denjenigen von
    <markup|new-theorem>. Beispielsweise wird in den Standard-Basis-Stilen
    <markup|new-theorem> so

    <\tm-fragment>
      <\inactive*>
        <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem>>>
      </inactive*>
    </tm-fragment>
  </explain>

  definiert.

  Es sei daran erinnert, dass man neue Z�hler und Z�hlergruppen zu
  <verbatim|theorem-env> mit Hilfe von <markup|new-counter-group> und
  <markup|add-to-counter-group>, wie das in dem Abschnitt �ber Z�hler erkl�rt
  wurde.\ 

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