<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <\tmdoc-title>
    Daten-Beschreibungen (D.R.D.)

    (data relation descriptions)
  </tmdoc-title>

  <paragraph*|der sinn von Daten-Beschreibungen, <abbr|D.R.D.>s>

  Einer der Hauptvorteile von <TeXmacs> ist die Verwendung von Baumstrukturen
  als allgemeines Datenformat des Editors. Wie bei <no-break>XML hat dies den
  Vorteil, dass diese Strukturen �bersichtlich und leicht verst�ndlich sind,
  sodass es einfach ist, Dokumente mit den eingebauten Werkzeugen zu �ndern
  und anzupassen. Wenn jedoch der Editor f�r einen ganz bestimmten,
  besonderen Zweck eingesetzt werden soll, dann muss in aller Regel das
  Datenformat auf eine bestimmte Teilmenge der m�glichen B�ume beschr�nkt
  werden.

  In XML benutzt man Data Type Definitions (<abbr|D.T.D.>s), um eine
  spezielle Teilmenge des allgemeinen XML-Formats f�r einen bestimmten Zweck
  zu definieren: f�r Web-Dokumente \RXHTML``, f�r Mathematik \RMathML``, f�r
  2-dimensionale Graphik \RSVG`` usw.. Au�erdem erlaubt XML, solche
  <abbr|D.T.D.>s in einem gewissen Umfang miteinander zu kombinieren.
  Schlie�lich enthalten <abbr|D.T.D.>s. normalerweise ein Referenz-Handbuch.

  In <TeXmacs> sind wir mit der Einf�hrung von <abbr|D.R.D.>s noch einen
  Schritt weiter gegangen. Neben der Entscheidung, ob ein gegebenes Dokument
  syntaktisch korrekt ist, k�nnen wir damit auch formal bestimmte
  Eigenschaften des Dokuments beschreiben. Beispielsweise k�nnen
  Textfragmente erreichbar (<em|accessible>) sein oder eben nicht. So ist
  typischerweise der Z�hler in einem Bruch erreichbar und kann daher
  jederzeit im Editor ge�ndert werden, w�hrend beispielsweise die URL eines
  Hyperlinks nicht erreichbar ist und daher erst nach Aufhebung des Schutzes
  ver�nderbar wird. Ganz �hnlich implizieren bestimmte Konstrukte wie z.B.
  <markup|itemize> Blockinhalte, w�hrend andere wie z.B. <markup|sqrt>
  Zeileninhalt impliziert. Schlie�lich verhalten sich bestimmte verwandte
  Konstrukte wie <markup|chapter>, <markup|section>, <markup|subsection>,
  <abbr|usw.> bei bestimmten Operationen wie z.B. Konversionen gleichartig.

  Eine Daten-Beschreibung (<abbr|D.R.D.>) besteht aus einer \RData Type
  Definition`` und zus�tzlichen logischen Eigenschaften von Marken,
  Konstrukten und Textfragmenten. Diese logischen Eigenschaften werden in
  sogenannten <em|Horn clauses> definiert, die auch in anderen
  Programmiersprachen wie z.B. Prolog Verwendung finden. Es sollte damit
  relativ leicht sein, die Eigenschaften von Marken, Konstrukten und
  Textfragmenten zu bestimmen, so dass bestimmte Datenbanktechniken zur
  effektiven Implementierung genutzt werden k�nnen. Momentan haben wir damit
  gerade erst begonnen und benutzen zur Zeit noch eine Menge C++ Code im
  Widerspruch zu dem, was wir oben beschrieben haben. Daher k�nnen wir eine
  bessere formale Beschreibung von <abbr|D.R.D.>s jetzt noch nicht liefern.

  Einer der wichtigsten Vorteile von <abbr|D.R.D.>s ist, dass anders als in
  objektorientierten Sprachen keine festen hierarchischen Strukturen
  vorgegeben werden m�ssen. Das ist deshalb so n�tzlich, weil Eigenschaften
  wie Ver�nderbarkeit (accessability), Blockinhalt, Zeileninhalt von einander
  unabh�ngig sind. Es ist in der Tat so, dass <abbr|D.T.D.>s meist zur
  Beschreibung passiver Dokumente ausreichend sind, dass aber, wenn es darum
  geht, Dokumente interaktiv zu editieren, eine feinere Beschreibung von
  Eigenschaften g�nstiger ist.

  <paragraph*|Derzeitige Eigenschaften und Anwendungen von <abbr|D.R.D.>s>

  Derzeit enth�lt die <abbr|D.R.D.> eines Dokuments die folgenden
  Informationen:

  <\itemize>
    <item>Die erlaubten Anzahlen von Variablen (arity).

    <item>Die Erreichbarkeit (accessability) eines Konstrukts und seiner
    Kinder.
  </itemize>

  In n�chster Zukunft sollen die folgenden Eigenschaften eingef�hrt werden:

  <\itemize>
    <item>Ist_Zeileninhalt (inline-ness) eines Konstrukts und seiner Kinder.

    <item>Ist_Tabelle (tabular-ness) eines Konstrukts und seiner Kinder.\ 

    <item>Zweck eines Konstrukts und seiner Kinder.
  </itemize>

  Diese Informationen werden neben anderen f�r folgende Aktionen verwandt:

  <\itemize>
    <item>Nat�rliches Vorgabeverhalten, wenn Konstrukte oder ihre Kinder
    erzeugt oder vernichtet werden, z.B. automatische Erg�nzung von fehlenden
    Argumenten und/oder L�schung von Konstrukten mit zu geringer
    Argumentanzahl.\ 

    <item>Beschr�nke Suchoperationen auf erreichbare Knoten z.B. bei der
    Rechtschreibpr�fung.

    <item>Automatische Einf�gung der Konstrukte <markup|document> bzw.
    <markup|table>, wenn Bl�cke oder Tabellen erzeugt werden.

    <item>Syntaktische Hervorhebungen im Quellmodus in Abh�ngigkeit von dem
    Zweck des Konstrukts und seiner Argumente.
  </itemize>

  <paragraph*|Erzeugung der <abbr|Daten-Beschreibung (D.R.D.>) eines
  Dokuments>

  <TeXmacs> erzeugt f�r jedes Dokument eine eigene Daten-Beschreibung,
  <abbr|D.R.D.>. Diese wird in zwei Stufen erzeugt. Zun�chst versucht
  <TeXmacs> heuristisch die Eigenschaften von Konstrukten, die der Benutzer
  definiert hat, und die Konstrukte von assoziierten Stildefinitionen
  auszuwerten. Ist zum Beispiel folgendes Makro\ 

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|name|Hello <arg|name>!>>>
  </tm-fragment>

  vorhanden, dann erkennt <TeXmacs> automatisch, dass <markup|hi> ein Makro
  mit einem Argument ist, und nimmt an, dass die zul�ssige Argumentanzahl 1
  ist. Beachten Sie, dass die heuristische Erzeugung der Daten-Beschreibung
  interaktiv ist. Wenn Sie ein Makro in Ihrem Dokument erzeugen, werden seine
  Eigenschaften automatisch in die Daten-Beschreibung aufgenommen, sofern Sie
  <TeXmacs> daf�r etwas Zeit lassen (ca. 1 s).

  Manchmal sind die heuristisch definierten Eigenschaften nicht zutreffend.
  F�r diesen Fall besitzt <TeXmacs> das Konstrukt <markup|drd-props>, mit der
  eine <hyper-link|manuelle Korrektur|../stylesheet/prim-macro.de.tm> dieser
  Eigenschaften durchgef�hrt werden kann.

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
    <associate|preamble|false>
  </collection>
</initial>