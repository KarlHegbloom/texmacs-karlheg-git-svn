<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Sonstige Konstrukte>

  <\explain>
    <explain-macro|group|content><explain-synopsis|Atom>
  <|explain>
    Setze <src-arg|content>, der Zeileninhalt sein muss, als Atom, d.h. als
    Einheit, die nicht getrennt wird und an deren Grenzen auch keine
    speziellen Operationen vorgenommen werden.
  </explain>

  <\explain>
    <explain-macro|float|type|where|body><explain-synopsis|bewegliche
    Einf�gungen>
  <|explain>
    Bewegliche Einf�gungen sind Seiten-Elemente, die keinen festen Ort haben.
    Sie bestehen aus zwei Boxen. Die Ankerbox markiert die Ausgangsposition
    im Text und die bewegliche Box enth�lt den Rumpfschriftsatz,
    <src-arg|body>. Dieses Konstrukt wird f�r Fu�noten und bewegliche Bl�cke
    benutzt.

    Die beiden ersten Argument werden evaluiert. In den Beispielen werden
    aber zur Vereinfachung Zeichenketten verwendet. <src-arg|body> kann
    Blockinhalt sein, selbst wenn das <markup|float>-Konstrukt im
    Zeilen-kontext liegt.

    <\indent>
      <explain-macro|float|<src-value|footnote>||body> f�gt eine Fu�note ein.
      Dies sollte aber nur mit dem <markup|footnote>-Makro benutzt werden und
      wird als Stil-Definition betrachtet. Die Fu�note wird an das Ende der
      Seite gesetzt, die die Ankerbox enth�lt.

      <explain-macro|float|<src-value|float>|where|body> erzeugt einen
      beweglichen Block, dies wird als normales Konstrukt angesehen. Die
      Position der beweglichen Box wird von Seitenumbruch-Algorithmus
      zugewiesen, der die durch die Beweglichkeit erzeugten Freiheitsgrade
      zur Minimierung der Seitenumbruchstrafe ben�tzt.

      <src-arg|where> muss zu einer Zeichenkette evaluieren, die folgende
      Zeichen enthalten kann:

      <\description>
        <item*|t>Gestatte eine Position der Box
        <em|<translate|top|english|german>>.

        <item*|b>Gestatte eine Position der Box
        <em|<translate|bottom|english|german>>.

        <item*|h>Gestatte eine Position der Box \R<em|hier>'', inmitten der
        Seite nahe zur Ankerbox.

        <item*|f>Erzwinge eine Position der Box auf der selben Seite wie die
        Ankerbox.
      </description>
    </indent>
  </explain>

  <\explain>
    <explain-macro|specific|medium|body><explain-synopsis|medium-specific
    content>
  <|explain>
    Dieses Konstrukt sorgt daf�r, dass <src-arg|body> nur �ber ein bestimmtes
    \R<src-arg|medium>'' ausgegeben werden kann. Die folgenden Werte von
    <src-arg|medium> werden unterst�tzt:

    <\description>
      <item*|texmacs><src-arg|body> wird als normaler Zeilen-Inhalte gesetzt.

      <item*|latex><src-arg|body> muss eine Zeichenkette sein. Diese ist
      nicht sichtbar unter <TeXmacs>, wird aber \Rw�rtlich`` �bernommen, wenn
      das Dokument nach <LaTeX> exportiert wird.

      <item*|html>Analog zu <verbatim|latex>, aber f�r Export nach
      <name|HTML>.

      <item*|screen><src-arg|body> wird nur auf dem Bildschirm sichtbar. Es
      kann w�hrend der Erstellung und �nderung von Dokumenten sehr n�tzlich
      sein, Kommentare anzubringen, die beim Druck verschwunden sind. Das
      Konstrukt <markup|flag> kann �hnlich verwendet werden.

      <item*|printer>Dies ist komplement�r zu <verbatim|screen>,
      <src-arg|body> wird gedruckt, aber nicht auf dem Bildschirm angezeigt.
    </description>
  </explain>

  <\explain|<explain-macro|raw-data|data><explain-synopsis|gesch�tzte Daten>>
    In bestimmten Kontexten muss man Daten, meist Bin�rdaten, vor
    Ver�nderungen gesch�tzt, einf�gen. Das <markup|raw-data>-Konstrukt
    verhindert Ver�nderungen im Editor.
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