<TeXmacs|1.0.4.5>

<style|<tuple|tmdoc|>>

<\body>
  <tmdoc-title|Ein einfaches Stil-Paket schreiben>

  Lassen Sie uns an einem einfachen Beispiel erkl�ren, wie man ein einfaches
  Stil-Paket schreibt.

  Wenn Sie das Beispiel direkt am Rechner nachvollziehen wollen, erleichtern
  Sie sich die Arbeit, wenn Sie ein zweites <TeXmacs> parallel eventuell in
  einem anderen virtuellen Fenster starten und zwischen den beiden Instanzen
  wechseln.

  Zuerst einmal m�ssen Sie einen neuen Puffer, d.h. eine neue leere
  Textdatei, erzeugen. Dazu w�hlen Sie das Men� \ <menu|Datei|Neu> und w�hlen
  den <tmstyle|Quellcode> Basis-Stil unter
  <menu|Dokument|Basis-Stil|Quellcode>-Men�. Dann speichern Sie mit einem
  aussagekr�ftigen Namen und der Datei-Erg�nzung <verbatim|.ts> in Ihr
  Paket-Verzeichnis:

  <verbatim| \ \ \ $HOME/.TeXmacs/packages>

  Beachten Sie bitte, dass der Knopf <em|<menu|Texte>> im Datei-Browser dem
  Verzeichnis\ 

  <verbatim| \ \ \ $HOME/.TeXmacs/texts>

  entspricht. Daher k�nnen Sie durch Doppelklick auf \ .. und danach auf
  <verbatim|packages> schnell in dieses Verzeichnis wechseln. Ganz
  entsprechend enth�lt das Verzeichnis

  <verbatim| \ \ \ $HOME/.TeXmacs/styles>

  Ihre pers�nlichen Basis-Stil-Dateien. Nach dem Sichern mit der
  Dateierg�nzung <verbatim|.ts> sollte das leere Stil-Paket automatisch in
  dem Men� <menu|Dokument|Paket einf�gen > erscheinen. Wenn Sie ein
  Unterverzeichnis im Verzeichnis <verbatim|$HOME/.TeXmacs/packages>
  erstellen, erzeugen Sie automatisch ein neues Untermen� und wenn Sie da
  hinein ein Stilpaket speichern einen neuen Men�punkt in dem entsprechenden
  Untermen�.

  Lassen Sie uns nun ein einfaches Makro <markup|hi> erzeugen, das \RHello
  world'' auf dem Bildschirm ausgibt. Zuerst tippen Sie <key|A-=> oder
  <key|M-i =>, um eine Zuordnung, engl. \Rassignment'', zu erzeugen. Sie
  sollten nun auf dem Bildschirm Folgendes sehen

  <\tm-fragment>
    <inactive*|<assign||>>
  </tm-fragment>

  Geben Sie nun ``hi'' als erstes Argument ein, gehen zum zweiten Argument
  und tippen <key|A-m> oder <key|M-i m> um ein Makro einzuf�gen. Jetzt sollte
  es so aussehen:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|>>>
  </tm-fragment>

  Schlie�lich schreiben Sie \RHello world'' in den Rumpf des Makros. Ihr
  Dokument sollte nun aus folgender Zeile bestehen:

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world>>>
  </tm-fragment>

  Nachdem Sie Ihr Stil-Paket unter einem Namen gespeichert haben, k�nnen Sie
  das Makro verwenden, z.B., indem Sie ein neues Dokument erstellen und es
  mit Ihrem Stil-Paket mit <menu|Dokument|Paket hinzuf�gen> verbinden. Sie
  benutzen das Makro <markup|hi> durch Eintippen von \ <key|\\ h i> mit
  nachfolgendem Dr�cken der Eingabetaste, <key|<key-return>>.

  Analog k�nnen Sie Makros mit Argumenten erzeugen, die Sie zur Laufzeit
  eingeben und im Makro auswerten k�nnen. Wenn Sie z.B. in gleicher Weise ein
  Makro <markup|hello> erzeugt haben, k�nnen Sie mit der Tastenkombination
  <key|A-links> oder <key|M-i links> im Makrorumpf ein zus�tzliches Argument
  auf der linken Seite des Cursors einf�gen. \Rlinks`` steht dabei f�r die
  linke Pfeiltaste. Nachdem Sie mit dem Cursor im Makrorumpf <key|A-links>
  oder <key|M-i links> eingetippt haben, geben Sie dem Argument einen Namen,
  z.B. \Rname'', um anschlie�end darauf zugreifen zu k�nnen. Sie sollten nun
  Folgendes sehen:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|>>>
  </tm-fragment>

  In die zweite Argumentposition des Makrorumpfes tippen Sie nun Ihren Text
  z.B. \RHallo``, dann um das mit dem Namen \Rname`` bezeichnete erste
  Argument einzusetzen, dr�cken Sie die Kombinationen \ <key|A-#> oder
  <key|M-i #> tippen dann schlie�lich \Rname'', dr�cken <key|rechts>, das ist
  die rechte Pfeiltaste \ und geben weiter Text ein z.B. \R, wie geht es
  Ihnen?''. Das sieht dann so aus:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hallo <arg|name>, wie geht es
    Ihnen?>>>
  </tm-fragment>

  Die Kurzbefehlkombination <key|A-#> bzw. <key|M-i #> wird zum Zugriff auf
  das Makroargument, hier \ <src-arg|name>, verwendet. Anstatt \ <key|A-#>
  bzw. <key|M-i #> zu benutzen, dann \Rname'' and <key|<key-right>>
  einzutippen, k�nnen Sie auch die <key|\\>-Taste benutzen und \ <key|\\ n a
  m e> gefolgt von der Eingabetaste <key|<key-return>> eintippen. Nachdem Sie
  Ihr Stil-Paket gesichert haben, k�nnen Sie Ihr neues Makro in jedem
  Dokument, dem Sie dieses Paket zugef�gt haben, benutzen, indem Sie
  \ <key|\\ h e l l o> eingeben und die <key|return>-Taste bet�tigen.

  Intern werden alle Makrodefinitionen in der \R<TeXmacs>
  typesetter``-Umgebung gespeichert. Daneben werden dort auch normale
  Kontextvariablen wie Absatzz�hler (section counters) oder Schriftgr��e
  (font size) abgelegt. Die Kontextvariablen k�nnen global mit dem
  <markup|assign>-Konstrukt oder lokal mit dem <markup|with>-Konstrukt
  gesetzt werden. Wenn z.B. die folgenden Zeile\ 

  <\tm-fragment>
    <inactive*|<assign|section-nr|-1>>
  </tm-fragment>

  in Ihrem Paket enthalten ist und Sie als Basis-Stil <tmstyle|Artikel>
  verwenden, dann erh�lt der erste Abschnitt die Abschnittnummer
  <no-break><with|mode|math|0>.

  Die folgende Variante

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello
    <with|font-shape|small-caps|<arg|name>>!>>>
  </tm-fragment>

  <markup|hello>-Makros bringt den Namen in \ in
  <with|font-shape|small-caps|kleinen gro�buchstaben> auf den Bildschirm.
  Beachten Sie, da� Sie mit dem <markup|with>-Konstrukt auch ein Makro lokal
  umdefinieren k�nnen. Dies wird beispielsweise in den Standardumgebungen f�r
  Listen benutzt, wo das Makro, das die Listensymbole liefert innerhalb des
  Listenrumpfes modifiziert wird.

  Eine weitere Variante des <markup|hello>-Makros benutzt das
  <markup|person>-Makro des Standard-Stils:

  <\tm-fragment>
    <inactive*|<assign|hello|<macro|name|Hello <person|<arg|name>>!>>>
  </tm-fragment>

  Um in die Makrodefinition <inactive*|<person|<arg|name>>> einzuf�gen,
  m�ssen Sie zuerst an seiner Stelle ein Leerkonstrukt (compound) erzeugen.
  Dazu benutzen Sie <key|A-c> oder <key|M-i c>, tippen dann \Rperson'', f�gen
  ein Argument mit \ <key|A-rechts> oder <key|M-i rechts> hinzu, und tippen
  schlie�lich den Namen des Arguments <src-arg|name>. Schhlie�lich dr�cken
  Sie \ <key|<key-return>>, um das <with|color|blue|<translate|compound|english|german>>
  in ein <markup|person>-Makro umzuwandeln. Alternativ k�nnen Sie <key|\\>,
  ``person'', <key|A-rechts> und \Rname'' tippen.

  Durch Kombination der vorgehend beschriebenen Vorgehensweisen sollte der
  Durchschnittsanwender bereits Stil-Pakete f�r alle h�ufig vorkommenden
  Anwendungsf�lle zu schreiben k�nnen.\ 

  Ein interessane Technik, mit der sich Makros schreiben lassen, die
  komplizierte mathematische Formeln enthalten, die wiederum von variablen
  Formeln abh�ngen, ist die Folgende:\ 

  <\enumerate>
    <item>Schreibe die Formel z.B. <with|mode|math|(a<rsub|1>,\<ldots\>,a<rsub|n>)>
    in ein gew�hnliches Dokument.

    <item>Erzeuge ein Makroskelett in ihrem Stil-Paket:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|>>>
    </tm-fragment>

    <item>Kopiere die Formel und f�ge sie in den Rumpf des Makros ein:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(a<rsub|1>,\<ldots\>,a<rsub|n>)>>>
    </tm-fragment>

    <item>Ersetze die die Variablen, die parametrisiert werden sollen, durch
    Makro-Argumente:

    <\tm-fragment>
      <inactive*|<assign|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>>>
    </tm-fragment>

    <item>Nach dem Speichern k�nnen Sie das neue Makro in Dokumenten
    einsetzen, die Ihr Paket verwenden, z.B.:

    <\equation*>
      <with|n-tuple|<macro|a|(<arg|a><rsub|1>,\<ldots\>,<arg|a><rsub|n>)>|<n-tuple|a>=<n-tuple|b>.>
    </equation*>
  </enumerate>

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
    <associate|language|english>
    <associate|preamble|false>
  </collection>
</initial>