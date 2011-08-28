<TeXmacs|1.0.7.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Anmerkungen f�r russische und ukrainische Anwender>

  Um russischen oder ukrainischen Text einzugeben, haben Sie verschiedene
  Optionen:

  <\itemize>
    <item>W�hlen Sie Russisch als Ihre Sprache mit dem Men�befehl
    <menu|Edit|Preferences|Language|Russian>. Wenn \ <TeXmacs> mit russischen
    Men�s startete, dann wurde bereits automatisch Russisch oder Ukrainisch
    vorgegeben.

    <item>W�hlen Sie Russisch bzw. Ukrainisch als Sprache f�r das ganze
    Dokument mit dem Men�befehl <menu|Document|Language|Russian> bzw.
    <menu|Document|Language|Ukrainian>.

    <item>W�hlen Sie Russisch bzw. Ukrainisch als Sprache f�r ein Textst�ck
    \ mit dem Men�befehl <menu|Format|Language|Russian> bzw.
    <menu|Format|Language|Ukrainian>.
  </itemize>

  Wenn Ihr X-Server die xkb-Erweiterung nutzt und zwischen \Rlateinischen''
  und kyrillischen Tastatur-Moden umschalten kann, dann brauchen Sie nichts
  besonderes zu tun. Schalten Sie einfach auf den kyrillischen Tastatur-Modus
  um. Die Software, die man daf�r braucht ist in allen modernen
  Linux-Distributionen vorhanden und normalerweise ist die xkb-Erweiterungen
  <with|font-family|tt|XF86Config> standardm��ig eingestellt. Die Tastatur
  wird mit <with|font-family|tt|setxkbmap> konfiguriert. Wenn X startet f�hrt
  es diesen Befehl mit der systemweiten Xkbmap-Datei aus. Sie befindet sich
  normalerweise in <with|font-family|tt|/etc/X11/xinit>. Anschlie�end wird
  <with|font-family|tt|setxkbmap> mit \ <with|font-family|tt|~/.Xkbmap>
  ausgef�hrt, sofern diese existiert. Eine typische
  <with|font-family|tt|~/.Xkbmap> kann so aussehen

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Das bedeutet, dass der Tastatur-Modus mit <render-key|l-shift r-shift>
  umgeschaltet wird. Andere h�ufig benutzte Alternativen sind <prefix|C-S->
  oder <key|C- A->. Sehen sie in <with|font-family|tt|/usr/X11R6/lib/X11/xkb/>
  f�r weitere Einzelheiten. Wenn Sie h�ufiger russische Texte schreiben
  wollen, dann ist das die bevorzugte Methode auf modernen Linux-Systemen.\ 

  Auf �lteren Linux-Systemen ist die xkb-Erweiterung oft inaktiviert. Sie
  werden mit <with|font-family|tt|xmodmap> konfiguriert. Wenn X startet f�hrt
  es den Befehl mit der systemweiten Xmodmap-Datei aus. Sie befindet sich
  normalerweise in <with|font-family|tt|/etc/X11/xinit>. Anschlie�end wird
  mit <with|font-family|tt|xmodmap> mit <with|font-family|tt|~/.Xmodmap>
  ausgef�hrt, sofern diese existiert. Sie k�nnen die Tastenkombination f�r
  den Tastatur-Modus mit einem 1-Byte Code (z.B. koi8-r) im Kyrillisch-Modus
  konfigurieren. Einfacher ist es das Paket <with|font-family|tt|xruskb>
  herunterzuladen und zu Beginn der X-Session, den Befehl

  <verbatim| \ \ \ xrus jcuken-koi8>

  zu benutzen. das setzt das Tastatur-Layout auf jcuken (siehe unten) und den
  Tastatur-Code auf koi8-r f�r den kyrillischen Modus. Au�erdem sollten Sie
  die Option \ <menu|Edit|Preferences|Keyboard|Cyrillic input method|Koi8-r>
  einstellen.

  Man kann auch die Windows cp1251 Codierung verwenden anstelle von koi8-r,
  obwohl das unter UNIX nur selten geschieht. Wenn Sie
  <with|font-family|tt|xrus jcuken-cp1251> benutzen wollen, w�hlen Sie
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Cp1251>.\ 

  Alle diese Methoden ben�tigen zus�tzliche Aktionen, um eine kyrillische
  Tastatur zu erhalten. Das ist nicht schwer. Lesen Sie das Cyrillic-HOWTO
  oder die neue Version

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Au�erdem beeinflussen sie alle X-Anwendungen: Text-Editoren, wie emacs,
  nedit, kedit usw., xterms, <TeXmacs> usw..

  Wenn Sie kyrillische Schriftzeichen nur einmal bzw. sehr selten ben�tigen,
  kann dieser ganze Aufwand die Sache nicht wert sein. Darum hat <TeXmacs>
  noch eine weitere M�glichkeit, kyrillische Buchstaben einzugeben und die
  ohne jegliche Vorbereitung. Nat�rlich beeinflusst diese Methode
  ausschlie�lich <TeXmacs> und keine andere Anwendung. Dazu w�hlen Sie auf
  einer Standard US-Stil-Tastatur den Men�befehl
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Translit>. Au�erdem
  m�ssen Sie f�r Ihr Textst�ck die richtige Schriftart w�hlen:
  <menu|Format|Font-name|Foreign|Cyrillic>. Die Eingabe eines Buchstabens
  wird dann einen �hnlichen kyrillischen Buchstaben erzeugen. F�r einige
  Buchstaben m�ssen Sie Zwei- oder Drei-Buchstaben-Kombinationen
  verwenden:<vspace|0.5fn>

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Eingabe>|<cell|f�r>|<cell|Eingabe(n)>|<cell|f�r>>|<row|<cell|<key|text
  " e>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|j
  var>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|J
  var>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|e
  var>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|E
  var>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|�>>>>>>|Kyrillische
  Buchstaben mit \Rlateinischer`` Tastatur.>

  Die Verwendung von Mehrfach-Kombinationen f�hrt manchmal zu unerw�nschten
  Ergebnissen. Versuchen Sie es dann mit <key|/> als Trennzeichen. Wenn Sie
  <key|s h> eingeben, erhalten Sie \R<with|language|russian|font|cyrillic|�>'';
  um das gew�nschte \R<with|language|russian|font|cyrillic|��>'' zu erhalten,
  m�ssen Sie \ <key|s / h> eingeben. Es gibt keine eindeutige Weise
  lateinische Buchstaben kyrillischen Zuzuordnen. Schauen Sie sich deshalb
  die mitgelieferten Tastatur-Dateien an und, wenn Sie ihnen nicht gefallen,
  passen Sie sie in Ihrer Initialisierungs-Datei
  <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm> Ihren W�nschen
  entsprechend an.

  Wenn Sie \Rjcuken`` anstelle von \Rtranslit`` w�hlen, erhalten Sie das
  offizielle russische Tastatur-Layout. Die Tasten der oberen Reihe
  \Rqwerty'' erzeugen dann \R<with|language|russian|<with|font|cyrillic|������>>``.
  Diese Methode ist dann n�tzlich, wenn Sie eine russische Tastatur mit der
  richtigen Beschriftung besitzen oder eine andere passend beschriften.
  Vielleicht k�nnen Sie ja auch kyrillisch Blindschreiben.

  Diejenigen, die keine russische Tastatur besitzen, bevorzugen oft das
  \Ryawerty``-Layout, bei dem die Tasten \Rqwerty'' die Ausgabe
  \R<with|language|russian|font|cyrillic|������>`` und zus�tzliche
  kyrillische Schriftzeichen mit der Umschalt-Taste, <prefix|S->, erzeugt
  werden k�nnen. <TeXmacs> hat ein etwas modifizierte \Ryawerty\R-Layout,
  denn es �ndert nicht die Tasten <key|$>, <render-key|�>, <key|\\>, da diese
  f�r <TeXmacs> wichtig sind sind und eine spezielle Bedeutung haben. Die
  dazugeh�rigen kyrillischen Buchstaben k�nnen durch bestimmte Kombinationen
  mit der Umschalt-Taste erzeugt werden.

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
    <associate|preamble|false>
  </collection>
</initial>