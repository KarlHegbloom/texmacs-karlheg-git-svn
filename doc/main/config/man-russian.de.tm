<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Anmerkungen f�r Anwender aus Russland und der Ukraine>

  Um russischen (und ebenso ukrainischen) Text zu schreiben, haben Sie
  mehrere Optionen:

  <\itemize-dot>
    <item>W�hlen Sie Russisch als Standardsprache unter
    <menu|Edit|Preferences|Language|Russian>. Wenn <TeXmacs> mit russischen
    Men�s startet, wird dies automatisch getan wenn die russische
    Umgebungsvariable gesetzt ist.

    <item>W�hlen Sie Russisch als Sprache f�r ein Dokument mit
    <menu|Document|Language|Russian>.

    <item>W�hlen Sie Russisch als Sprache f�r einen Textabschnitt in einem
    anderssprachigen Dokument �ber <menu|Format|Language|Russian>.
  </itemize-dot>

  Wenn Ihr X-Server die xkb-Erweiterung benutzt und eingestellt ist zwischen
  den lateinischen und russischen Tastatur-Modi zu wechseln, m�ssen Sie
  nichts Weiteres mehr unternehmen. Schalten Sie einfach Ihre Tastatur in den
  russischen Modus um und fahren Sie fort. Alle Software die hierzu ben�tigt
  wird ist in modernen Linux-Distributionen bereits enthalten und die
  xkb-Erweiterung ist standardm��ig aktiviert in der <kbd|XF86Config>. Mit
  der xkb-Erweiterung sind die Keysyms 2 Byte gro� und die russischen
  Buchstaben liegen bei 0x6??. Die Tastatur wird �ber <kbd|setxkbmap>
  konfiguriert. Wenn X startet, verbindet es dieses Kommando mit der
  systemweiten Xkbmap-Datei (normalerweise finden Sie diese unter
  <kbd|/etc/X11/xinit>) wenn Sie existiert, und mit der Benutzerdatei
  <kbd|~/.Xkbmap>, falls diese existiert. Eine typische <kbd|~/.Xkbmap> sieht
  m�glicherweise wie folgt aus:

  <kbd| \ \ \ ru basic grp:shift_toggle>

  Dies bedeutet dass der Tastaturmodus �ber <key|l-shift r-shift>
  umgeschaltet wird. Andere beliebte M�glichkeiten sind <key|strg shift> oder
  <key|strg alt>, sehen Sie nach in <kbd|/usr/X11R6/lib/X11/xkb/> um weitere
  Informationen zu erhalten. Dies ist das bevorzugte Tastatur-Setup f�r
  moderne Linux-Systeme wenn Sie beabsichtigen, oft russisch zu schreiben.

  Auf �lteren Linux-Systemen ist die xkb-Erweiterung des �fteren deaktiviert.
  Die Keysyms sind 1-Byte gro� und werden �ber <kbd|xmodmap> konfiguriert.
  Beim Start von X wird dieses Kommando mit dem systemweiten <kbd|Xmodmap>
  verbunden (diese finden Sie �blicherweise unter <kbd|/etc/X11/xinit>) wenn
  es existiert; danach mit der Benutzerdatei <kbd|/.Xmodmap> falls diese
  ebenso existiert. Man kann eine Tastaturkombination einrichten um den Modus
  zu wechseln und eine 1-Byte russische Kodierung (wie z.B. koi8-r) im
  russischen Modus zu benutzen. Einfacher geht es wenn Sie das Paket
  <kbd|xruskb> herunterladen und beim Start der X-Session

  <\kbd>
    \ \ \ xrus jcuken-koi8
  </kbd>

  ausf�hren. Dies setzt das Tastaturlayout auf jcuken (siehe unten) und die
  Kodierung auf koi8-r f�r den russischen Modus. Wenn Sie solch ein
  Tastaturlayout benutzen, sollten Sie die Option
  <with|mode|math|\<rightarrow\>> international keyboard
  <with|mode|math|\<rightarrow\>> russian <with|mode|math|\<rightarrow\>>
  koi8-r setzen.

  Es ist au�erdem noch m�glich, die Windows cp1251 Kodierung statt koi8-r zu
  benutzen, was aber unter <name|Unix> eher un�blich ist. Wenn Sie <kbd|xrus
  jcuken-cp1251> nutzen, w�hlen Sie cp1251 statt koi8-r.

  Alle oben beschriebenen Vorgehensweisen erfordern spezielle Aktionen um die
  Tastatur zur russifizieren``. Das ist alles nicht kompliziert, schauen Sie
  sich das Cyrillic-HOWTO an, oder besser noch seine aktualisierte Version

  <\kbd>
    http://www.inp.nsk.su/~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html
  </kbd>

  Alle hier beschrieben Vorgehensweisen wirken sich auf alle X-Anwendungen
  aus: Texteditoren (emacs, nedit, kedit,...), xterms, <TeXmacs> usw..

  Wenn Sie nur einmal oder sehr selten russisch Schreiben m�chten, erfordert
  ein komplettes Tastatur-Setup mehr Arbeit als die Sache letztendlich Wert
  ist. Als Vereinfachung f�r solche Gelegenheitsnutzer bietet <TeXmacs>
  einige Methoden f�r die Eingabe in Russisch die keine vorhergehende Arbeit
  ben�tigen. Diese Methoden beeinflussen ausschlie�liche <TeXmacs> und keine
  anderen Anwendungen.

  Der einfachste Weg um russisch auf einer Standardtastatur ohne
  Software-Setup zu schreiben f�hrt �ber die Option
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|translit>. Danach
  wird bei Eingabe eines lateinischen Buchstaben der �hnlichste russische
  Buchstabe erzeugt. Um bestimmte russische Buchstaben zu bekommen, muss man
  2- oder 3-Buchstabenkombinationen benutzen:

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Tastenkombination>|<cell|f�r>|<cell|Tastenkombination>|<cell|f�r>>|<row|<cell|<kbd-text|"
  e>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<kbd-text|"
  E>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|j
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|J
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|e
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|E
  <key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|�>>>>>>|Kyrillischen Text
  auf einer lateinischen Tastatur schreiben>

  Wenn Sie beispielsweise <with|language|russian|font|cyrillic|��>``, und
  nicht <with|language|russian|font|cyrillic|�>`` m�chten, m�ssen Sie <key|s
  / h> tippen. Selbstverst�ndlich ist die optimale`` Verbindung zwischen
  lateinischen und russischen Buchstaben nicht allgemein g�ltig. Sie k�nnen
  das von <TeXmacs> mitgelieferte Schema untersuchen und, falls Ihnen etwas
  nicht zu sagt, es in der Datei <kbd|~/TeXmacs/progs/my-init-texmacs.scm>
  entsprechend �ndern.

  Wenn Sie jcuken statt translit verwenden bekommen Sie das offizielle``
  russische Schreibmaschinenlayout. Es wird so genannt weil die Tasten
  qwertz`` die Buchstabenkombination <with|language|russian|<with|font|cyrillic|������>>``
  erzeugen. <with|language|german|Diese Eingabemethode ist am sinnvollsten
  wenn sie eine richtige russische Tastatur besitzen, bei der zus�tzliche
  russische Buchstaben in Rot im jcuken-Layout auf die Tasten geschrieben
  sind (ein �hnlicher Effekt kann bei einer herk�mmlichen deutschen Tastatur
  erzeugt werden in dem man transparente Aufkleber mit den roten russischen
  Buchstaben auf die Tasten klebt). Das ist au�erdem n�tzlich wenn Sie ein
  erfahrener russischer Schreiber sind und Ihre Finger bereits an das Layout
  gew�hnt sind.>

  Diejenigen die keine russischen Buchstaben auf ihrer Tastatur dargestellt
  haben, bevorzugen meistens das yawerty Layout, bei dem die Tasten qwertz``
  die Buchstabenkombination <with|language|russian|<with|font|cyrillic|�����>>``
  ergeben. Jeder lateinische Buchstabe ist einem gleichwertigen russischen
  Buchstaben zugeordnet; zus�tzliche russische Buchstaben werden durch
  <key|shift>-Kombinationen erzeugt. <TeXmacs> kommt mit einem leicht
  modifizierten yawerty Layout welches die Tastenfunktion f�r <key|$>,
  <key|�>, <key|<with|mode|math|\<backslash\>>> nicht ver�ndert weil diese
  f�r <TeXmacs> sehr wichtig sind. Die entsprechenden russischen Buchstaben
  werden stattdessen durch verschiedene <key|shift>-Kombinationen erzeugt.

  <tmdoc-copyright|1998-2004|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-bot|30mm>
    <associate|page-odd|30mm>
    <associate|language|german>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Bearbeiten>|<with|font-family|<quote|ss>|Einstellungen>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Dokument>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Formate>|<with|font-family|<quote|ss>|Sprache>|<with|font-family|<quote|ss>|Russisch>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>