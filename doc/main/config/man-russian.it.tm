<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Note per utilizzatori russi e ucraini>

  Per scrivere testi in russo o in ucraino sono disponibili diverse opzioni:

  <\itemize>
    <item>scegliere Russo come lingua predefinita in
    <apply|menu|Edit|Preferences|Language|Russian>. Se la lingua russa �
    selezionata come lingua locale, <apply|TeXmacs> si avvier�
    automaticamente con i menu in lingua russa;

    <item>selezionare Russo per scrivere un intero documento in lingua russa
    utilizzando il menu <apply|menu|Document|Language|Russian>;

    <item>selezionare Russo per scrivere porzioni di un testo in russo
    all'interno di un testo in altra lingua utilizzando il menu
    <apply|menu|Format|Language|Russian>.
  </itemize>

  Se il vostro server X utilizza l'estensione xkb ed � istruito per eseguire
  lo switch tra le modalit� tastiera Latina e tastiera Russa, non avete
  bisogno di introdurre modifiche. Semplicemente eseguite lo switch con la
  modalit� tastiera russa e procedete. Tutto il software necessario a questo
  scopo viene normalmente incluso nelle moderne distribuzioni di GNU/Linux e
  l'estensione xkb viene attivata di default in <with|font
  family|tt|XF86Config>. Nell'estensione xkb i caratteri russi sono scritti
  su 2 byte e le lettere russe su 0x6??. La tastiera viene configurata
  attraverso <with|font family|tt|setxkbmap>. Quando il server X parte viene
  lanciato questo comando con il file di sistema globale <with|font
  family|tt|Xkbmap> (usualmente questo file si trova in <with|font
  family|tt|/etc/X11/xinit>) se esiste; e quindi, sempre se esiste, con il
  file degli utilizzatori <with|font family|tt|~/.Xkbmap>. Un file <with|font
  family|tt|~/.Xkbmap> pu� avere un aspetto del tipo

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Questo significa che la modalit� della tastiera pu� essere modificata
  attraverso <key|<apply|localize|l-shift> <apply|localize|r-shift>>. Altre
  scelte diffuse sono <key|C- S-> o
  <key|C- A->, si veda <with|font
  family|tt|/usr/X11R6/lib/X11/xkb/> per ulteriori dettagli. Questo � il
  settaggio migliore per sistemi Linux moderni e nel caso in cui sia
  prevedibile un uso intensivo della lingua russa.

  In vecchi sistemi Linux l'estensione xkb � spesso disabilitata. I caratteri
  vengono scritti su 1 byte e configurati in <with|font family|tt|xmodmap>.
  Quando il server X parte i comandi vengono lanciati attraverso <with|font
  family|tt|Xmodmap> (tipicamente incluso in <with|font
  family|tt|/etc/X11/xinit>), se esiste; e, sempre se esiste, con il file
  degli utilizzatori <with|font family|tt|~/.Xmodmap>. Potete configurare una
  combinazione di tasti per la modalit� di cambio della tastiera e utilizzare
  un codice in russo a 1 byte (come koi8-r) per lavorare in modalit� Russo.
  Risulta comunque pi� agevole scaricare il pacchetto <with|font
  family|tt|xruskb> e attivarlo all'inizio di una sesione di X digitando

  <verbatim| \ \ \ xrus jcuken-koi8>.

  Attraverso questo comando viene selezionato il formato jcuken (vedi oltre)
  e il codice koi8-r per la tastiera in modalit� Russo. Se decidete di
  utilizzare questo settaggio per la tastiera dovete selezionare Options
  <with|mode|math|\<rightarrow\>> international keyboard
  <with|mode|math|\<rightarrow\>> russian <with|mode|math|\<rightarrow\>>
  koi8-r.

  Potete anche utilizzare il codice Windows cp1251 al posto di koi8-r anche
  se questa scelta � rara in ambiente UNIX. Se utilizzate <with|font
  family|tt|xrus jcuken-cp1251>, selezionate cp1251 al posto di koi8-r.

  Tutti i metodi appena descritti richiedono alcune azioni specifiche per
  "rendere russa" la tastiera. Ci� non � difficile e per questo potete
  consultare il Cyrillic-HOWTO o, meglio, una sua versione aggiornata:

  <verbatim|http://www.inp.nsk.su/<with|font
  family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Oltre a questo precisiamo che i metodi precedenti modificano globalmente
  tutte le applicazioni che fanno uso di X: editori di testo (emacs, nedit,
  kedit...), terminali X, <apply|TeXmacs> ecc....

  Se avete bisogno di utilizzare raramente la lingua russa il settaggio della
  tastiera potrebbe causare pi� danni che benefici. Per utilizzatori
  occasionali della lingua russa <apply|TeXmacs> implementa un metodo che non
  richiede azioni preliminari. Ovviamente questo metodo funziona solo in
  <apply|TeXmacs> e non in altre applicazioni.

  Il modo pi� semplice per scrivere in russo con una tastiera standard
  americana, senza tuttavia aver apportato modifiche al software, consiste
  nel selezionare il menu <apply|menu|Edit|Preferences|Keyboard|Cyrillic
  input method|translit>. In questo modo quando si digita una lettera
  dell'alfabeto latino verr� prodotta la lettera russa ad esso "pi� simile".
  Per ottenere alcune lettere russe � necessario ricorrere a combinazioni di
  due o tre lettere:<vspace|0.5fn>

  <expand|big-table|<expand|descriptive-table|<tformat|<cwith|2|11|1|1|cell
  halign|l>|<cwith|2|11|2|2|cell halign|l>|<cwith|2|11|2|2|cell
  halign|c>|<cwith|2|11|4|4|cell halign|l>|<cwith|2|11|4|4|cell
  halign|c>|<table|<row|<cell|Abbreviazione>|<cell|per>|<cell|Abbreviazione>|<cell|per>>|<row|<cell|<expand|kbd-text|"
  e>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<expand|kbd-text|"
  E>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|j
  <expand|key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|J
  <expand|key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|e
  <expand|key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|E
  <expand|key-variant>>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|�>>>>>>|Scrivere in
  cirillico su una tastiera occidentale.>

  Se desiderate ``<with|language|russian|font|cyrillic|��>'' e non
  ``<with|language|russian|font|cyrillic|�>'' dovete digitare <key|s / h>.
  Chiaramente la scelta ottimale della mappatura da lettere latine a russe
  non � unica. Potete verificare la mappatura supportata da <apply|TeXmacs> e
  se non vi � gradita sovrascriverla in <with|font
  family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  Se viene selezionato jcuken al posto di translit, si ha a disposizione la
  tastiera russa ``ufficiale''. Questa tastiera ha questo nome in quanto i
  tasti \ ``qwerty'' producono ``<with|language|russian|<with|font|cyrillic|������>''>.
  Questo metodo di input risulta pi� utile se avete a disposizione una
  tastiera originale russa, che ha a disposizione ulteriori lettere
  dell'alfabeto russo scritte in rosso su alcuni tasti. Volendo utilizzare
  jcuken, un effetto simile si pu� riprodurre incollando opportunamente degli
  adesivi con lettere dell'alfabeto russo sulla tastiera americana.

  Coloro che non dispongono di lettere russe sulla tastiera forse
  preferiscono l'impaginazione yawertyin cui il tasto ``qwerty'' produce
  ``<with|language|russian|font|cyrillic|������''>. Anche qui ogni lettera
  latina viene mappata in una russa "simile", mentre altre lettere russe
  vengono prodotte con il tasto <key|S->-lettera. In
  <apply|TeXmacs> yawerty viene implementato in modo un p� diverso in quanto,
  per convenienza, non vengono ridefiniti i tasti <key|$>, <key|�>,
  <key|<with|mode|math|\<backslash\>>>. Le corrispondenti lettere russe
  vengono prodotte utilizzando una combinazione del tipo
  <key|S->-lettera.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|table>
      <tuple|normal|Typing Cyrillic text on a Roman
      keyboard.|<pageref|gly-1>>
    </associate>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Langue>|<with|font
      family|<quote|ss>|Russo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Lingua>|<with|font
      family|<quote|ss>|Russo>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Formato>|<with|font
      family|<quote|ss>|Lingua>|<with|font
      family|<quote|ss>|Russo>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>|<with|font family|<quote|ss>|Metodo di
      input cirillico>|<with|font family|<quote|ss>|translit>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
