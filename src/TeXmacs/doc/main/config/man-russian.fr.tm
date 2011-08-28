<TeXmacs|1.0.7.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Notes pour les utilisateurs russes et ukrainiens>

  Pour saisir des textes en russe (ou en ukrainien), vous avez le choix entre
  :

  <\itemize>
    <item>Choisir le russe comme langage par d�faut avec
    <menu|Edit|Preferences|Language|Russian>. \ <TeXmacs> charge
    automatiquement les menus russes si le russe est d�sign�e comme votre
    langue par d�faut dans votre syst�me.

    <item>Choisir le russe pour un document donn� avec
    <menu|Document|Language|Russian>.

    <item>Choisir le russe pour une partie de texte � l'int�rieur d'un
    document avec <menu|Format|Language|Russian>.
  </itemize>

  Si votre serveur X utilise l'extension xkb et qu'il sait passer du mode
  latin au mode russe, vous n'avez pratiquement rien � faire. Il suffit que
  vous passiez en mode clavier russe. L'ensemble des logiciels n�cessaires
  pour effectuer cette op�ration est inclus dans les distributions r�centes
  de Linux et l'extension xkb est activ�e par d�faut dans
  <with|font-family|tt|XF86Config>. Avec l'extension xkb, les caract�res sont
  �crits sur 2 bytes et les caract�res russes d�marrent � 0x6??. Le clavier
  est configur� par <with|font-family|tt|setxkbmap>. Lors de son lancement, X
  envoie cette commande avec le fichier global <with|font-family|tt|Xkbmap>
  situ� en g�n�ral, s'il existe, dans <with|font-family|tt|/etc/X11/xinit> ;
  puis il envoie, s'il existe, le fichier utilisateur
  <with|font-family|tt|~/.Xkbmap>. Le fichier <with|font-family|tt|~/.Xkbmap>
  contient, en g�n�ral, la ligne suivante :

  <verbatim| \ \ \ ru basic grp:shift_toggle>

  Cela signifie qu'on peut changer de mode clavier avec <key|l-maj. temp.
  r-maj. temp>. On peut aussi choisir <key|ctrl majuscule temporaire> ou
  <key|ctrl alt>, voir <with|font-family|tt|/usr/X11R6/lib/X11/xkb/> pour de
  plus amples informations. C'est la m�thode la plus r�pandue pour changer de
  clavier sur les syst�mes Linux r�cents, dans le cas o� vous devez
  fr�quemment �crire en russe.

  Sur les syst�mes Linux plus anciens, l'extension xkb est, en g�n�ral,
  d�sactiv�e. Les caract�res sont �crits sur 1 byte et sont configur�s �
  l'aide de <with|font-family|tt|xmodmap>. Lors de son lancement, X envoie
  cette commande avec le fichier global <with|font-family|tt|Xmodmap> situ�
  en g�n�ral, s'il existe, dans <with|font-family|tt|/etc/X11/xinit> ; puis
  il envoie, s'il existe, le fichier utilisateur
  <with|font-family|tt|~/.Xmodmap>. Vous pouvez d�finir la combinaison de
  touches pour changer de mode clavier et choisir un encodage russe (par
  exemple koi8-r) sur 1 byte lorsque vous �tes en mode russe. Il est plus
  facile, n�anmoins, de t�l�charger le progiciel <with|font-family|tt|xruskb>
  et de lancer :

  <verbatim| \ \ \ xrus jcuken-koi8>

  au d�marrage de votre session X. Cela vous permet de disposer du clavier
  jcuken (voir plus loin) et de l'encodage koi8-r en mode russe. Si vous
  utilisez cette configuration de clavier, vous devez s�lectionner
  <menu|Edit|Preferences|Keyboard|Cyrillic input method|Koi8-r>.

  Vous pouvez aussi utiliser l'encodage Windows cp1251 au lieu de koi8-r,
  bien que cela soit rare sous UNIX. Si vous utilisez
  <with|font-family|tt|xrus jcuken-cp1251>, choisissez Cp1251 au lieu de
  Koi8-r.

  Toutes les m�thodes d�crites ci-dessus demandent un minimum de pr�paration
  pour ``russifier'' le clavier. Ce n'est pas tr�s difficile, voir
  Cyrillic-HOWTO ou mieux, sa derni�re version :\ 

  <verbatim|http://www.inp.nsk.su/<with|font-family|tt|~baldin/Cyrillic-HOWTO-russian/Cyrillic-HOWTO-russian.html>>

  Toutes les m�thodes ci-dessus ont aussi un effet sur toutes les
  applications X : �diteurs de texte \ (emacs, nedit, kedit...), xterms,
  <TeXmacs>, etc...

  Si vous �crivez en russe de fa�on tr�s �pisodique, il vaut mieux ne pas
  configurer de clavier. <TeXmacs> fournit, dans ce cas, des m�thodes de
  saisie en russe qui ne demandent aucune pr�paration. Ces m�thodes n'ont
  d'effet que dans <TeXmacs>.

  Le moyen le plus simple de saisir du russe sur un clavier standard US sans
  logiciel particulier est de choisir \ <menu|Edit|Preferences|Keyboard|Cyrillic
  input method|translit>. La saisie d'un caract�re latin produira alors le
  caract�re russe ``le plus proche''. Pour saisir certains caract�res russes,
  vous devrez utiliser des combinaisons de deux ou 3 caract�res :

  <big-table|<descriptive-table|<tformat|<cwith|2|11|1|1|cell-halign|l>|<cwith|2|11|2|2|cell-halign|l>|<cwith|2|11|2|2|cell-halign|c>|<cwith|2|11|4|4|cell-halign|l>|<cwith|2|11|4|4|cell-halign|c>|<table|<row|<cell|Raccourcis>|<cell|Pour>|<cell|Raccourcis>|<cell|Pour>>|<row|<cell|<key|text
  " e>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|accent:umlaut
  E>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  o>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y o> <key|Y
  O>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|z
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Z h> <key|Z
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|j
  tab>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|J
  tab>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|C h> <key|C
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S h> <key|S
  H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|s c
  h>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|S c h> <key|S
  C H>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|e
  tab>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|E
  tab>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  u>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y u> <key|Y
  U>>|<cell|<with|language|russian|font|cyrillic|�>>>|<row|<cell|<key|y
  a>>|<cell|<with|language|russian|font|cyrillic|�>>|<cell|<key|Y a> <key|Y
  A>>|<cell|<with|language|russian|font|cyrillic|�>>>>>>|Saisie d'un texte
  cyrillique sur un clavier roman.>

  Si, au contraire, vous voulez obtenir, par exemple,
  ``<with|language|russian|font|cyrillic|��>'' et non pas
  ``<with|language|russian|font|cyrillic|�>'', vous devez saisir <key|s / h>.
  Bien s�r, ce n'est pas la seule possibilit� pour faire correspondre des
  caract�res latins � des caract�res russes. Regardez les correspondances
  fournies dans <TeXmacs> et, si quelque chose ne vous convient pas,
  changez-le dans <with|font-family|tt|~/.TeXmacs/progs/my-init-texmacs.scm>.

  Si vous choisissez jcuken plut�t que translit, vous obtiendrez le clavier
  de machine � �crire russe ``standard''. On l'appelle ainsi car les touches
  ``qwerty'' donnent ``<with|language|russian|font|cyrillic|������>''. Cette
  m�thode de saisie est tr�s pratique si vous avez un clavier d'origine russe
  standard, clavier qui poss�de des caract�res russes suppl�mentaires peints
  en rouge sur les touches. On peut obtenir la m�me chose en collant des
  transparents avec des caract�res russes peints en rouge sur un clavier US
  standard). De m�me, si vous pouvez saisir un texte russe en aveugle,
  utilisez ce clavier.

  Les personnes qui ne poss�dent pas de clavier russe utilisent, en g�n�ral,
  le clavier yawerty, qui permet d'obtenir
  ``<with|language|russian|font|cyrillic|������>'' en saisissant ``qwerty''.
  Chaque caract�re est mis en correspondance avec un caract�re russe
  ``similaire'' ; on obtient certains caract�res russes sp�cifiques
  avec<key|majuscule temporaire>-nombre. <TeXmacs> fournit un clavier yawerty
  l�g�rement modifi�, car les touches <key|$>, <render-key|�> et <key|\\>,
  qui servent par ailleurs dans <TeXmacs>, ne sont pas red�finies. Les
  caract�res russes correspondant avec la combinaison de touches
  <key|majuscule temporaire>-nombre.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven|Mich�le Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
    <associate|preamble|false>
  </collection>
</initial>