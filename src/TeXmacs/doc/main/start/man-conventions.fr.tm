<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Conventions typographiques>

  <paragraph|Acc�s aux menus>

  Dans le guide <TeXmacs>, les acc�s aux menus sont indiqu�s par une police
  <em|sans serif> : <menu|Document>, <menu|File|Load> ou
  \ <menu|Format|Police|Forme|Italique>.

  <paragraph|Modificateurs clavier>

  Voici les abr�viations utilis�es pour les raccourcis clavier dans <TeXmacs>
  :

  <\description>
    <item*|<prefix|S->>Combinaison avec touche majuscule temporaire.

    <item*|<prefix|C->>Combinaison avec touche ctrl.

    <item*|<prefix|A->>Combinaison avec touche alt.

    <item*|<prefix|M->>Combinaison avec touche meta.
  </description>

  Par exemple, <key*|M-S-x> correspond � l'action consistant � appuyer
  simultan�ment sur les touches <prefix|M->, <prefix|S-> et <key|x>.

  <paragraph|Raccourcis clavier>

  Des raccourcis clavier plus complexes sont obtenus en appuyant sur
  plusieurs touche � la suite. Par exemple, le raccourci <key*|-\<gtr\>>
  correspond � appuyer d'abord sur la touche <key*|-> puis sur la touche
  <key*|\<gtr\>>. Dans une formule math�matique, ce racourci insert la fl�che
  <math|\<rightarrow\>>. De m�me, le raccourci clavier <key*|C-x><key*|C-f>
  consiste � appuyer simultan�ment, sur les touches <key*|C-> et <key*|x>
  d'abord, puis sur les touches <key*|C-> et <key*|f> ensuite. En mode
  <name|Emacs>, ce raccourci clavier vous permettra d'ouvrir un nouveau
  fichier.

  Certains pr�fixes clavier communs sont d�taill�s dans la section sur
  <hlink|les r�gles g�n�rales du clavier|../text/keyboard/man-general-rules.fr.tm>.
  Dans les cas o� les raccourcis clavier <TeXmacs> sont remplac�s par les
  raccourcis clavier du syst�me d'exploitation, nous remarquons que les
  <hlink|�quivalents pour les modificateurs
  clavier|../config/man-config-keyboard.fr.tm#kbd-escape-table> peuvent �tre
  obtenus en utilisant la touche <key*|escape>. Par exemple, <key*|escape>
  est �quivalente � <key*|M-> et <key*|escape><key*|escape> est �quivalente �
  <key*|A->.

  Notez que, dans <TeXmacs>, les menus et le clavier sont <em|contextuels>,
  c'est-�-dire qu'ils d�pendent du mode actif (mode texte ou
  \S<space|0.2spc>mode math<space|0.2spc>\T, par exemple), du langage utilis�
  et de la position du curseur dans le document. En mode math, par exemple,
  il existe des raccourcis clavier sp�ciaux pour saisir facilement des
  formules math�matiques ; ces raccourcis sont �videmment inop�rants en mode
  texte.

  <paragraph|Touches sp�ciales>

  Sur certaines plateformes, certaines touches sp�ciales telles que la touche
  <key*|Return> sont pr�sent�es par des glyphes courts. Le tableau suivant
  pr�sentes ces touches sp�ciales et leur signification.

  <\big-table|<descriptive-table|<tformat|<cwith|1|-1|3|3|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|l>|<cwith|1|-1|1|-1|cell-row-span|1>|<cwith|1|-1|1|-1|cell-col-span|1>|<cwith|7|11|1|1|cell-lborder|1ln>|<cwith|7|11|1|2|cell-halign|l>|<cwith|7|11|1|2|cell-row-span|1>|<cwith|7|11|1|2|cell-col-span|1>|<table|<row|<cell|Touche>|<cell|Signification>|<cell|Touche>|<cell|Signification>>|<row|<cell|<key*|S->>|<cell|Modificateur
  touche majuscule temporaire>|<cell|<key*|left>>|<cell|Cuuseur vers la
  gauche>>|<row|<cell|<key*|capslock>>|<cell|Verrouillage des
  majuscules>|<cell|<key*|right>>|<cell|Curseur vers la
  droite>>|<row|<cell|<key*|C->>|<cell|Modificateur touche
  ctrl>|<cell|<key*|up>>|<cell|Cursor vers le
  haut>>|<row|<cell|<key*|A->>|<cell|Modificateur touche
  alt>|<cell|<key*|down>>|<cell|Curseur vers le
  bas>>|<row|<cell|<key*|M->>|<cell|Modificateur touche
  meta>|<cell|<key*|home>>|<cell|Racine>>|<row|<cell|<key*|return>>|<cell|Return>|<cell|<key*|end>>|<cell|Fin>>|<row|<cell|<key*|delete>>|<cell|Supprimer
  vers l'avant>|<cell|<key*|pageup>>|<cell|Haut de
  page>>|<row|<cell|<key*|backspace>>|<cell|Backspace>|<cell|<key*|pagedown>>|<cell|Bas
  de page>>|<row|<cell|<key*|escape>>|<cell|Escape>|<cell|<key*|space>>|<cell|Espace>>|<row|<cell|<key*|tab>>|<cell|Tab>|<cell|>|<cell|>>>>>>
    Touches sp�ciales.
  </big-table>

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Mich�le Garoche, Daouda
  Niang Diatta>

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
  </collection>
</initial>