<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Balises standards>

  Diverses balises standards sont d�finies dans <tmdtd|std-markup>. Les
  balises de texte suivantes comportent un argument. Vous trouverez la
  plupart d'entre elles dans le menu <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Signale une zone de texte <strong|importante>. Vous pouvez entrer cette
    balise avec <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Met l'accent sur une zone de texte, comme dans \ \S<space|0.2spc>la
    <em|pure> v�rit�<space|0.2spc>\T. Cette balise correspond au menu
    <menu|Insert|Content tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Pour les d�finitions, comme dans \S<space|0.2spc>un <dfn|gnou> est une
    b�te � cornes<space|0.2spc>\T. Cette balise correspond au menu
    <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Suite de caract�res, comme les caract�res <samp|ae> dans la ligature �.
    Cette balise est accessible � partir de <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    Nom d'une chose ou d'un concept, comme le syst�me <name|Linux>. Cette
    balise est accessible via <menu|Insert|Content tag|Name>.
  </explain>

  <\explain|<markup|person>>
    Nom d'une personne, comme <name|Joris>. Cette balise correspond �
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Citation bibliographique de livre ou de journal. Exemple : <cite*|Moby
    Dick> de Melville. Ne confondez pas cette balise, accessible via
    <menu|Insert|Content tag|Cite>, avec <markup|cite>. Cette derni�re balise
    est aussi utilis�e pour les citations, mais utilise un argument qui se
    r�f�re � une entr�e dans une base de donn�es de r�f�rences
    bibliographiques.
  </explain>

  <\explain|<markup|abbr>>
    Abr�viation. Exemple : Je travaille au <abbr|C.N.R.S.> Vous pouvez cr�er
    une abr�viation avec <menu|Insert|Content tag|Abbreviation> ou avec le
    raccourci clavier <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Un acronyme est une abr�viation form�e � partir de la premi�re lettre des
    mots d'un nom ou d'une phrase, tels <acronym|HTML> ou <acronym|IBM>. Les
    lettres ne sont pas s�par�es par des points. Vous pouvez saisir un
    acronyme avec <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Texte verbatim, telle la sortie d'un programme informatique. Exemple : le
    programme a dit <verbatim|bonjour>. Vous pouvez saisir du texte verbatim
    avec <menu|Insert|Content tag|Verbatim>. Cette balise peut aussi �tre
    utilis�e en tant qu'environnement de texte multi-paragraphe.
  </explain>

  <\explain|<markup|kbd>>
    Texte � entrer au clavier. Exemple : veuillez appuyer sur la touche
    <kbd|retour chariot>. Cette balise correspond au menu<menu|Insert|Content
    tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    Code d'un programme informatique, comme dans \S<space|0.2spc><code*|cout
    \<less\>\<less\> 1+1;> yields <verbatim|2><space|0.2spc>\T. Cette balise
    correspond � <menu|Insert|Content tag|Code>. Pour de longues portions de
    code, utilisez l'environnement <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Variables d'un programme informatique, comme dans <verbatim|cp
    <var|src-file> <var|dest-file>>. Cette balise correspond au menu
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Cette balise sert � ins�rer une formule math�matique dans un texte
    litt�ral. Exemple : la formule <math|sin<rsup|2> x+cos<rsup|2> x=1> est
    bien connue.
  </explain>

  <\explain|<markup|op>>
    Cette balise sert, dans un texte scientifique, � indiquer qu'un op�rateur
    doit �tre consid�r� pour lui-m�me, sans aucun argument. Exemple :
    l'op�ration <math|<op|+>> est une fonction de <math|\<bbb-R\><rsup|2>>
    dans <math|\<bbb-R\>>. Cette balise pourrait devenir obsol�te.
  </explain>

  <\explain|<markup|tt>>
    C'est une balise physique pour la phase de saisie. Elle est utilis�e pour
    assurer la compatibilit� avec <name|HTML>, n�anmoins nous vous
    recommandons de ne pas l'utiliser.
  </explain>

  Les balises suivantes correspondent � des environnements standards :

  <\explain|<markup|verbatim>>
    D�crit ci-dessus.
  </explain>

  <\explain|<markup|code>>
    Identique � <markup|code*>, mais pour plusieurs lignes de code.
  </explain>

  <\explain|<markup|quote>>
    Environnement de citation courte (un paragraphe).
  </explain>

  <\explain|<markup|quotation>>
    Environnement de citation longue (plusieurs paragraphes).
  </explain>

  <\explain|<markup|verse>>
    Environnement de versification.
  </explain>

  <\explain|<markup|center>>
    C'est une balise physique pour centrer une ou plusieurs lignes de texte.
    Elle est utilis�e pour assurer la compatibilit� avec <name|HTML>, mais
    nous vous recommandons de ne pas l'utiliser.
  </explain>

  Voici quelques environnements standards pour les tableaux :

  <\explain|<markup|tabular*>>
    Tableaux centr�s.
  </explain>

  <\explain|<markup|block>>
    Tableaux align�s � gauche avec une bordure standard de largeur
    <verbatim|1ln>.
  </explain>

  <\explain|<markup|block*>>
    Tableaux centr�s avec une bordure standard de largeur <verbatim|1ln>.
  </explain>

  Les balises suivantes n'ont pas d'arguments :

  <\explain|<markup|TeXmacs>>
    Logo <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    Logo <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    Logo <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Utilis�e par les d�veloppeurs pour le cadrage � droite dans la d�finition
    d'un environnement.
  </explain>

  <\explain|<markup|hrule>>
    Ligne horizontale telle que celle que vous pouvez voir ci-dessous :

    <hrule>
  </explain>

  Les balises suivantes acceptent un ou plusieurs argument(s) :

  <\explain|<markup|overline>>
    Pour <overline|surligner> du texte, qui peut s'�tendre sur plusieurs
    lignes.
  </explain>

  <\explain|<markup|underline>>
    Pour <underline|souligner> du texte, qui peut s'�tendre sur plusieurs
    lignes.
  </explain>

  <\explain|<markup|fold>>
    Macro � deux arguments. Le premier argument est affich�, le second ne
    l'est pas. La macro correspond � l'affichage pli� d'un rabat sur un texte
    associ� � un titre court ou un r�sum�. On peut rendre visible le second
    argument avec <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|unfold>>
    Macro � deux arguments <var|x> et <var|y>, qui correspond � l'affichage
    d�pli� d'un rabat sur un texte <var|y> associ� � un titre court ou un
    r�sum� <var|x>. On peut rendre invisible le second argument avec
    <menu|Insert|Switch|Fold>.
  </explain>

  <\explain|<markup|switch>>
    Macro � deux arguments <var|x> et <var|y>, o� <var|y> est une suite de
    repr�sentations possibles d'un rabat et <var|x> la repr�sentation active.
    Les touches de fonction <key|F9>, <key|F10>, <key|F11> et <key|F12>
    peuvent �tre utilis�es pour passer d'une repr�sentation � une autre.
  </explain>

  <\explain|<markup|phantom>>
    Fonction � un argument <var|x>. Cette balise permet d'afficher un espace
    �quivalent en largeur � la place qu'aurait occup� <var|x> s'il avait �t�
    affich�. Par exemple, si l'on utilise
    \S<space|0.2spc>fant�me<space|0.2spc>\T en argument, on obtient
    \S<space|0.2spc><phantom|fant�me><space|0.2spc>\T.
  </explain>

  <\explain|<markup|set-header>>
    Fonction � un argument pour changer l'en-t�te de fa�on permanente. Notez
    que certaines balises du fichier de style, telles les balises de section,
    ne prennent pas en compte ces changements manuels.
  </explain>

  <\explain|<markup|set-footer>>
    Fonction � un argument pour changer le pied de page de fa�on permanente.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

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