<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Balises standards>

  Diverses balises standards sont d�finies dans <tmdtd|std-markup>. Les
  balises de texte suivantes comportent un argument. Vous trouverez la
  plupart d'entre elles dans le menu <apply|menu|Insert|Content tag>.

  <\description>
    <expand|item*|<markup|strong>>Signale une zone de texte
    <strong|importante>. Vous pouvez entrer cette balise avec
    <apply|menu|Insert|Content tag|Strong>.

    <expand|item*|<markup|em>>Met l'accent sur une zone de texte, comme dans
    \ <space|0.2spc>la <em|pure> v�rit�<space|0.2spc>. Cette balise
    correspond au menu <apply|menu|Insert|Content tag|Emphasize>.

    <expand|item*|<markup|dfn>>Pour les d�finitions, comme dans
    <space|0.2spc>un <dfn|gnou> est une b�te � cornes<space|0.2spc>. Cette
    balise correspond au menu <apply|menu|Insert|Content tag|Definition>.

    <expand|item*|<markup|samp>>Suite de caract�res, comme les caract�res
    <samp|ae> dans la ligature �. Cette balise est accessible � partir de
    <apply|menu|Insert|Content tag|Sample>.

    <expand|item*|<markup|name>>Nom d'une chose ou d'un concept, comme le
    syst�me <name|Linux>. Cette balise est accessible via
    <apply|menu|Insert|Content tag|Name>.

    <expand|item*|<markup|person>>Nom d'une personne, comme <name|Joris>.
    Cette balise correspond � <apply|menu|Insert|Content tag|Person>.

    <expand|item*|<markup|cite*>>Citation bibliographique de livre ou de
    journal. Exemple : <expand|cite*|Moby Dick> de Melville. Ne confondez pas
    cette balise, accessible via <apply|menu|Insert|Content tag|Cite>, avec
    <markup|cite>. Cette derni�re balise est aussi utilis�e pour les
    citations, mais utilise un argument qui se r�f�re � une entr�e dans une
    base de donn�es de r�f�rences bibliographiques.

    <expand|item*|<markup|abbr>>Abr�viation. Exemple : Je travaille au
    <abbr|C.N.R.S.> Vous pouvez cr�er une abr�viation avec
    <apply|menu|Insert|Content tag|Abbreviation> ou avec le raccourci clavier
    <key|text a>.

    <expand|item*|<markup|acronym>>Un acronyme est une abr�viation form�e �
    partir de la premi�re lettre des mots d'un nom ou d'une phrase, tels
    <acronym|HTML> ou <acronym|IBM>. Les lettres ne sont pas s�par�es par des
    points. Vous pouvez saisir un acronyme avec <apply|menu|Insert|Content
    tag|Acronym>.

    <expand|item*|<markup|verbatim>>Texte verbatim, telle la sortie d'un
    programme informatique. Exemple : le programme a dit <verbatim|bonjour>.
    Vous pouvez saisir du texte verbatim avec <apply|menu|Insert|Content
    tag|Verbatim>. Cette balise peut aussi �tre utilis�e en tant
    qu'environnement de texte multi-paragraphe.

    <expand|item*|<markup|kbd>>Texte � entrer au clavier. Exemple : veuillez
    appuyer sur la touche <kbd|retour chariot>. Cette balise correspond au
    menu<apply|menu|Insert|Content tag|Keyboard>.

    <expand|item*|<markup|code*>>Code d'un programme informatique, comme dans
    <space|0.2spc><expand|code*|cout \<less\>\<less\> 1+1;> yields
    <verbatim|2><space|0.2spc>. Cette balise correspond �
    <apply|menu|Insert|Content tag|Code>. Pour de longues portions de code,
    utilisez l'environnement <markup|code>.

    <expand|item*|<markup|var>>Variables d'un programme informatique, comme
    dans <verbatim|cp <var|src-file> <var|dest-file>>. Cette balise
    correspond au menu <apply|menu|Insert|Content tag|Variable>.

    <expand|item*|<markup|math>>Cette balise sert � ins�rer une formule
    math�matique dans un texte litt�ral. Exemple : la formule
    <math|sin<rsup|2> x+cos<rsup|2> x=1> est bien connue.

    <expand|item*|<markup|op>>Cette balise sert, dans un texte scientifique,
    � indiquer qu'un op�rateur doit �tre consid�r� pour lui-m�me, sans aucun
    argument. Exemple : l'op�ration <math|<op|+>> est une fonction de
    <with|mode|math|\<bbb-R\><rsup|2>> dans <with|mode|math|\<bbb-R\>>. Cette
    balise pourrait devenir obsol�te.

    <expand|item*|<markup|tt>>C'est une balise physique pour la phase de
    saisie. Elle est utilis�e pour assurer la compatibilit� avec <name|HTML>,
    n�anmoins nous vous recommandons de ne pas l'utiliser.
  </description>

  Les balises suivantes correspondent � des environnements standards :

  <\description>
    <expand|item*|<markup|verbatim>>D�crit ci-dessus.

    <expand|item*|<markup|code>>Identique � <markup|code*>, mais pour
    plusieurs lignes de code.

    <expand|item*|<markup|quote>>Environnement de citation courte (un
    paragraphe).

    <expand|item*|<markup|quotation>>Environnement de citation longue
    (plusieurs paragraphes).

    <expand|item*|<markup|verse>>Environnement de versification.

    <expand|item*|<markup|center>>C'est une balise physique pour centrer une
    ou plusieurs lignes de texte. Elle est utilis�e pour assurer la
    compatibilit� avec <name|HTML>, mais nous vous recommandons de ne pas
    l'utiliser.
  </description>

  Voici quelques environnements standards pour les tableaux :

  <\description>
    <expand|item*|<markup|tabular*>>Tableaux centr�s.

    <expand|item*|<markup|block>>Tableaux align�s � gauche avec une bordure
    standard de largeur <verbatim|1ln>.

    <expand|item*|<markup|block*>>Tableaux centr�s avec une bordure standard
    de largeur <verbatim|1ln>.
  </description>

  Les balises suivantes n'ont pas d'arguments :

  <\description>
    <expand|item*|<markup|TeXmacs>>Logo <TeXmacs>.

    <expand|item*|<markup|TeX>>Logo <TeX>.

    <expand|item*|<markup|LaTeX>>Logo <LaTeX>.

    <expand|item*|<markup|hflush>>Utilis�e par les d�veloppeurs pour le
    cadrage � droite dans la d�finition d'un environnement.

    <expand|item*|<markup|hrule>>Ligne horizontale telle que celle que vous
    pouvez voir ci-dessous :

    <value|hrule>
  </description>

  Les balises suivantes acceptent un ou plusieurs argument(s) :

  <\description>
    <expand|item*|<markup|overline>>Pour <overline|surligner> du texte, qui
    peut s'�tendre sur plusieurs lignes.

    <expand|item*|<markup|underline>>Pour <underline|souligner> du texte, qui
    peut s'�tendre sur plusieurs lignes.

    <expand|item*|<markup|fold>>Macro � deux arguments. Le premier argument
    est affich�, le second ne l'est pas. La macro correspond � l'affichage
    pli� d'un rabat sur un texte associ� � un titre court ou un r�sum�. On
    peut rendre visible le second argument avec
    <apply|menu|Insert|Switch|Unfold>.

    <expand|item*|<markup|unfold>>Macro � deux arguments <var|x> et <var|y>,
    qui correspond � l'affichage d�pli� d'un rabat sur un texte <var|y>
    associ� � un titre court ou un r�sum� <var|x>. On peut rendre invisible
    le second argument avec <apply|menu|Insert|Switch|Fold>.

    <expand|item*|<markup|switch>>Macro � deux arguments <var|x> et <var|y>,
    o� <var|y> est une suite de repr�sentations possibles d'un rabat et
    <var|x> la repr�sentation active. Les touches de fonction <key|F9>,
    <key|F10>, <key|F11> et <key|F12> peuvent �tre utilis�es pour passer
    d'une repr�sentation � une autre.

    <expand|item*|<markup|phantom>>Fonction � un argument <var|x>. Cette
    balise permet d'afficher un espace �quivalent en largeur � la place
    qu'aurait occup� <var|x> s'il avait �t� affich�. Par exemple, si l'on
    utilise <space|0.2spc>fant�me<space|0.2spc> en argument, on obtient
    <space|0.2spc><apply|phantom|fant�me><space|0.2spc>.

    <expand|item*|<markup|set-header>>Fonction � un argument pour changer
    l'en-t�te de fa�on permanente. Notez que certaines balises du fichier de
    style, telles les balises de section, ne prennent pas en compte ces
    changements manuels.

    <expand|item*|<markup|set-footer>>Fonction � un argument pour changer le
    pied de page de fa�on permanente.
  </description>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
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
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-50|<tuple|<uninit>|?>>
    <associate|idx-40|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-30|<tuple|<uninit>|?>>
    <associate|idx-51|<tuple|<uninit>|?>>
    <associate|idx-41|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-31|<tuple|<uninit>|?>>
    <associate|idx-52|<tuple|<uninit>|?>>
    <associate|idx-42|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-22|<tuple|<uninit>|?>>
    <associate|idx-32|<tuple|<uninit>|?>>
    <associate|idx-53|<tuple|<uninit>|?>>
    <associate|idx-43|<tuple|<uninit>|?>>
    <associate|idx-33|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-23|<tuple|<uninit>|?>>
    <associate|idx-34|<tuple|<uninit>|?>>
    <associate|idx-44|<tuple|<uninit>|?>>
    <associate|idx-24|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-54|<tuple|<uninit>|?>>
    <associate|idx-55|<tuple|<uninit>|?>>
    <associate|idx-45|<tuple|<uninit>|?>>
    <associate|idx-35|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-25|<tuple|<uninit>|?>>
    <associate|idx-56|<tuple|<uninit>|?>>
    <associate|idx-46|<tuple|<uninit>|?>>
    <associate|idx-36|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-26|<tuple|<uninit>|?>>
    <associate|idx-57|<tuple|<uninit>|?>>
    <associate|idx-47|<tuple|<uninit>|?>>
    <associate|idx-37|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-27|<tuple|<uninit>|?>>
    <associate|idx-58|<tuple|<uninit>|?>>
    <associate|idx-48|<tuple|<uninit>|?>>
    <associate|idx-38|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-28|<tuple|<uninit>|?>>
    <associate|idx-59|<tuple|<uninit>|?>>
    <associate|idx-49|<tuple|<uninit>|?>>
    <associate|idx-39|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
    <associate|idx-29|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-markup>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|strong>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Important>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|em>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font family|<quote|ss>|Mis en
      relief>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dfn>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|D�finition>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|samp>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|�chantillon>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|name>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Nom>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|person>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Personne>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite*>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Citer>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|cite>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|abbr>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Abr�viation>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|acronym>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Sigle>>|<pageref|idx-21>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-22>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Verbatim>>|<pageref|idx-23>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|kbd>>|<pageref|idx-24>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-25>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-26>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Code>>|<pageref|idx-27>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-28>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|var>>|<pageref|idx-29>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Balise>|<with|font
      family|<quote|ss>|Variable>>|<pageref|idx-30>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|math>>|<pageref|idx-31>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|op>>|<pageref|idx-32>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tt>>|<pageref|idx-33>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verbatim>>|<pageref|idx-34>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code>>|<pageref|idx-35>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|code*>>|<pageref|idx-36>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quote>>|<pageref|idx-37>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|quotation>>|<pageref|idx-38>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|verse>>|<pageref|idx-39>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|center>>|<pageref|idx-40>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular*>>|<pageref|idx-41>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-42>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block*>>|<pageref|idx-43>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeXmacs>>|<pageref|idx-44>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|TeX>>|<pageref|idx-45>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|LaTeX>>|<pageref|idx-46>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hflush>>|<pageref|idx-47>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|hrule>>|<pageref|idx-48>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|overline>>|<pageref|idx-49>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|underline>>|<pageref|idx-50>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|fold>>|<pageref|idx-51>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Switch>|<with|font
      family|<quote|ss>|Unfold>>|<pageref|idx-52>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|unfold>>|<pageref|idx-53>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Switch>|<with|font
      family|<quote|ss>|Plier>>|<pageref|idx-54>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|switch>>|<pageref|idx-55>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|phantom>>|<pageref|idx-56>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-header>>|<pageref|idx-57>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|set-footer>>|<pageref|idx-58>>
    </associate>
  </collection>
</auxiliary>
