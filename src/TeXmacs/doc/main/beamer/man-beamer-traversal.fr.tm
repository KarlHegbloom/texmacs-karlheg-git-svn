<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Parcourir une pr�sentation>

  Une grande partie des marqueurs pour les pr�sentations concerne le parcours
  du document durant la pr�sentation. Les touches <key|F10> et <key|F11> sont
  utilis�es pour se d�placer dans le document en arri�re <abbr|resp.> en
  avant. Les touches <key|F9> et <key|F12> sont utilis�es pour aller au d�but
  <abbr|resp.> � la fin de la pr�sentation. Quand on utilise le style
  <tmstyle|beamer> ou quand on a activ� ``presentation tool'' dans le menu
  <menu|Tools>, un menu <menu|Dynamic> et des ic�nes suppl�mentaires
  apparaissent; Ils peuvent aussi �tre utilis�s pour la navigation dans votre
  pr�sentation.

  Le marqueur de parcours le plus basique est nomm� \S \ <markup|switch> \T,
  il permet de montrer successivement les diff�rentes parties du document. La
  pr�sentation elle-m�me est habituellement un \ <markup|commutateur
  screens>, o� les parties sont les vues successives. Apr�s la selection du
  style <tmstyle|beamer>, ce commutateur peut �tre ins�r� � l'aide de
  <menu|Focus|Screens> ou <menu|Insert|Fold|Switch|Screens>. La vue nomm�e \S
  slide 1 \T est alors cr��e. Apr�s avoir cr�er d'autres vues (voir comment
  plus bas<space|0.2spc>), vous pourrez sauter de vue en vue en utilisant
  \ <key|pageup> et <key|pagedown>

  A l'int�rieur d'un <markup|switch>, de nouveaux \ \S branchements \T
  peuvent �tre ins�r�s avant ou apr�s la vue courante en utilisant
  <menu|Focus|Insert argument after> ou <menu|Focus|Insert argument before>.
  En plus du <markup|commutateur screens>, vous pouvez utiliser
  <menu|Insert|Fold|Switch|Standard> pour ins�rer des commutations sur des
  paragraphes entiers ou sur de simples lignes (de fa�on anlogue �
  l'affichage de formules en ligne).

  Une \ mani�re r�pandue de parcourir les pr�sentations, est le d�roulement
  progressif du contenu. Cela peut �tre fait par l'insertion de la balise
  <markup|unroll> � l'aide de <menu|Insert|Fold|Unroll>. Gr�ce � une astuce,
  cette balise peut �tre combin�e avec les balises <markup|itemize> et
  <markup|enumerate> de la fa�on suivante :

  <\enumerate-numeric>
    <item>Cr�ez la liste � l'aide de par exemple <menu|Insert|List|dash>

    <item>Enlevez le premier <markup|item> (le tiret dans cet exemple) �
    l'aide de <key|backspace>

    <item>Ins�rez la balise <markup|unroll> � l'aide de
    <menu|Insert|Fold|Unroll>

    <item>Pressez <key|enter> pour cr�er le premier item de la liste

    <item>Utilisez <menu|Focus|Insert argument after> suivi de <key|enter>
    pour ajouter un nouvel <markup|item>
  </enumerate-numeric>

  Notez que vous pouvez d�rouler plusieurs \ <markup|items> � la fois en les
  ajoutant simplement � la suite (sans utiliser <menu|Focus|Insert argument
  after>).

  Une variante du d�roulement est le d�veloppement d'un contenu. Il s'agit
  basiquement d'une balise � deux branchements, diff�rentes variantes sont
  disponibles dans <menu|Insert|Fold|Folded> selon l'effet d�sir�. Quelques
  unes des variantes affichent un bouton dans la barre d'information
  <markup|Focus> permettant de d�velopper ou plier le contenu. \ Les champs
  d'entr�e-sortie d'une session de calcul sont aussi utilisables.
  Similairement, la balise <menu|Insert|Fold|Summarize> est un
  \ <markup|commutateur> � deux branchements avec l� aussi diff�rents types
  d'effets disponibles.

  Lors de l'utilisation de <TeXmacs> en combinaison avec un plug-in externe,
  tel qu'un syst�me de calcul alg�brique sur ordinateur, vous pouvez
  remarquer que tous les champs d'entr�e-sortie des
  <hlink|sessions|../interface/man-session-basic.en.tm> sont pliables. En
  plus, vous pouvez cr�er un \S <hlink|executable
  switches|../interface/man-scripting-language.en.tm> \T en utilisant le
  sous-menu <menu|Insert|Fold|Executable>. Cela vous permet de commuter entre
  une entr�e donn�e au syst�me de calcul et sa sortie correspondante.

  Tous les marqueurs utilis�s pour le parcours des pr�sentations, peuvent
  �tre imbriqu�s de mani�re naturelle. Dans le menu
  <menu|Insert|Fold|Traversal>, vous pouvez sp�cifier si le d�roulement et le
  zones pliables qui doivent �tre repli�es apr�s la parcours.\ 

  <tmdoc-copyright|2010|Joris van der Hoeven|Denis Raux>

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