<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Mouvement structur� du curseur>

  <TeXmacs> poss�de trois m�canismes principaux pour le \S mouvement du
  curseur structur� \T :

  <\enumerate>
    <item>Parmi toute la structure du document.

    <item>Parmi les tags similaires � celui le plus int�rieur.

    <item>D�placement � l'int�rieur du tag le plus int�rieur.
  </enumerate>

  La plupart des raccourcis pour le d�placement structur� du curseur peut
  �tre utilis�e en combinaison avec la touche<nbsp><prefix|S-> afin de
  s�lectionner le texte durant le d�placement.

  <todo|personnaliser le comportement>

  <paragraph*|Parcours structur� du document>

  Les touches <shortcut|(traverse-left)>, <shortcut|(traverse-right)>,
  <shortcut|(traverse-up)> et <shortcut|(traverse-down)> sont utilis�es pour
  la travers�e structur�e de tout le document. En mode texte,
  <shortcut|(traverse-left)> et <shortcut|(traverse-right)> permettent de se
  d�placer de mot en mot, tandis que \ <shortcut|(traverse-up)> et
  <shortcut|(traverse-down)> permettent un d�placement de paragraphe en
  paragraphe.

  En pr�sence d'autres tags, le touches \ <shortcut|(traverse-left)> et
  <shortcut|(traverse-right)> permettent d'acc�der � toutes les positions du
  curseur � l'int�rieur du document; Toutefois, le d�placement mot � mot est
  conserv� en mode texte. Le comportement des touches
  <shortcut|(traverse-up)> et <shortcut|(traverse-down)> est plus d�pendant
  du contexte. A l'int�rieur des matrices, elles permettent typiquement de se
  d�placer de ligne � ligne.

  <paragraph*|Parcours structur� des tags>

  Ce type de mouvement permet de parcourir rapidement les tags
  <em|similaires> au \ tag le plus int�rieur. Les touches
  <shortcut|(traverse-previous)> et <shortcut|(traverse-next)> permettent de
  se d�placer vers le suivant ou le pr�c�dent, alors que
  <shortcut|(traverse-first)> et <shortcut|(traverse-last)> permettent
  d'atteindre directement le premier ou le dernier tag similaire.

  Par exemple, lorsque vous �tre dans une section de titre, vous pouvez
  rejoindre la section pr�c�dente (qui peut aussi �tre le titre d'une
  sous-section ou un chapitre, par exemple) en utilisant
  <shortcut|(traverse-previous)>. Remarquez que vous pouvez utiliser
  <key|C-�> pour sauter au titre de la section pr�c�dente.

  <paragraph*|D�placement � l'int�rieur d'un tag>

  Il est aussi possible de se d�placer dans le tag le plus int�rieur sans le
  quitter. Les raccourcis <shortcut|(structured-left)>,
  <shortcut|(structured-right)>, <shortcut|(structured-start)> et
  \ <shortcut|(structured-end)> fournissent un moyen pour aller � l'argument
  pr�c�dent, au suivant, au premier ou au dernier. En outre, les raccourcis
  <shortcut|(structured-exit-left)> et <shortcut|(structured-exit-right)>
  peuvent �tre utilis�s pour quitter le tag le plus int�rieur vers la gauche
  ou vers la droite.

  Ce comportement par d�faut peut �tre modifi� dans des contextes
  particuliers. Par exemple, � l'int�rieur de tableaux ou d'arbres, \ ils
  correspondent plut�t � des mouvements de cellule � cellule ou de noeud �
  noeud. En plus, des mouvements verticaux peuvent �tre effectu�s � l'aide de
  <shortcut|(structured-up)>, <shortcut|(structured-down)>,
  <shortcut|(structured-top)> et<nbsp><shortcut|(structured-bottom)>.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven|Denis Raux>

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