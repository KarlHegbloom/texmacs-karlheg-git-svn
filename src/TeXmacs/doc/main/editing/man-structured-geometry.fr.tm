<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <with|language|french|><tmdoc-title|Positionnement et redimensionnement
  d'objects>

  Le pr�fixe <prefix|structured:geometry> peut �tre utilis� pour
  repositionner et redimensionner les objects. Par exemple, � l'int�rieur
  d'une cellule d'un tableau, vous pouvez utiliser <key|structured:geometry
  right> pour d�placer les cellules plus vers la droite. Sur l'espace cr��
  <em|via> <menu|Format|Space>, la m�me touche permet d'accro�tre sa largeur.
  Plus g�n�ralement, les raccourcis suivants sont d�finis :

  <\description>
    <item*|<shortcut|(geometry-left)>>Diminue la taille horizontale d'un
    objet, ou le d�place vers la gauche.

    <item*|<shortcut|(geometry-right)>>Augmente la taille horizontale d'un
    objet, ou le d�place vers la droite.

    <item*|<shortcut|(geometry-down)>>Diminue/augmente la taille verticale
    d'un objet, ou le d�place vers le bas.

    <item*|<shortcut|(geometry-up)>>Augmente/diminue la taille verticale d'un
    object, ou le d�place vers le haut.

    <item*|<shortcut|(geometry-start)>>Diminue le d�calage horizontal d'un
    objet, ou l'aligne gauche.

    <item*|<shortcut|(geometry-end)>>Augmente le d�calage horizontal d'un
    objet, ou l'aligne droite.

    <item*|<shortcut|(geometry-bottom)>>Diminue le d�calage vertical d'un
    objet, ou l'aligne en bas.

    <item*|<shortcut|(geometry-top)>>Augmente le d�calage vertical d'un
    objet, ou l'aligne en haut.

    <item*|<shortcut|(geometry-reset)>>Restaure la g�om�trie (taille,
    position, alignement) aux valeurs par d�faut.

    <item*|<shortcut|(geometry-circulate #t)>, <shortcut|(geometry-circulate
    #f)>>Boucle � travers toutes les unit�s possibles pour la g�om�trie.

    <item*|<shortcut|(geometry-slower)>, <shortcut|(geometry-faster)>>D�cro�t
    ou augmente le pas de variation pour le redimensionnement ou le
    recadrage.
  </description>

  Tags sp�cifiques auxquels ces raccourcis s'appliquent :

  <\description>
    <item*|Espaces>A la fois horizontaux et verticaux � l'aide du menu
    <menu|Format|Space>. Vous devez mettre le curseur juste apr�s l'espace
    pour appliquer le raccourci.

    <item*|Bo�tes modificateurs>Les tags <markup|move>, <markup|shift>,
    <markup|resize> et <markup|clipped><compound|markup|> du menu
    <menu|Format|Transform>.

    <item*|Animations>Les dur�es des animations peuvent �tre modifi�es avec
    <shortcut|(geometry-left)> and <shortcut|(geometry-right)>.

    <item*|Images>La taille et l'alignement des images peuvent �tre modifi�s.
  </description>

  <tmdoc-copyright|1998--2010|Joris van der Hoeven|Denis Raux>

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