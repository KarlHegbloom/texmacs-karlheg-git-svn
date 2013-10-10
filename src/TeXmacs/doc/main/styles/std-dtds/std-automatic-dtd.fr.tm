<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|G�n�ration automatique de contenu>

  Le d.t.d. <tmdtd|std-automatic> d�finit la g�n�ration automatique de
  contenu, telles les tables de mati�res et les bibliographies, et leur
  affichage. Les balises suivantes sont utilis�es pour les bibliographies\ 

  <\explain|<markup|cite>>
    Fonction avec un nombre arbitraire d'arguments. Chaque argument est une
    citation correspondant � un article dans un fichier BiB-<TeX> file. Les
    citations sont affich�es telles que r�f�renc�es dans la bibliographie et
    servent d'hyperliens aux r�f�rences. Un point d'interrogation remplace
    les citations quand la bibliographie n'est pas g�n�r�e.
  </explain>

  <\explain|<markup|nocite*>>
    Identique � <markup|cite>, mais les citations ne sont pas affich�es dans
    le texte principal.
  </explain>

  <\explain|<markup|bibitem*>>
    Fonction qui indique comment afficher un article de bibliographie.
  </explain>

  Les balises suivantes sont utilis�es pour compiler des tables de mati�res :

  <\explain|<markup|toc-main-1>>
    Fonction � un argument pour cr�er une entr�e principale dans la table des
    mati�res. Cette fonction peut �tre utilis�e, par exemple, pour les
    diff�rentes parties d'un livre.
  </explain>

  <\explain|<markup|toc-main-2>>
    Fonction � un argument pour cr�er une entr�e principale dans la table des
    mati�res. Cette fonction est utilis�e pour les chapitres.
  </explain>

  <\explain|<markup|toc-normal-1>>
    Fonction � un argument pour cr�er une entr�e ordinaire dans la table des
    mati�res. Cette fonction est souvent utilis�e pour les sections.
  </explain>

  <\explain|<markup|toc-normal-2>>
    Identique � <markup|toc-normal-2> pour des entr�es moins importantes,
    telles les sous-sections.
  </explain>

  <\explain|<markup|toc-normal-3>>
    Identique � <markup|toc-normal-3> pour des entr�es encore moins
    importantes, telles les sous-sous-sections.
  </explain>

  <\explain|<markup|toc-small-1>>
    Utilis�e pour des entr�es de peu d'importance, tels les paragraphes (peut
    �tre ignor�e).
  </explain>

  <\explain|<markup|toc-small-2>>
    Utilis�e pour des entr�es d'encore moins d'importance, tels les
    sous-paragraphes.
  </explain>

  <\explain|<markup|toc-dots>>
    S�paration entre une entr�e dans la table des mati�res et le num�ro de
    page correspondant. Par d�faut, on utilise une suite de points
    horizontaux.
  </explain>

  Les balises suivantes sont utilis�es pour les indices :

  <\explain|<markup|index>>
    Fonction � un argument <var|x>, qui l'ins�re dans l'index en tant
    qu'entr�e principale.
  </explain>

  <\explain|<markup|subindex>>
    Fonction � deux arguments <var|x> et <var|y>, qui ins�re <var|y> dans
    l'index en tant que sous-entr�e de <var|x>.
  </explain>

  <\explain|<markup|subsubindex>>
    Fonction � trois arguments <var|x>, <var|y> et <var|z>, qui ins�re
    <var|z> dans l'index en tant que sous-entr�e de <var|y>, lui-m�me
    sous-entr�e de <var|x>.
  </explain>

  <\explain|<markup|index-complex>>
    Fonction � quatre arguments <var|key>, <var|how>, <var|range>,
    <var|entry>, expliqu�e dans le section <hlink|g�n�ration des
    index|../../links/man-index.fr.tm>.
  </explain>

  <\explain|<markup|index-line>>
    Cette fonction a deux arguments. Le premier <var|key>, cl� de tri,
    indique comment trier le second <var|entry>, l'entr�e. Aucun num�ro de
    page n'est g�n�r�.
  </explain>

  <\explain|<markup|index-1>>
    Macro avec une entr�e d'index et un num�ro de page. Utilis�e pour
    l'affichage d'une entr�e principale d'index.
  </explain>

  <\explain|<markup|index-1*>>
    Identique � <markup|index-1>, mais sans num�ro de page.
  </explain>

  <\explain|<markup|index-<math|n>>>
    (avec <math|n> compris entre 1 et 5) : macro avec une entr�e d'index et
    un num�ro de page, utilis�e pour l'affichage d'une entr�e de niveau
    <math|n>.
  </explain>

  <\explain|<markup|index-<math|n>*>>
    Identique � <markup|index-<math|n>>, mais sans num�ro de page.
  </explain>

  <\explain|<markup|index-dots>>
    Macro qui g�n�re les points entre une entr�e d'index et le(s) num�ro(s)
    de page(s) correspondant(s).
  </explain>

  Les balises suivantes sont utilis�es pour les glossaires :

  <\explain|<markup|glossary>>
    Fonction qui ins�re son unique argument dans un glossaire.
  </explain>

  <\explain|<markup|glossary-dup>>
    Cr�e un num�ro de page suppl�mentaire pour une entr�e d�j� ins�r�e.
  </explain>

  <\explain|<markup|glossary-explain>>
    Fonction pour ins�rer une entr�e de glossaire accompagn�e de son
    explication.
  </explain>

  <\explain|<markup|glossary-line>>
    Ins�re une entr�e de glossaire sans num�ro de page.
  </explain>

  <\explain|<markup|glossary-1>>
    Macro pour afficher une entr�e de glossaire et le num�ro de page
    correspondant.
  </explain>

  <\explain|<markup|glossary-2>>
    Macro pour afficher une entr�e de glossaire, son explication et le num�ro
    de page correspondant.
  </explain>

  <\explain|<markup|glossary-dots>>
    Macro qui g�n�re les points entre une entr�e de glossaire et le(s)
    num�ro(s) de page(s) correspondant(s).
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
  </collection>
</initial>