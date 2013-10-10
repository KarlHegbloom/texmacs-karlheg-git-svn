<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements pour les objets flottants>

  Le d.t.d. <tmdtd|env-float> d�finit des balises pour les objets flottants.
  La balise suivante est la seule de haut niveau :

  <\explain|<markup|footnote>>
    Cr�e une note en bas de page.
  </explain>

  Les balises de bas niveau suivantes peuvent �tre utilis�es pour d�finir des
  environnements de haut niveau pour les figures ou les tableaux, tels
  <markup|big-figure>, <markup|small-figure>, <markup|big-table> et
  <markup|small-table> :

  <\explain|<markup|render-small-figure>>
    Macro d'affichage de petite figure. Les arguments sont : un nom court
    (tels \S<space|0.2spc>figure<space|0.2spc>\T ou
    \S<space|0.2spc>tableau<space|0.2spc>\T) pour la liste des figures, son
    nom r�el (tels \S<space|0.2spc>Figure 2.3<space|0.2spc>\T ou
    \S<space|0.2spc>Tableau 5<space|0.2spc>\T), la figure elle-m�me et une
    l�gende.
  </explain>

  <\explain|<markup|render-big-figure>>
    Variante de <markup|render-small-figure> pour afficher une grande figure.
  </explain>

  Les balises suivantes peuvent �tre utilis�es pour personnaliser l'apparence
  du texte autour des figures, tableaux et notes en bas de page :

  <\explain|<markup|figure-name>>
    Macro qui g�re l'apparence du texte \S<space|0.2spc>Figure<space|0.2spc>\T.
    Par d�faut, on utilise un style gras.
  </explain>

  <\explain|<markup|figure-sep>>
    S�parateur entre la figure suivie de son num�ro et la l�gende. Par
    d�faut, c'est un point suivi d'une espace.
  </explain>

  <\explain|<markup|footnote-sep>>
    S�parateur entre le num�ro de la note en bas de page et son texte. Par
    d�faut, c'est un point suivi d'une espace.
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