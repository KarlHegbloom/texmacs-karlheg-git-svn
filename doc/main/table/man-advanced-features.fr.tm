<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Autres caract�ristiques>

  Dans les menus, vous trouverez d'autres propri�t�s applicables aux
  tableaux. Les voici r�sum�es, tr�s bri�vement :

  <\itemize>
    <item>Changement de la <space|0.2spc>port�e<space|0.2spc> d'une cellule
    de fa�on � ce qu'elle s'�tende sur les cellules voisines � droite et en
    dessous.

    <item>Cr�ation de sous-tableaux � l'int�rieur de cellules.

    <item>Changement de la hauteur et de la largeur d'un texte pour que les
    lignes de base correspondent.

    <item>C�sure horizontale du contenu d'une cellule et c�sure vertical du
    tableau.

    <item>Regroupement de lignes et/ou colonnes, de fa�on � ce que les
    cellules regroup�es fassent partie des bordures des autres cellules.

    <item>D�sactivation de la table pour voir son <space|0.2spc>code
    source<space|0.2spc>.

    <item>D�finition du <space|0.2spc>centre d'extension<space|0.2spc> du
    tableau. � partir de l�, les propri�t�s de mise en page de la cellule
    concern�e seront appliqu�es � toute nouvelle cellule cr��e autour de ce
    centre.

    <item>Sp�cification de la taille maximale et minimale d'un tableau, qui
    sera respect�e lors de modifications ult�rieures (ceci est tr�s utile
    quand on cr�e des macros tableau).
  </itemize>

  � l'heure actuelle, tous les tableaux sont ins�r�s dans un environnement de
  type <markup|tabular>, <markup|block>, <markup|matrix>, etc... Quand vous
  cr�ez vos propres macros tableau, vous pouvez utiliser
  <apply|menu|Table|Special table properties|Extract format> pour extraire le
  format d'un tableau donn�.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Mich�le Garoche>

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
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tabular>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|block>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|matrix>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tableau>|<with|font
      family|<quote|ss>|Propri�t�s sp�ciales du tableau>|<with|font
      family|<quote|ss>|Extraire format>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
