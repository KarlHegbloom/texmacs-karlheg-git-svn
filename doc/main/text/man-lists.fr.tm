<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Listes>

  Vous pouvez ins�rer un liste ordinaire avec <menu|Insert|Itemize>. Vous
  pouvez aussi choisir un marqueur particulier pour chaque �l�ment de la
  liste : <math|\<bullet\>> (puces), <math|<op|->> (tirets) or
  <math|<op|\<rightarrow\>>> (fl�ches) ou utiliser le marqueur par d�faut.
  Les listes peuvent �tre <em|imbriqu�es> comme ci-dessous :

  <\itemize>
    <item>Premier �l�ment.

    <item>Sous-liste :

    <\itemize>
      <item>Sous-�l�ment.

      <item>Autre sous-�l�ment.
    </itemize>

    <item>Dernier �l�ment.
  </itemize>

  Le marqueur par d�faut change suivant le niveau d'imbrication. Au niveau le
  plus haut, on utilise <math|\<bullet\>>, au niveau secondaire
  <math|<op|\<circ\>>>, et ainsi de suite. Quand le curseur se trouve �
  l'int�rieur d'une liste et que vous appuyez sur <key|retour chariot>, un
  nouvel �l�ment est automatiquement ajout� � la liste. Si la longueur de
  certains �l�ments d�passe la longueur de la ligne, appuyez sur
  <key|S-retour chariot> pour aller � la ligne sans sortir de l'�l�ment.

  Les �num�rations, obtenues avec <menu|Insert|Enumerate>, ont des propri�t�s
  similaires aux listes ordinaires. Leurs �l�ments sont num�rot�s. Voici une
  �num�ration obtenue avec <menu|Insert|Enumerate|I, II, III> :

  <\enumerate-Roman>
    <item>Premier �l�ment.

    <item>Second �l�ment.

    <item>Dernier �l�ment.
  </enumerate-Roman>

  Le dernier type de liste sert � faire une description. On la g�n�re avec
  <menu|Insert|Description> et cela permet de d�finir une liste de choses :

  <\description>
    <item*|Gnou>Un animal poilu, mais gentil.

    <item*|Moucheron>On ne le trouve qu'au zoo (NdT : sic).
  </description>

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