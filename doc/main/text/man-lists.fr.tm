<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listes>

  Vous pouvez ins�rer un liste ordinaire avec <apply|menu|Insert|Itemize>. Vous
  pouvez aussi choisir un marqueur particulier pour chaque �l�ment de la
  liste : <with|mode|math|\<bullet\>> (puces), <with|mode|math|<op|->>
  (tirets) or <with|mode|math|<op|\<rightarrow\>>> (fl�ches) ou utiliser le
  marqueur par d�faut. Les listes peuvent �tre <em|imbriqu�es> comme
  ci-dessous :

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
  plus haut, on utilise <with|mode|math|\<bullet\>>, au niveau secondaire
  <with|mode|math|<op|\<circ\>>>, et ainsi de suite. Quand le curseur se
  trouve � l'int�rieur d'une liste et que vous appuyez sur <key|retour
  chariot>, un nouvel �l�ment est automatiquement ajout� � la liste. Si la
  longueur de certains �l�ments d�passe la longueur de la ligne, appuyez sur
  <key|S-retour chariot> pour aller � la ligne sans sortir de l'�l�ment.

  Les �num�rations, obtenues avec <apply|menu|Insert|Enumerate>, ont des
  propri�t�s similaires aux listes ordinaires. Leurs �l�ments sont num�rot�s.
  Voici une �num�ration obtenue avec <apply|menu|Insert|Enumerate|I, II, III> :

  <\expand|enumerate-Roman>
    <item>Premier �l�ment.

    <item>Second �l�ment.

    <item>Dernier �l�ment.
  </expand>

  Le dernier type de liste sert � faire une description. On la g�n�re avec
  <apply|menu|Insert|Description> et cela permet de d�finir une liste de choses
  :

  <\description>
    <expand|item*|Gnou.>Un animal poilu, mais gentil.

    <expand|item*|Moucheron.>On ne le trouve qu'au zoo (NdT : sic).
  </description>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Liste>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|�num�ration>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|�num�ration>|<with|font family|<quote|ss>|I, II,
      III>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texte>|<with|font
      family|<quote|ss>|Description>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
