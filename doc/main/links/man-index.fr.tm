<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|G�n�ration d'un index>

  Pour g�n�rer un index, vous devez d'abord ins�rer les entr�es d'index dans
  votre document avec <apply|menu|Insert|Link|Index entry>. Ensuite,
  positionnez votre curseur � l'endroit o� vous voulez que votre index soit
  g�n�r� et cliquez sur <apply|menu|Text|Automatic|Index>. Les index sont
  g�n�r�s de la m�me fa�on que les tables de mati�res.

  Vous trouverez dans le menu <apply|menu|Insert|Link|Index entry>
  diff�rentes sortes d'entr�es d'index. Les plus simples sont
  <space|0.2spc>principal<space|0.2spc>, <space|0.2spc>sous<space|0.2spc>
  et <space|0.2spc>sous-sous<space|0.2spc>, qui sont des macros avec,
  respectivement, un, deux ou trois arguments. Les entr�es de la forme
  <space|0.2spc>sous<space|0.2spc> ou <space|0.2spc>sous-sous<space|0.2spc>peuvent
  �tre utilis�es pour subordonner certaines entr�es d'index � d'autres.

  Une entr�e d'index complexe comporte quatre arguments. La premi�re est une
  cl� de tri sur l'entr�e et doit �tre un
  <space|0.2spc>tuplet<space|0.2spc> (cr�� avec <key|inactive \<less\>>),
  dont le premier �l�ment est la cl� principale, le second la cl� secondaire,
  etc... Le second argument d'une entr�e d'index complexe est soit vide soit
  <space|0.2spc>strong<space|0.2spc>, auquel cas le num�ro de la page
  s'affichera en gras. Le troisi�me argument est en g�n�ral vide, mais si
  vous cr�ez deux entr�es d'index dont le troisi�me argument est non vide et
  identique, cela g�n�rera un <space|0.2spc>champ<space|0.2spc> de num�ros
  de page. Le quatri�me argument correspond � l'entr�e d'index exprim�e sous
  forme de tuplet.

  On peut aussi cr�er une entr�e d'index sans r�f�rence � un num�ro de page
  en utilisant l'article <space|0.2spc>Interjection<space|0.2spc> du menu
  <apply|menu|Insert|Link|Index entry>. Le premier argument de cette macro
  est une cl� de tri sur l'entr�e. Le second argument contient le texte de
  l'entr�e. Ceci permet de cr�er des sections
  <space|0.2spc>A<space|0.2spc>, <space|0.2spc>B<space|0.2spc>,
  <space|0.2spc>C<space|0.2spc>, etc... dans l'index.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entr�e dans
      l'index>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Automatique>|<with|font
      family|<quote|ss>|Index>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entr�e dans
      l'index>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Entr�e dans
      l'index>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
