<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Rechercher et remplacer>

  On commence la recherche de texte avec <key|C-s> ou
  <apply|menu|Edit|Search>. Pendant la recherche, la <space|0.2spc>cha�ne de
  recherche<space|0.2spc> est affich�e dans le pied de page � gauche. Tout
  caract�re saisi est ajout� � la cha�ne de recherche et son occurrence
  suivante est entour�e d'un cadre rouge. Si vous appuyez sur <key|C-s> une
  seconde fois pendant la recherche, le programme recherchera une seconde
  occurrence de la cha�ne. Un bip vous signale qu'il n'y a plus d'autres
  occurrences de la cha�ne � rechercher dans le document ; si vous appuyez
  encore une fois sur <key|C-s> � ce moment-l�, la recherche reprendra au
  d�but du document. Vous pouvez appuyez sur <key|retour arri�re> pour
  annuler les frappes et les d�placements effectu�s pendant la recherche.

  En g�n�ral, la recherche de texte s'effectue vers le bas � partir de la
  position du curseur. Vous pouvez aussi rechercher vers le haut avec
  <key|C-r>. La recherche est restreinte au mode et � la langue actifs au
  d�but de la recherche. En d'autres termes, si vous cherchez
  <with|mode|math|x> en mode math, la recherche ne trouvera pas les �ventuels
  x en mode texte. Actuellement, la cha�ne de recherche ne peut contenir que
  du texte ordinaire, aucun symbole math�matique ou texte structur� n'est
  pris en compte.

  Pour effectuer un remplacement, utilisez \ <key|C-=> or
  <apply|menu|Edit|Replace>. Vous devrez alors fournir une cha�ne de
  recherche et une cha�ne de remplacement. � chaque occurrence de la cha�ne
  de recherche, vous devrez choisir entre remplacer la cha�ne (y), ne pas la
  remplacer (n) ou remplacer la cha�ne et toutes les occurrences suivantes
  (a). Comme dans le cas de la recherche, le remplacement s'effectue sur la
  base d'un mode et d'une langue donn�s.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Chercher>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Remplacer>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
