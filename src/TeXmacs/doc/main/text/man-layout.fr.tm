<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Probl�mes de mise en page>

  En g�n�ral, <apply|TeXmacs> se charge de la mise en page du texte. Par
  cons�quent, nous vous recommandons de ne pas la faire vous-m�me, bien que
  cela ne soit pas impossible. Par exemple, vous ne devez pas ins�rer
  d'espaces ou de lignes vierges suppl�mentaires entre les mots ou les
  lignes. Ces espaces verticaux ou horizontaux doivent �tre ins�r�s
  explicitement avec <apply|menu|Ins�rer|Espace>. Cela vous permettra de
  g�rer votre document de mani�re plus souple lors de changements mineurs
  affectant les sauts de page ou de ligne, ou de changements majeurs comme la
  modification du style du document.

  On a impl�ment� diff�rents types d'espaces explicites. Tout d'abord, on
  peut ins�rer des espaces fixes ; leur largeur et leur hauteur sont fixes.
  Les espaces horizontaux ont une hauteur nulle et sont soit �tirables, soit
  non �tirables. La longueur des espaces �tirables d�pend de la c�sure du
  paragraphe. De plus, il est possible d'ins�rer des tabulations. Les espaces
  verticaux peuvent �tre ins�r�s au d�but ou � la fin d'un paragraphe.
  L'espace vertical r�el entre deux paragraphes correspond au maximum entre
  l'espace vertical apr�s le premier paragraphe et l'espace vertical avant le
  second (ceci permet d'�viter un espace disproportionn� entre deux
  th�or�mes, contrairement � \ <apply|TeX>).

  En ce qui concerne le paragraphe, l'utilisateur peut indiquer le style du
  paragraphe (justifi�, cadr� � gauche, centr�, cadr� � droite), les marges
  et l'indentation � gauche (resp. � droite) de la premi�re (resp. derni�re)
  ligne. On peut aussi contr�ler l'espace entre paragraphes et lignes d'un
  m�me paragraphe.

  Vous pouvez indiquer la mise en page avec <apply|menu|Document|Page>. Tout
  d'abord, choisissez la fa�on dont les pages sont affich�es sur l'�cran ; si
  vous choisissez papier comme type de page dans
  <apply|menu|Document|Page|Type>, les sauts de page seront visibles. Par
  d�faut, le type de page est papyrus, ce qui �vite de voir les sauts de
  page lors de la cr�ation du document. Le type de page automatique
  correspond � une taille de papier identique � la taille de la fen�tre. Les
  marges de la page et la largeur du texte sont sp�cifi�s avec
  <apply|menu|Document|Page|Mise en page>. Il est souvent pratique de r�duire
  les marges de la page lorsqu'on la visualise ; on peut le faire avec
  <apply|menu|Document|Page|Apparence � l'�cran>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Espace>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Type>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Mise en
      page>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Page>|<with|font family|<quote|ss>|Apparence �
      l'�cran>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
