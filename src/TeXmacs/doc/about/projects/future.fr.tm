<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Plans pour l'avenir>

  <with|color|red|[Doit �tre mis � jour]>

  <section|Mise en page>

  Certains outils de mise en page n'ont pas encore �t� impl�ment�s. Ce sont
  les suivants :

  <\itemize>
    <item>S�paration d'un document en plusieurs parties.

    <item>Objets dynamiques, comme dans HTML.

    <item>Environnements pour les interfaces de logiciels de calcul formel.
  </itemize>

  Certains outils moins importants doivent �tre compl�t�s. En voici une liste
  non exhaustive :

  <\itemize>
    <item>Consid�rer les fractions comme des op�rateurs
    <with|mode|math|\<Rightarrow\>> espaces avant et apr�s. Idem pour les
    indices et exposants <with|mode|math|\<Rightarrow\>> petite espace avant
    quand ils sont plac�s � gauche et petite espace apr�s lorsqu'ils sont
    plac�s � droite.
  </itemize>

  <section|Outils d'�dition de texte>

  Bien que les mouvements du curseur, les s�lections, etc... soient d�j�
  g�r�s, il reste � parfaire certains outils d'�dition standards. Par exemple
  :

  <\itemize>
    <item>Recherche dans du texte, des formules, dans certains environnement,
    etc...

    <item>Demande de remplacement.

    <item>Outils math�matiques : simplification d'une r�gion s�lectionn�e,
    substitutions de formules � l'int�rieur d'autres formules, etc...

    <item>Contr�le de version.

    <item>Compression et protection de donn�es.

    <item>Correcteurs grammaticaux et programmes de traduction automatiques.
    Si vous savez o� trouvez des dictionnaires exhaustifs libres de droits ou
    du mat�riel se rapportant � la traduction, n'h�sitez pas � nous en faire
    part.

    <item>Int�gration d'un programme libre de reconnaissance vocale.
  </itemize>

  <section|Tableur universel>

  Nous aimerions incorporer un <space|0.2spc>tableur
  universel<space|0.2spc> dans <TeXmacs>. L'id�e est que toutes les
  d�pendances entre les cellules de la feuille de calcul soient analys�es par
  <TeXmacs>, mais que les calculs r�els soient d�l�gu�s au syst�me externe
  que vous aurez choisi, par exemple l'un de ceux qui sont g�r�s par les
  logiciels de calcul formel. Les donn�es de la feuille de calcul ne seraient
  pas forc�ment format�es dans un tableau rectangulaire ; on peut imaginer
  des d�pendances entre les noeuds d'un arbre, les �l�ments d'un graphe ou
  n'importe quoi d'autre.

  <section|Graphiques techniques>

  J'aimerais �galement incorporer un outil de dessin de graphiques
  techniques. On pourrait alors utiliser des macros d�finies par
  l'utilisateur pour faire des constructions g�om�triques. Il serait
  possible, par exemple, d'�crire un fichier de style pour dessiner des
  circuits �lectroniques ou des composants chimiques avec une barre d'ic�ne
  pour acc�der aux circuits ou composants voulus, de la m�me fa�on qu'on peut
  le faire pour s�lectionner des lignes et des cercles dans les dessins
  ordinaires.

  <section|Interface avec les logiciels de calcul formel>

  Les changements suivants doivent �tre faits pour pouvoir relier
  <apply|TeXmacs> aux logiciels de calcul formel :

  <\enumerate>
    <item>Am�lioration de la pr�sentation des sessions de calcul formel.

    <item>Ajout de fonctionnalit�s nouvelles pour augmenter
    l'interop�rabilit� entre <apply|TeXmacs> et les logiciels de calcul
    formel et pour permettre un contr�le plus grand de la pr�sentation de
    sorties longues.

    <item>S�mantique �tendue au niveau de la communication entre objets. Ce
    pourrait �tre soit des informations de haut niveau (comme les balises
    math�matiques Openmath ou HTML 4.0) ou des informations de bas niveau (y
    compris celles concernant la repr�sentation des donn�es), selon la
    vitesse d'ex�cution requise.

    <item>Autres possibilit�s d'�volution : mise en surbrillance, d�bogage,
    etc...
  </enumerate>

  <section|Interaction avec d'autres projets de type GNU>

  Il serait bon d'augmenter l'interaction entre <apply|TeXmacs> et d'autres
  projets de type GNU, tels Gnome ou des GUIs multi-plateformes. Cela
  faciliterait l'int�gration de donn�es externes dans les documents
  <apply|TeXmacs> et augmenterait le nombre de plateformes support�es. D'un
  autre c�t�, certaines fonctionnalit�s propres � <apply|TeXmacs>, telle la
  gestion des polices, peuvent int�resser d'autres projets.

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Typesetting><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Extra facilities for editing
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>A universal
      spreadsheet><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Technical
      pictures><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Interface with computer algebra
      systems><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Interaction with other GNU-like
      projects><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
