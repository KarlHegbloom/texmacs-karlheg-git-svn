<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Architecture g�n�rale de <TeXmacs>>

  <section|Introduction>

  Le programme <apply|TeXmacs> a �t� �crit en C++. Vous devez utiliser
  <verbatim|g++> et l'utilitaire <verbatim|makefile> pour compiler
  <apply|TeXmacs>. � l'heure actuelle, le source (situ� dans le r�pertoire
  <verbatim|src>) de <apply|TeXmacs> est ventil� en plusieurs parties dans
  diff�rents r�pertoires :

  <\itemize>
    <item>Un jeu de structures de donn�es de base et g�n�riques dans le
    r�pertoire <verbatim|Basic>.

    <item>Des ressources standards pour <apply|TeXmacs>, tels les polices
    <apply|TeX>, les langages, encodages et dictionnaires dans le r�pertoire
    <verbatim|Resource>.

    <item>Une bo�te � outils graphiques (avec sa documentation pas tr�s �
    jour) dans le r�pertoire <verbatim|Window>.

    <item>Le langage d'extension pour <apply|TeXmacs> dans le r�pertoire
    <verbatim|Prg>.

    <item>La partie typographique de l'�diteur dans le r�pertoire
    <verbatim|src/Typeset>.

    <item>L'�diteur dans le r�pertoire <verbatim|src/Edit>.

    <item>Le serveur <apply|TeXmacs> dans le r�pertoire
    <verbatim|src/Server>.
  </itemize>

  Toutes ces parties utilisent les structures de donn�es d�finies dans
  <verbatim|Basic>. La bo�te � outils graphiques d�pend de
  <verbatim|Resource> en ce qui concerne les polices <apply|TeX>. Le langage
  d'extension est ind�pendant de <verbatim|Resource> et de <verbatim|Window>.
  La partie typographique s'appuie sur toutes les autres parties, sauf
  <verbatim|Prg>. L'�diteur et le serveur <apply|TeXmacs> utilisent toutes
  les autres parties.

  Les donn�es de <apply|TeXmacs> sont contenues dans le r�pertoire
  <verbatim|edit> qui correspond � la distribution <apply|TeXmacs> sans code
  source. En gros, on a les types de donn�es suivantes :

  <\itemize>
    <item>Donn�es concernant les polices dans <verbatim|fonts> (encodage,
    fichiers<verbatim|.pk>, etc...).

    <item>Donn�es concernant les langages dans <verbatim|languages> (mod�les
    de c�sure, dictionnaires, etc...).

    <item>Styles de document dans <verbatim|style>.

    <item>Initialisation et autres programmes <apply|scheme> dans
    <verbatim|progs>.
  </itemize>

  Le r�pertoire <verbatim|misc> contient diverses donn�es, telle l'ic�ne
  d'�dition (<verbatim|misc/pixmaps/edit.xpm>).

  <section|Repr�sentation interne des textes>

  <apply|TeXmacs> repr�sente tous les textes par des arbres (pour un texte
  donn�e, l'arbre correspondant est appel� <em|arbre d'�dition>). Les noeuds
  d'un tel arbre sont �tiquet�s par des <em|op�rateurs> standards dont la
  liste figure dans les fichiers <verbatim|Basic/Data/tree.hpp> et
  <verbatim|Basic/Data/tree.cpp>. Les �tiquettes des feuilles des arbres sont
  des cha�nes, qui sont visibles (le texte r�el) ou invisibles (telles les
  longueurs ou les d�finitions de macros).

  Le sens du texte et la mani�re dont il est typographi� d�pendent
  essentiellement de l'environnement actif. L'environnement consiste
  principalement en une table de hachage de type
  <verbatim|rel_hashmap\<less\>string,tree\<gtr\>>, c'est-�-dire une table de
  correspondance entre les variables d'environnement et leur valeur dans
  l'arbre. Le langage actif et la police courante sont des exemples de
  variables d'environnement ; de nouvelles variables peuvent �tre d�finies
  par l'utilisateur.

  <subsection|Texte>

  Dans <apply|TeXmacs>, toutes les cha�nes de texte sont constitu�es de
  suites de symboles universels ou sp�ciaux. Un symbole sp�cial est un
  caract�re, diff�rent de <verbatim|'\\0'>, <verbatim|'\<less\>'> ou
  <verbatim|'\<gtr\>'>. Son sens peut varier en fonction de la police avec
  laquelle il est utilis�. Un symbole universel est une cha�ne commen�ant par
  <verbatim|'\<less\>'>, suivi d'une suite arbitraire de caract�res diff�rent
  de <verbatim|'\\0'>, <verbatim|'\<less\>'> ou <verbatim|'\<gtr\>'>, et se
  terminant par <verbatim|'\<gtr\>'>. Le sens des caract�res universels ne
  d�pend pas de la police avec laquelle ils sont utilis�s, mais des polices
  diff�rentes peuvent les rendre de fa�on diff�rente.

  <subsection|langage>

  Le langage d'un texte est capable de fournir une analyse s�mantique
  d�taill�e d'une phrase du texte. Il peut, au minimum, d�couper la phrase en
  <em|mots> (qui sont de plus petites phrases) et donner � l'outil
  typographique des informations sur les espaces d�sir�es entre les mots et
  sur les c�sures � appliquer. Plus tard, il se peut qu'on ajoute d'autres
  s�mantiques aux langages. Par exemple, on pourrait impl�menter des
  correcteurs orthographiques pour les langages naturels et des analyseurs
  syntaxiques pour les formules math�matiques et les langages de
  programmation.

  <section|Composition des textes>

  En gros, l'outil typographique de <apply|TeXmacs> prend un arbre en entr�e
  et renvoie un cadre en acc�dant et en modifiant l'environnement
  typographique. La classe <verbatim|box> est multifonctionnelle. Sa m�thode
  principale est utilis�e pour afficher le cadre sur un p�riph�rique
  PostScript (soit l'�cran, soit une imprimante). Mais elle contient aussi de
  nombreuses donn�es de composition, tels les cadres logiques et les cadres
  d'encre, la position des scripts, etc...

  Les cadres servent aussi � passer d'un curseur physique (position sur
  l'�cran) � un curseur logique (chemin dans l'arbre d'�dition). En fait, les
  cadres sont, eux aussi, organis�s sous forme d'arbre, ce qui facilite la
  conversion. N�anmoins, les routines de conversion peuvent �tre tr�s
  complexes du fait de l'expansion des macros et des sauts de ligne et de
  page. Notez �galement qu'en plus d'une position horizontale et verticale le
  curseur physique poss�de aussi une position horizontale infinit�simale. En
  gros, cette coordonn�e infinit�simale est utilis�e dans certains cadres
  (par exemple pour les changements de couleur) pour ajouter une largeur
  infinit�simale.

  <section|Modification du texte>

  Vous trouverez dans le r�pertoire <verbatim|Edit/Modify> diff�rentes
  routines de modification de l'arbre d'�dition. Les modifications se
  d�roulent en plusieurs �tapes :

  <\enumerate>
    <item>Un �v�nement en entr�e d�clenche une action, comme par exemple
    <verbatim|make_fraction>, qui tente de modifier l'arbre d'�dition.

    <item>Toutes les modifications que <verbatim|make_fraction> ou ses
    sous-routines appliquent � l'arbre d'�dition sont, en fait, d�compos�es
    en sept routines de modification �l�mentaires : <verbatim|assign>,
    <verbatim|insert>, <verbatim|remove>, <verbatim|split>, <verbatim|join>,
    <verbatim|ins_unary> et <verbatim|rem_unary>.

    <item>Avant d'ex�cuter la modification demand�e, la routine de
    modification �l�mentaire envoie � toutes les vues du texte une
    notification concernant la modification.

    <item>Lors de la notification, chaque vue met � jour plusieurs �l�ments,
    comme la position du curseur. Une notification est aussi envoy�e �
    l'outil de composition du texte, car il maintient une liste des
    paragraphes d�j� compos�s.

    <item>Lorsque toutes les vues ont re�u notification de la modification,
    elle est alors r�ellement ex�cut�e.

    <item>Chaque action utilisateur, tels la frappe sur une touche ou un clic
    de souris, est responsable de l'insertion de <em|points d'annulation>
    entre les suites de modifications �l�mentaires. Lorsqu'une modification
    est annul�e, l'�dition revient au point d'annulation pr�c�dent.
  </enumerate>

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
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|2.1|?>>
    <associate|toc-4|<tuple|2.2|?>>
    <associate|toc-5|<tuple|3|?>>
    <associate|toc-6|<tuple|4|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Intern representation of
      texts><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      2.1<space|2spc>Text<value|toc-dots><pageref|toc-3>

      2.2<space|2spc>The language<value|toc-dots><pageref|toc-4>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Typesetting
      texts><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Making modifications in
      texts><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
