<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cadres produits par l'outil typographique>

  <section|Introduction>

  L'outil de composition de <apply|TeXmacs> traduit un document repr�sent�
  par un arbre en un cadre graphique, qui peut �tre affich� sur l'�cran ou
  imprim�. Contrairement � <apply|LaTeX>, le cadre graphique contient
  beaucoup plus d'informations qu'il n'est n�cessaire pour un rendu
  graphique. En gros, ces informations peuvent �tre rang�es dans les
  cat�gories suivantes :

  <\itemize>
    <item>Cadres physique et logique.

    <item>M�thode de rendu graphique.

    <item>Diverses donn�es typographiques.

    <item>Trace du sous-arbre source qui a conduit au cadre.

    <item>Calcul des positions des curseurs et des s�lections.

    <item>Gestionnaires d'�v�nements pour le contenu dynamique.
  </itemize>

  Le cadre logique est utilis�e par le composeur pour positionner le cadre
  par rapport aux autres. D'autres informations, telle l'inclinaison du
  cadre, sont aussi sauvegard�es pour que le composeur puisse en faire usage.
  Le cadre physique contient la repr�sentation graphique du cadre. Cette
  information sert � redessiner partiellement un cadre de mani�re efficiente.

  Pour positionner le curseur ou pour faire une s�lection, il faut avoir une
  correspondance entre les positions logiques dans l'arbre source et les
  positions physiques dans les cadres compos�s. En fait, les cadres et leurs
  sous-cadres sont organis�s logiquement comme des arbres. Les cadres
  fournissent des routines de traduction entre les chemins dans l'arbre du
  cadre et l'arbre source, et pour trouver le chemin associ� � un point
  graphique.

  <section|Correspondance entre un cadre et son source>

  <subsection|Probl�mes � r�soudre>

  Pour impl�menter la correspondance entre les chemins dans l'arbre source et
  l'arbre du cadre, il faut surmonter plusieurs types de difficult�s en m�me
  temps :

  <\enumerate>
    <item>La correspondance peut ne pas �tre directe � cause des sauts de
    lignes, des notes en base de page et de l'expansion des macros.

    <item>La correspondance doit �tre relativement efficiente du point de vue
    de l'espace et du temps.

    <item>Certains cadres, tels les en-t�tes et les pieds de page ou le
    r�sultat de certains expansions de macros, peuvent ne pas �tre
    <space|0.2spc>accessibles<space|0.2spc>. Bien qu'on puisse trouver une
    position du curseur pour cliquer dessus, le contenu d'un tel cadre n'est
    pas directement �ditable.

    <item>La correspondance doit �tre relativement compl�te (voir section
    suivante).
  </enumerate>

  La premi�re difficult� nous oblige � stocker dans l'arbre d'�dition un
  chemin pour tout cadre. Pour gagner de la place, ce chemin est stock� en
  ordre inverse de fa�on � ce que les sommets communs puissent �tre partag�s.
  Ce partage des sommets communs est aussi n�cessaire pour changer rapidement
  les emplacements du source quand on modifie l'arbre source, par exemple
  lorsqu'on ins�re un nouveau paragraphe.

  Pour surmonter la troisi�me difficult�, le chemin inverse peut d�buter par
  un nombre n�gatif qui indique que le cadre ne peut �tre �dit� directement
  (on dit alors que le cadre est une d�coration). Dans ce cas, la fin du
  chemin inverse correspond � l'emplacement, dans l'arbre source, o� le
  curseur est positionn� lorsque l'on clique sur le cadre. Le nombre n�gatif
  a une influence sur la fa�on dont ceci est r�alis�.

  <subsection|Trois sortes de chemins>

  Il faut g�rer trois sortes de chemins :

  <\description>
    <expand|item*|Chemins d'arbres.>Ces chemins correspondent aux chemins
    dans l'arbre source. En fait, le chemin amput� de son dernier �l�ment
    pointer sur un sous-arbre de l'arbre source. Le dernier �l�ment donne la
    position dans ce sous-arbre : si le sous-arbre est une feuille,
    c'est-�-dire une cha�ne, c'est la position dans cette cha�ne. Autrement,
    un 0 indique une position avant le sous-arbre, un 1 une position apr�s le
    sous-arbre.

    <expand|item*|Chemins inverses.>Ce sont les chemins construits en
    inversant les chemins d'arbres (avec une fin de chemin commune) ; ils
    peuvent avec une en-t�te n�gative. Une en-t�te n�gative indique que le
    chemin d'arbre n'est pas accessible, c'est-�-dire que le sous-arbre
    correspondant ne repr�sente pas un contenu �ditable. Si les valeurs
    n�gatives sont <with|mode|math|-2>, <with|mode|math|-3> ou
    <with|mode|math|-4>, alors il faut mettre un 0 ou un 1 derri�re le chemin
    d'arbre suivant la valeur et la position du curseur.

    <expand|item*|Chemins de cadres.>Ces chemins correspondent aux chemins
    logiques dans l'arbre de cadre. Le chemin amput� de son dernier �l�ment
    pointe vers un sous-cadre du cadre principal. Le dernier �l�ment donne la
    position dans le sous-arbre : si le sous-cadre correspond � un cadre de
    texte, c'est la position dans ce texte. Sinon, un 0 indique une position
    avant le sous-cadre et un 1 une position apr�s le sous-cadre. Dans le cas
    de cadres auxiliaires, un 2 ou un 3 peuvent aussi indiquer la position
    apr�s un indice ou exposant � gauche, ou bien avant un indice ou exposant
    � droite.
  </description>

  <subsection|Routines de conversion>

  Pour impl�menter la conversion entre les trois sortes de chemins, chaque
  cadre poss�de un chemin inverse de r�f�rence <verbatim|ip> dans l'arbre
  source. Les cadres composites poss�dent en plus un chemin de r�f�rence
  gauche <verbatim|lip> et droit <verbatim|rip>, qui correspondent aux
  chemins accessibles juste � leur gauche ou juste � leur droite dans leurs
  sous-cadres (s'il existe de tels sous-cadres).

  La routine :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_tree_path (path bp)
  </verbatim>

  transforme un chemin de cadre en un chemin d'arbre. Cette routine (qui
  n'utilise que <verbatim|ip>) est rapide et poss�de une complexit� en temps
  lin�aire fonction de la longueur des chemins. La routine :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_box_path (path p)
  </verbatim>

  effectue la conversion inverse. Malheureusement, dans le pire des cas, il
  peut �tre n�cessaire de rechercher le chemin d'arbre dans tous les
  sous-cadres. N�anmoins, dans le meilleur des cas, un algorithme
  dichotomique (qui utilise <verbatim|lip> et <verbatim|rip>) \ trouve la
  branche � parcourir dans un temps logarithmique. Cet algorithme poss�de
  aussi une complexit� quadratique du temps fonction de la longueur des
  chemins, car il faut souvent inverser les chemins.

  <section|Curseur et s�lection>

  Pour remplir son r�le d'�diteur structur�<space|0.2spc>, <apply|TeXmacs>
  doit fournir une correspondance (relativement) compl�te entre les chemins
  d'arbres logiques et les positions physiques du curseur. Ceci conduit � des
  difficult�s suppl�mentaires dans le cas de <space|0.2spc>changement
  d'environnement<space|0.2spc>, tel un changement de police ou de couleur.
  En effet, quand on se trouve � la fronti�re d'un tel changement, on ne sait
  pas, <with|font shape|italic|a priori>, de fa�on pr�cise dans quel
  environnement on se trouve.

  C'est pourquoi, dans <apply|TeXmacs>, la position du curseur contient des
  coordonn�es <with|mode|math|x> et <with|mode|math|y>, ainsi qu'une abscisse
  infinit�simale suppl�mentaire, appel�e <with|mode|math|\<delta\>>. Par
  suite, un changement d'environnement est repr�sent� par un cadre de largeur
  infinit�simale. Bien que la position <with|mode|math|\<delta\>> du curseur
  soit toujours 0 quand on utilise la souris pour effectuer une s�lection,
  elle peut ne pas �tre nulle quand on se d�place � l'aide des fl�ches
  directionnelles. La routine en temps lin�aire :

  <\verbatim>
    \ \ \ \ virtual path box_rep::find_box_path (SI x, SI y, SI delta)
  </verbatim>

  fonction de la longueur du chemin recherche le chemin du cadre qui
  correspond � la position du curseur. Inversement, la routine :\ 

  <\verbatim>
    \ \ \ \ virtual cursor box_rep::find_cursor (box bp)
  </verbatim>

  m�ne � une repr�sentation graphique du curseur pour un chemin de cadre
  donn�. Le curseur est d�fini par ses coordonn�es <with|mode|math|x>,
  <with|mode|math|y> et <with|mode|math|\<delta\>> et un segment de ligne
  relatif � cette origine, d�fini par ses extr�mit�s
  <with|mode|math|(x<rsub|1>,y<rsub|1>)> et
  <with|mode|math|(x<rsub|2>,y<rsub|2>)>.

  De m�me, la routine :

  <\verbatim>
    \ \ \ \ virtual selection box_rep::find_selection (box lbp, box rbp)
  </verbatim>

  calcule la s�lection entre deux chemins de cadres donn�s. Cette s�lection
  comprend deux chemins d'arbre d�limitant la s�lection et une repr�sentation
  graphique sous la forme d'une liste de rectangles.

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
    <associate|toc-3|<tuple|2.1|?>>
    <associate|toc-4|<tuple|2.2|?>>
    <associate|toc-5|<tuple|2.3|?>>
    <associate|toc-6|<tuple|3|?>>
    <associate|toc-7|<tuple|4.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Introduction><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>The correspondence between a box and
      its source><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      2.1<space|2spc>Discussion of the problems being
      encountered<value|toc-dots><pageref|toc-3>

      2.2<space|2spc>The three kinds of paths<value|toc-dots><pageref|toc-4>

      2.3<space|2spc>The conversion routines<value|toc-dots><pageref|toc-5>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The cursor and
      selections><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
