<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <\expand|tmdoc-title>
    Les documents sont des arbres

    \;
  </expand>

  <apply|TeXmacs> repr�sente tous les textes sous forme d'arbres (pour un
  texte fig�, l'arbre correspondant est appel� <expand|def-index|arbre
  d'�dition>). Les noeuds internes de l'arbre sont �tiquet�s par des
  <expand|def-index|op�rateurs> de type <verbatim|tree_label> (voir
  <verbatim|Basic/Data/tree.gen.h>). Les �tiquettes des feuilles de l'arbre
  sont des cha�nes de caract�res, soit invisibles (telles les mesures ou les
  d�finitions de macros), soit visible (le texte lui-m�me). Les arbres
  <TeXmacs> peuvent �tre d�crits � l'aide de notations diverses. Par exemple,
  l'arbre :

  <\expand|quote>
    <with|mode|math|<tree|concat|x+y+|<tree|frac|1|2>|+|<tree|sqrt|y+z>>>
  </expand>

  repr�sente la formule :

  <\expand|tm-fragment>
    <with|mode|math|x+y+<frac|1|2>+<sqrt|y+z>>
  </expand>

  et peut aussi �tre d�crit de la fa�on suivante :

  <\expand|scheme-fragment>
    (concat

    \ \ "x+y"

    \ \ (frac "1" "2")

    \ \ "+"

    \ \ (sqrt "y+z"))
  </expand>

  en notation <value|scheme>.

  La signification du texte et la fa�on dont il est typographi� d�pendent
  essentiellement de son environnement. L'environnement consiste en une table
  d'informations qui fait correspondre les variables d'environnement � leurs
  valeurs dans l'arbre. La langue, la police et la couleur actives sont des
  exemples de variables d'environnement syst�me ; de nouvelles variables
  peuvent �tre d�finies par l'utilisateur. Par exemple, l'expression
  <value|scheme> suivante :

  <\expand|scheme-fragment>
    (concat

    \ \ "Some "

    \ \ (with "color" "blue" "blue")

    \ \ " text.")
  </expand>

  repr�sente le fragment de document :

  <\expand|tm-fragment>
    Some <with|color|blue|blue> text
  </expand>

  La primitive <TeXmacs> <verbatim|with> signale un changement local de
  variable d'environnement.

  Dans la suite, nous d�crirons plus en d�tail les op�rateurs standards
  <apply|TeXmacs> et les variables d'environnement. Il faut noter que le
  format de donn�es <apply|TeXmacs> est sujet � changement. Nous d�crivons
  ces changements dans la derni�re section. En g�n�ral, l'utilisateur ne
  prend pas conscience de ces changements, car ils sont effectu�s par des
  programmes de conversion qui mettent automatiquement � jour les op�rateurs.
  N�anmoins, ils ont parfois de l'importance pour les d�veloppeurs, bien que
  la plupart de ces changements concernent l'ajout de nouvelles primitives.

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
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|arbre d'�dition>|<pageref|idx-1>>

      <tuple|<tuple|op�rateurs>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
