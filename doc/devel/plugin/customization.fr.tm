<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personnalisation de l'interface>

  Une fois que vous aurez cr�� une premi�re interface entre votre syst�me et
  <apply|TeXmacs>, vous aurez s�rement envie de l'am�liorer. Vous trouverez
  ci-dessous quelques id�es pour le faire.

  Tout d'abord, vous pouvez personnaliser le comportement du clavier dans une
  session <verbatim|myplugin> et ajouter les menus d�sir�s. Vous trouverez
  les explications pour le faire dans le chapitre consacr� au langage
  d'extension <name|Guile/Scheme>. Vous pouvez int�grer vos changements au
  fichier <verbatim|init-myplugin.scm>. Nous vous recommandons d'examiner
  attentivement les plugins livr�s avec <TeXmacs> et situ�s dans le
  r�pertoire <verbatim|$TEXMACS_HOME_PATH/plugins>.

  Il vous faudra peut-�tre cr�er des balises sp�ciales pour certaines sorties
  sur votre syst�me. Supposons que vous vouliez associer un type invisible �
  chaque sous-expression de sortie. Pour ce faire, vous pouvez cr�er une
  macro <verbatim|exprtype> � deux arguments dans <verbatim|myplugin.ts> et
  envoyez des appels <apply|LaTeX>, tel <verbatim|\\exprtype{1}{Integer}>, �
  <apply|TeXmacs> durant la sortie.

  Dans le cas o� vous utilisez des tubes pour connecter votre syst�me �
  <apply|TeXmacs>, vous pouvez ex�cuter directement des commandes
  <apply|TeXmacs> pendant la sortie de votre syst�me en int�grant dans votre
  sortie des morceaux de code de la forme :

  <\verbatim>
    \ \ \ \ [DATA_BEGIN]command:scheme-program[DATA_END]
  </verbatim>

  � l'inverse, quand le curseur est dans une session syst�me, vous pouvez
  utiliser la commande <name|Scheme> :

  <\verbatim>
    \ \ \ \ (extern-exec plugin-command)
  </verbatim>

  pour ex�cuter une commande du syst�me.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
