<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Commandes hybrides et simulation <LaTeX>>

  <apply|TeXmacs> vous permet de saisir des commandes <apply|LaTeX> � partir
  du clavier, comme expliqu� ci-dessous. Pressez tout d'abord la touche
  <key|\\> pour entrer en mode commande hybride
  <apply|LaTeX>/<apply|TeXmacs>. Saisissez ensuite la commande que vous
  souhaitez ex�cuter. � la fin de la frappe, vous verrez dans le pied de page
  � gauche quelque chose de ce genre :

  <\verbatim>
    \ \ \ \ \<less\>entr�e\<gtr\>: commande � ex�cuter
  </verbatim>

  � ce moment, si vous pressez la touche <key|entr�e>, votre commande sera
  ex�cut�e. Par exemple, en mode maths, vous pouvez cr�er une fraction avec
  <key|\\ f r a c entr�e>.

  Si la commande que vous avez saisie n'est pas une commande <apply|LaTeX>
  reconnue, le programme cherchera tout d'abord s'il existe une macro, une
  fonction ou un environnement <apply|TeXmacs> correspondant fourni par le
  fichier de style. Si c'est le cas, il y aura ex�cution de la macro, de la
  fonction ou de l'environnement compte tenu des arguments fournis. Dans le
  cas contraire, le programme consid�re que vous voulez d�finir une variable
  d'environnement et vous demande sa valeur. La touche <key|\\> est toujours
  �quivalente � l'une des commandes suivantes : <key|inactive l>,
  <key|inactive e>, <key|inactive a>, <key|inactive #> ou
  <key|inactive v>.

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
