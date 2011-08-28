<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cr�ation de menus dynamiques>

  Vous pouvez d�finir ou modifier tout ou partie d'un menu nomm�
  <verbatim|name> avec :

  <\verbatim>
    \ \ \ \ (menu-bind name . prog)
  </verbatim>

  et ajouter de nouveaux articles � un menu nomm� <verbatim|name> avec :

  <\verbatim>
    \ \ \ \ (menu-extend name . prog)
  </verbatim>

  Ici, <verbatim|prog> est un programme qui repr�sente les articles d'un
  menu. Examinez les fichiers situ�s dans le r�pertoire :

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/progs/menu
  </verbatim>

  pour voir comment les menus standards de <apply|TeXmacs> sont d�finis.

  En fait, le programme <verbatim|prog> dans <verbatim|menu-set> ou
  <verbatim|menu-append> consiste en une liste d'articles qui peuvent prendre
  l'une des formes suivantes :

  <\verbatim>
    \ \ \ \ (=\<gtr\> "pulldown menu name" menu-definition)<format|next line>
    \ \ \ (-\<gtr\> "pullright menu name" menu-definition)<format|next line>
    \ \ \ ("entry" action)<format|next line> \ \ \ ---<format|next line>
    \ \ \ (if condition menu-definition)<format|next line> \ \ \ (link
    variable)
  </verbatim>

  Les constructeurs \ <verbatim|=\<gtr\>> et <verbatim|-\<gtr\>> sont
  utilis�s pour cr�er des menus d�roulants vers le bas ou la droite et
  <verbatim|menu-definition> doit contenir un programme qui cr�e le
  sous-menu. Le constructeur <verbatim|("entry" action)> cr�e une entr�e
  ordinaire, pour laquelle <verbatim|action> est compil�e et ex�cut�e quand
  on clique sur <verbatim|entry>. Les articles d'un menu peuvent �tre s�par�s
  avec <verbatim|--->. Le constructeur <verbatim|if> est utilis� pour ins�rer
  des articles de menus si et seulement si une certaine <verbatim|condition>
  est satisfaite (par exemple, si on est en mode math).

  Enfin, si l'on d�clare un menu <verbatim|name>, on peut utiliser ce menu
  indirectement avec le constructeur <verbatim|link>. Cette fa�on de faire
  procure deux avantages :

  <\itemize>
    <item>Un sous-menu <space|0.2spc>indirect<space|0.2spc>peut �tre li� �
    autant de menus que l'on veut.

    <item>On peut ajouter <with|font shape|italic|a posteriori> des articles
    aux sous-menus <space|0.2spc>indirects<space|0.2spc> avec
    <verbatim|menu-append>.
  </itemize>

  Les menus standards principaux de <apply|TeXmacs> sont :
  <verbatim|texmacs-menu>, <verbatim|texmacs-menu>,
  <verbatim|texmacs-popup-menu>, <verbatim|texmacs-main-icons>,
  <verbatim|texmacs-context-icons> et <verbatim|<verbatim|texmacs-extra-icons>>.
  Les autres menus standards indirects sont : <verbatim|file-menu>,
  <verbatim|edit-menu>, <verbatim|insert-menu>, <verbatim|text-menu>,
  <verbatim|paragraph-menu>, <verbatim|document-menu>,
  <verbatim|options-menu> et <verbatim|help-menu>.

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
