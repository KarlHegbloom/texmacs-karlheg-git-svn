<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Personnalisation des fichiers d'initialisation>

  Lors de son lancement, <apply|TeXmacs> ex�cute le fichier
  <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm>, sauf si vous cr�ez votre
  propre fichier d'initialisation dans <verbatim|$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm>.
  Par d�faut, le chemin <verbatim|$TEXMACS_HOME_PATH> correspond �
  <verbatim|.TeXmacs>. Vous voudrez peut-�tre ajouter certaines actions au
  fichier d'initialisation par d�faut. Dans ce cas, n'oubliez pas d'inclure
  la commande suivante :\ 

  <\verbatim>
    \ \ \ \ (exec-file "$TEXMACS_PATH/progs" "init-texmacs.scm")
  </verbatim>

  dans votre fichier d'initialisation. De m�me, le fichier
  <verbatim|$TEXMACS_PATH/progs/init-buffer.scm> est ex�cut� chaque fois que
  vous cr�ez un nouveau tampon, sauf si vous cr�ez votre propre fichier
  d'initialisation <verbatim|$TEXMACS_HOME_PATH/progs/my-init-buffer.scm>.

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
