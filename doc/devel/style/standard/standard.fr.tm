<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Styles et modules standards <TeXmacs>>

  Actuellement, les styles de documents standards suivants ont �t�
  impl�ment�s :

  <\itemize>
    <item>Livre,

    <item>Article,

    <item>Lettre,

    <item>S�minaire (pour les transparents).
  </itemize>

  Chacun de ces styles exporte un certain nombre de fonctions et
  d'environnement standards, dont la liste figure ci-dessous. Tout futur
  style de document standard devra g�r� au moins les commandes et
  environnements d�crits ci-dessus. Nous vous sugg�rons d'en faire autant si
  vous �crivez vos propres fichiers de style.

  <\itemize>
    <item>Commandes de sections.

    <item>Environnements de liste et d'�num�ration.

    <item>Environnements d'�quations.

    <item>Environnements de th�or�mes.

    <item>Environnements de programmation.
  </itemize>

  Vous noterez que les environnements de th�or�mes ne sont pas standards dans
  <apply|LaTeX>, ce qui est la principale source d'incompatibilit�. On peut
  ajouter de nouveaux  th�or�mes<space|0.2spc> avec la commande
  <verbatim|newtheorem>. On peut aussi ajouter de nouvelles
  <space|0.2spc>remarques<space|0.2spc> avec la commande
  <verbatim|newremark> ; les <space|0.2spc>remarques<space|0.2spc> sont
  diff�rentes des <space|0.2spc>th�or�mes<space|0.2spc> en ce sens que leur
  corps n'est g�n�ralement pas �crit avec une police grasse.

  Les environnements de programmation ne sont pas non plus g�r�s par
  <apply|LaTeX>. Ces environnements sont actuellement en cours de
  d�veloppement.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
