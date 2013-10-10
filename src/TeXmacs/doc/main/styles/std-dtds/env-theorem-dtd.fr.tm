<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements th�or�mes>

  Le d.t.d. <tmdtd|env-theorem> \ d�finit les balises d'affichage des
  environnements th�or�mes. Les balises principales sont les suivantes :

  <\explain|<markup|render-theorem>>
    Macro d'affichage des environnements th�or�mes. Le premier argument donne
    le nom du th�or�me, tel \S<space|0.2spc>Th�or�me 1.2<space|0.2spc>\T et
    le second argument est le corps du th�or�me. Cet environnement est
    utilis� par les environnements d�finis avec <markup|new-theorem>.
  </explain>

  <\explain|<markup|render-remark>>
    Identique � <markup|render-theorem> pour les environnements remarques.
  </explain>

  <\explain|<markup|render-exercise>>
    Identique � <markup|render-theorem> pour les environnements exercices.
  </explain>

  <\explain|<markup|render-proof>>
    Identique � <markup|render-theorem> pour les d�monstrations. Cet
    environnement est principalement utilis� pour personnaliser le nom d'une
    d�monstration, comme dans \S<space|0.2spc>Fin de la d�monstration du
    th�or�me 1.2<space|0.2spc>\T.\ 
  </explain>

  <\explain|<markup|dueto>>
    Environnement qui peut �tre utilis� pour indiquer des auteurs d'un
    th�or�me.
  </explain>

  <\explain|<markup|corollary*>>
    Pour les corollaires non num�rot�s. Cet environnement se base sur
    <markup|render-theorem>.
  </explain>

  <\explain|<markup|proof>>
    Pour la d�monstration des th�or�mes. Cet environnement se base sur
    <markup|render-proof>.
  </explain>

  Les balises suivantes peuvent �tre utilis�es pour personnaliser les
  environnements.

  <\explain|<markup|theorem-name>>
    Macro qui g�re l'apparence des noms des environnements th�or�mes et
    remarques. La plupart utilisent un style gras ou des petites majuscules.
  </explain>

  <\explain|<markup|exercise-name>>
    Identique � <markup|theorem-name> pour les exercices.
  </explain>

  <\explain|<markup|theorem-sep>>
    S�parateur entre le nom d'un environnement th�or�me ou remarque et son
    corps. Par d�faut, il s'agit d'un point suivi d'une espace.
  </explain>

  <\explain|<markup|exercise-sep>>
    Identique � <markup|theorem-sep> pour les exercices.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|french>
  </collection>
</initial>