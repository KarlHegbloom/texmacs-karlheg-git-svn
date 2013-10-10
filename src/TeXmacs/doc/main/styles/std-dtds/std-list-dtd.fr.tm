<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Listes standards>

  Les listes standards de <TeXmacs> sont d�finies dans <tmdtd|std-list>. Les
  listes non num�rot�es sont :

  <\explain|<markup|itemize>>
    La marque devant chaque article d�pend de la profondeur de l'imbrication.
  </explain>

  <\explain|<markup|itemize-minus>>
    La marque est un tiret.
  </explain>

  <\explain|<markup|itemize-dot>>
    La marque est une puce.
  </explain>

  <\explain|<markup|itemize-arrow>>
    La marque est une fl�che.
  </explain>

  Les listes num�rot�es correspondent aux environnements suivants :

  <\explain|<markup|enumerate>>
    Le style des nombres devant chaque article d�pend de la profondeur de
    l'imbrication.
  </explain>

  <\explain|<markup|enumerate-numeric>>
    Num�rotation de style 1, 2, 3, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-roman>>
    Num�rotation de style i, ii, iii, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-Roman>>
    Num�rotation de style I, II, III, <abbr|etc...>
  </explain>

  <\explain|<markup|enumerate-alpha>>
    Num�rotation de style a), b), c), <abbr|etc.>..
  </explain>

  <\explain|<markup|enumerate-Alpha>>
    Num�rotation de style A, B, C, <abbr|etc.>..
  </explain>

  Les environnements suivants sont utilis�s pour les listes descriptives.

  <\explain|<markup|description>>
    Environnement par d�faut pour les descriptions (en g�n�ral
    <markup|description-compact>).
  </explain>

  <\explain|<markup|description-compact>>
    Aligne les articles � gauche et les fait suivre imm�diatement de leur
    description.
  </explain>

  <\explain|<markup|description-dash>>
    Identique � <markup|description-compact>, mais utilise un tiret pour
    s�parer chaque article de sa description.
  </explain>

  <\explain|<markup|description-align>>
    Aligne les articles � droite et leur description � gauche.
  </explain>

  <\explain|<markup|description-long>>
    Les articles et leur description sont affich�s sur deux lignes
    diff�rentes.
  </explain>

  Un nouvel article dans une liste est rep�r� par les balises <markup|item>,
  ou <markup|item*> dans le cas de descriptions. Les d�veloppeurs trouveront
  quelques autres macros instables dans <tmdtd|std-list> qui leur permettront
  de d�finir d'autres types de listes.

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