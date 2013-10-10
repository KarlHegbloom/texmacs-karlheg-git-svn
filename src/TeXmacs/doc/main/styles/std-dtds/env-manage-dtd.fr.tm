<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|D�finition de nouveaux environnements>

  Le fichier <tmdtd|env-manage> contient des balises de haut niveau qui
  peuvent �tre utilis�es pour d�finir de nouveaux environnements de
  th�or�mes, de remarques, d'exercices et de figures :

  <\explain|<markup|new-theorem>>
    D�finit un environnement th�or�me. Vous devez sp�cifier un nom
    d'environnement (tel \S<space|0.2spc>exp�rimentation<space|0.2spc>\T) et
    le texte correspondant (tel \S<space|0.2spc>Exp�rimentation<space|0.2spc>\T).
  </explain>

  <\explain|<markup|new-remark>>
    Identique � <markup|new-theorem> pour les remarques.
  </explain>

  <\explain|<markup|new-exercise>>
    Identique � <markup|new-theorem> pour les exercices.
  </explain>

  <\explain|<markup|new-figure>>
    Identique � <markup|new-theorem> pour les figures (toujours par paire :
    petite et grande).
  </explain>

  Le <abbr|d.t.d.> contient aussi des balises de bas niveau pour d�finir les
  environnements. En fait, la d�finition de nouveaux th�or�mes se fait en
  deux �tapes. Dans la premi�re �tape, la balise <markup|new-theorem> est
  utilis�e pour indiquer quel type d'environnement th�or�me sera d�fini. Dans
  une seconde �tape, qui a lieu juste avant que le document utilisateur soit
  compil�, les environnements th�or�mes sont effectivement d�finis. Ce
  m�canisme permet de personnaliser les environnements dans des packages qui
  sont mis en route entre les deux �tapes. Par exemple, la num�rotation des
  th�or�mes est faite de cette fa�on.

  <\warning>
    � l'heure actuelle, nous ne devez utiliser la balise <markup|new-theorem>
    et les balises similaires qu'� l'int�rieur d'un fichier de style
    personnalis� ou d'un package. Si vous utilisez <markup|new-theorem>
    directement � l'int�rieur d'un document, la num�rotation sera incorrecte,
    compte tenu du m�canisme en deux �tapes expliqu� ci-dessus. Ce probl�me
    sera r�solu lorsqu'on pourra sp�cifier des pr�ambules corrects pour les
    documents <TeXmacs>.
  </warning>

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