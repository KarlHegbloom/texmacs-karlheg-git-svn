<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|D�finition de nouveaux environnements>

  Le fichier <tmdtd|env-manage> contient des balises de haut niveau qui
  peuvent �tre utilis�es pour d�finir de nouveaux environnements de
  th�or�mes, de remarques, d'exercices et de figures :

  <\description>
    <expand|item*|<markup|newtheorem>>D�finit un environnement th�or�me. Vous
    devez sp�cifier un nom d'environnement (tel
    <space|0.2spc>exp�rimentation<space|0.2spc>) et le texte correspondant
    (tel <space|0.2spc>Exp�rimentation<space|0.2spc>).

    <expand|item*|<markup|newremark>>Identique � <markup|newtheorem> pour les
    remarques.

    <expand|item*|<markup|newexercise>>Identique � <markup|newtheorem> pour
    les exercices.

    <expand|item*|<markup|newfigure>>Identique � <markup|newtheorem> pour les
    figures (toujours par paire : petite et grande).
  </description>

  Le <abbr|d.t.d.> contient aussi des balises de bas niveau pour d�finir les
  environnements. En fait, la d�finition de nouveaux th�or�mes se fait en
  deux �tapes. Dans la premi�re �tape, la balise <markup|newtheorem> est
  utilis�e pour indiquer quel type d'environnement th�or�me sera d�fini. Dans
  une seconde �tape, qui a lieu juste avant que le document utilisateur soit
  compil�, les environnements th�or�mes sont effectivement d�finis. Ce
  m�canisme permet de personnaliser les environnements dans des packages qui
  sont mis en route entre les deux �tapes. Par exemple, la num�rotation des
  th�or�mes est faite de cette fa�on.

  <\warning>
    � l'heure actuelle, nous ne devez utiliser la balise <markup|newtheorem>
    et les balises similaires qu'� l'int�rieur d'un fichier de style
    personnalis� ou d'un package. Si vous utilisez <markup|newtheorem>
    directement � l'int�rieur d'un document, la num�rotation sera incorrecte,
    compte tenu du m�canisme en deux �tapes expliqu� ci-dessus. Ce probl�me
    sera r�solu lorsqu'on pourra sp�cifier des pr�ambules corrects pour les
    documents <TeXmacs>.
  </warning>

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
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-manage>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newremark>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newexercise>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newfigure>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
