<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Balises sp�ciales pour les programmes et les sessions>

  Le d.t.d. <tmdtd|program> fournit principalement les environnement pour les
  sessions de calcul formel. Ce sont les suivants :

  <\description>
    <expand|item*|<markup|session>>Macro � trois arguments : le langage de
    calcul formel, le nom de la session et le corps de la session.

    <expand|item*|<markup|input>>Macro � deux arguments : une invite et
    l'entr�e.

    <expand|item*|<markup|output>>Macro qui fournit le corps de la sortie en
    argument.
  </description>

  En fait, ces environnements sont bas�s sur les environnements
  <markup|<em|lan>-session>, <markup|<em|lan>-input> et
  <markup|<em|lan>-output> pour chaque langage <verbatim|<em|lan>>.

  Le d.t.d. <tmdtd|program> d�finit aussi des balises d'affichage de
  programmes informatiques. Mais ces balises sont tr�s instables. On a pr�vu
  de les remplacer par un jeu de balises beaucoup plus �tendu. Les voici :

  <\description>
    <expand|item*|<markup|algorithm>>Macro � deux arguments : le nom de
    l'algorithme et l'algorithme lui-m�me, avec �ventuellement ses
    sp�cifications.

    <expand|item*|<markup|body>>Corps de l'algorithme.

    <expand|item*|<markup|indent>>Pour indenter une partie de l'algorithme.
  </description>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
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
      magenta>|program>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|session>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|input>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|output>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-session>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-input>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|<with|font shape|<quote|italic>|lan>-output>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|program>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|algorithm>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|body>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|indent>>|<pageref|idx-11>>
    </associate>
  </collection>
</auxiliary>
