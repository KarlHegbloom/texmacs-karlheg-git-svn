<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Sections de style <LaTeX>>

  Le d.t.d. <tmdtd|section-latex> d�finit les balises standards de section,
  qui sont les m�mes que dans <LaTeX>. La plupart des balises de section ne
  prennent qu'un argument : le nom de la section. Nous avons pr�vu
  d'impl�menter d'autres balises � deux arguments qui permettront de
  consid�rer le corps de la section comme faisant partie de la structure. Les
  balises suivantes correspondent g�n�ralement � des sections num�rot�es qui
  sont r�f�renc�es dans la table des mati�res :

  <\description>
    <expand|item*|<markup|chapter>>Macro pour cr�er un titre de chapitre qui
    peut �tre num�rot�.

    <expand|item*|<markup|section>>Macro pour cr�er un titre de section qui
    peut �tre num�rot�e.

    <expand|item*|<markup|subsection>>Macro pour cr�er un titre de
    sous-section qui peut �tre num�rot�e.

    <expand|item*|<markup|subsubsection>>Macro pour cr�er un titre de
    sous-sous-section qui peut �tre num�rot�e.

    <expand|item*|<markup|paragraph>>Macro pour cr�er un titre de paragraphe
    qui peut �tre num�rot�.

    <expand|item*|<markup|subparagraph>>Macro pour cr�er un titre de
    sous-paragraphe qui peut �tre num�rot�.
  </description>

  Les balises <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> et <markup|subparagraph*>
  peuvent �tre utilis�es pour cr�er les variantes non num�rot�es des balises
  ci-dessus, qui ne sont pas r�f�renc�es dans la table des mati�res. Le
  d.t.d. <tmdtd|section-latex> d�finit aussi les balises suivantes :

  <\description>
    <expand|item*|<markup|chapter**>>Macro � deux arguments : un type de
    chapitre sp�cial (tel <space|0.2spc>�pilogue<space|0.2spc>) et le nom
    du chapitre.

    <expand|item*|<markup|appendix>>Variante de <markup|chapter> ou
    <markup|section> pour cr�er des annexes.

    <expand|item*|<markup|sectionsep>>Macro pour personnaliser le s�parateur
    entre le num�ro de la section et son titre. Par d�faut, on utilise deux
    espaces.
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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|section-latex>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubsection>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|paragraph>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subparagraph>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection*>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsubsection*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|paragraph*>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subparagraph*>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|section-latex>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter**>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|appendix>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|sectionsep>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
