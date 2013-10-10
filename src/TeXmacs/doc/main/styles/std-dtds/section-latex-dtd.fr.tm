<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Sections de style <LaTeX>>

  Le d.t.d. <tmdtd|section-latex> d�finit les balises standards de section,
  qui sont les m�mes que dans <LaTeX>. La plupart des balises de section ne
  prennent qu'un argument : le nom de la section. Nous avons pr�vu
  d'impl�menter d'autres balises � deux arguments qui permettront de
  consid�rer le corps de la section comme faisant partie de la structure. Les
  balises suivantes correspondent g�n�ralement � des sections num�rot�es qui
  sont r�f�renc�es dans la table des mati�res :

  <\explain|<markup|chapter>>
    Macro pour cr�er un titre de chapitre qui peut �tre num�rot�.
  </explain>

  <\explain|<markup|section>>
    Macro pour cr�er un titre de section qui peut �tre num�rot�e.
  </explain>

  <\explain|<markup|subsection>>
    Macro pour cr�er un titre de sous-section qui peut �tre num�rot�e.
  </explain>

  <\explain|<markup|subsubsection>>
    Macro pour cr�er un titre de sous-sous-section qui peut �tre num�rot�e.
  </explain>

  <\explain|<markup|paragraph>>
    Macro pour cr�er un titre de paragraphe qui peut �tre num�rot�.
  </explain>

  <\explain|<markup|subparagraph>>
    Macro pour cr�er un titre de sous-paragraphe qui peut �tre num�rot�.
  </explain>

  Les balises <markup|chapter*>, <markup|section*>, <markup|subsection*>,
  <markup|subsubsection*>, <markup|paragraph*> et <markup|subparagraph*>
  peuvent �tre utilis�es pour cr�er les variantes non num�rot�es des balises
  ci-dessus, qui ne sont pas r�f�renc�es dans la table des mati�res. Le
  d.t.d. <tmdtd|section-latex> d�finit aussi les balises suivantes :

  <\explain|<markup|chapter**>>
    Macro � deux arguments : un type de chapitre sp�cial (tel
    \S<space|0.2spc>�pilogue<space|0.2spc>\T) et le nom du chapitre.
  </explain>

  <\explain|<markup|appendix>>
    Variante de <markup|chapter> ou <markup|section> pour cr�er des annexes.
  </explain>

  <\explain|<markup|section-sep>>
    Macro pour personnaliser le s�parateur entre le num�ro de la section et
    son titre. Par d�faut, on utilise deux espaces.
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