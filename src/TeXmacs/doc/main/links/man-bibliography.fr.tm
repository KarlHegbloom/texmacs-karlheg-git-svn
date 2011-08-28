<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Compilation d'une bibliographie>

  � l'heure actuelle, <apply|TeXmacs> utilise <verbatim|bibtex> pour compiler
  les bibliographies. Le m�canisme de compilation automatique d'une
  bibliographie est le suivant :\ 

  <\itemize>
    <item>Cr�er un fichier <verbatim|.bib> avec toutes vos r�f�rences
    bibliographiques. Ce fichier doit avoir le format d'un fichier
    bibliographique standard pour <apply|LaTeX>.

    <item>Utilisez <apply|menu|Insert|Link|Citation|Visible> et
    <apply|menu|Insert|Link|Citation|Invisible> pour ins�rer les citations
    qui correspondent aux entr�es du fichier<verbatim|.bib>.

    <item>Cliquez sur <apply|menu|Insert|Automatic|Bibliography> � l'endroit o�
    vous voulez que votre bibliographie soit compil�e. \ � l'invite, vous
    devez saisir un style <verbatim|bibtex> (<verbatim|plain>,
    <verbatim|alpha>, <verbatim|abbrv>, etc...) et votre fichier
    <verbatim|.bib>.

    <item>Utilisez <apply|menu|Document|Update|Bibliography> pour compiler
    votre bibliographie.
  </itemize>

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Citation>|<with|font
      family|<quote|ss>|Visible>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Citation>|<with|font
      family|<quote|ss>|Invisible>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Automatique>|<with|font
      family|<quote|ss>|Bibliographie>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Actualiser>|<with|font
      family|<quote|ss>|Bibliographie>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
