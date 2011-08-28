<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Navigation dans la documentation <TeXmacs>>

  En r�gle g�n�rale, vous devez �viter d'utiliser des sections dans la
  documentation <TeXmacs> et essayer d'�crire des pages d'aide courtes sur
  des sujets bien pr�cis. Ensuite, vous devez cr�er des
  <space|0.2spc>m�ta-fichiers d'aide<space|0.2spc> qui indiquent comment
  naviguer automatiquement dans la documentation. Ceci permet le r�emploi
  d'une page d'aide pour diff�rents usages (un livre destin� � l'impression,
  un tutoriel sur un site web, etc...).

  Le style <tmstyle|tmdoc> fournit trois macros de marquage pour indiquer
  comment naviguer dans la documentation. La macro <markup|traverse> est
  utilis�e pour d�limiter des r�gions de navigation. La macro <markup|branch>
  signale une page d'aide qui doit �tre consid�r�e comme sous-section et la
  macro <markup|continue> indique une page de suite. Les macros
  <markup|branch> et <markup|continue> prennent deux arguments. Le premier
  argument d�crit le lien, le second donne l'adresse physique relative du
  fichier li�.

  En g�n�ral, � la fin d'un m�ta-fichier d'aide, on trouve plusieurs macros
  <markup|branch> ou <markup|continue> � l'int�rieur d'une macro
  <markup|traverse>. Vous devez aussi indiquer en haut du document son titre
  avec la macro <markup|tmdoc-title>. Lors de la cr�ation automatique d'un
  livre destin� � l'impression, une structure chapitre-section-sous-section
  sera automatiquement g�n�r�e � partir de ces informations et des titres des
  documents. On peut aussi cr�er automatiquement des boutons de navigation
  pour usage dans un navigateur.

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
    <associate|idx-10|<tuple|<uninit>|?>>
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
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|tmdoc>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|branch>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|continue>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|traverse>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|tmdoc-title>>|<pageref|idx-10>>
    </associate>
  </collection>
</auxiliary>
