<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|En-t�tes standards>

  Le d.t.d. <tmdtd|header> d�finit les balises de personnalisation des
  en-t�tes et pieds de page. Cette personnalisation est bas�e sur l'id�e que
  l'on peut d�finir un <em|texte de page> pour chaque page. Ce texte de page
  peut �tre, par exemple, un titre courant ou le nom de la section courante.
  Le texte de page peut d�pendre de la parit� du num�ro de page et appara�tre
  diff�remment pour certaines pages, comme celles qui d�butent un chapitre.
  Les balises suivantes g�rent la disposition physique des diff�rents types
  de pages :

  <\description>
    <expand|item*|<markup|start-page>>Cette balise, qui a pour seul argument
    le texte de page, g�re la disposition de la premi�re page d'un chapitre
    ou d'une section.

    <expand|item*|<markup|odd-page-text>>Identique � <markup|start-page> pour
    la disposition des pages impaires ordinaires.

    <expand|item*|<markup|even-page-text>>Identique � <markup|start-page>
    pour la disposition des pages paires ordinaires.
  </description>

  Les balises suivantes g�rent les actions logiques relatives aux en-t�tes
  qui sont ex�cut�es lors de la d�finition d'un titre, d'un auteur ou au
  d�but d'une nouvelle section.

  <expand|item*|<markup|header-title>>Une balise avec un
  <space|0.2spc>argument titre<space|0.2spc> qui est utilis� lors de la
  sp�cification du titre du document.

  <expand|item*|<markup|header-author>>Une balise avec un
  <space|0.2spc>argument auteur<space|0.2spc> qui est utilis� lors de la
  sp�cification de l'auteur du document.

  <expand|item*|<markup|header-primary>>Une balise avec un
  <space|0.2spc>argument de nom de section<space|0.2spc> qui est utilis� au
  d�but de chaque section de base (c'est-�-dire <markup|chapter> pour le
  style livre ou <markup|section> pour le style article).

  <expand|item*|<markup|header-secondary>>Une balise avec un
  <space|0.2spc>argument de nom de section<space|0.2spc> qui est utilis� au
  d�but de chaque section secondaire (c'est-�-dire <markup|section> pour le
  style livre et <markup|subsection> pour le style article).

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
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
      magenta>|header>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|odd-page-text>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|even-page-text>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|start-page>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-title>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-author>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-primary>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|chapter>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|header-secondary>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|section>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|subsection>>|<pageref|idx-14>>
    </associate>
  </collection>
</auxiliary>
