<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|En-t�tes standards>

  Le d.t.d. <tmdtd|header> d�finit les balises de personnalisation des
  en-t�tes et pieds de page. Cette personnalisation est bas�e sur l'id�e que
  l'on peut d�finir un <em|texte de page> pour chaque page. Ce texte de page
  peut �tre, par exemple, un titre courant ou le nom de la section courante.
  Le texte de page peut d�pendre de la parit� du num�ro de page et appara�tre
  diff�remment pour certaines pages, comme celles qui d�butent un chapitre.
  Les balises suivantes g�rent la disposition physique des diff�rents types
  de pages :

  <\explain|<markup|start-page>>
    Cette balise, qui a pour seul argument le texte de page, g�re la
    disposition de la premi�re page d'un chapitre ou d'une section.
  </explain>

  <\explain|<markup|odd-page-text>>
    Identique � <markup|start-page> pour la disposition des pages impaires
    ordinaires.
  </explain>

  <\explain|<markup|even-page-text>>
    Identique � <markup|start-page> pour la disposition des pages paires
    ordinaires.
  </explain>

  Les balises suivantes g�rent les actions logiques relatives aux en-t�tes
  qui sont ex�cut�es lors de la d�finition d'un titre, d'un auteur ou au
  d�but d'une nouvelle section.

  <\explain|<markup|header-title>>
    Une balise avec un \S<space|0.2spc>argument titre<space|0.2spc>\T qui est
    utilis� lors de la sp�cification du titre du document.
  </explain>

  <\explain|<markup|header-author>>
    Une balise avec un \S<space|0.2spc>argument auteur<space|0.2spc>\T qui
    est utilis� lors de la sp�cification de l'auteur du document.
  </explain>

  <\explain|<markup|header-primary>>
    Une balise avec un \S<space|0.2spc>argument de nom de
    section<space|0.2spc>\T qui est utilis� au d�but de chaque section de
    base (c'est-�-dire <markup|chapter> pour le style livre ou
    <markup|section> pour le style article).
  </explain>

  <\explain|<markup|header-secondary>>
    Une balise avec un \S<space|0.2spc>argument de nom de
    section<space|0.2spc>\T qui est utilis� au d�but de chaque section
    secondaire (c'est-�-dire <markup|section> pour le style livre et
    <markup|subsection> pour le style article).
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