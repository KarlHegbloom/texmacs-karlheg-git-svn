<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Titres standards>

  Le d.t.d. <tmdtd|header-title> d�finit les balises pour les titres. Les
  balises suivantes de haut niveau ne peuvent �tre utilis�es qu'� l'int�rieur
  d'une balise <markup|make-title> :

  <\explain|<markup|title>>
    D�finit le titre d'un document.
  </explain>

  <\explain|<markup|author>>
    D�finit le(s) auteur(s) du document.
  </explain>

  <\explain|<markup|address>>
    D�finit l'adresse de l'auteur.
  </explain>

  <\explain|<markup|address-block>>
    D�finit l'adresse d'un auteur (en cas d'adresses multiples).
  </explain>

  <\explain|<markup|title-email>>
    D�finit l'adresse email d'un auteur.
  </explain>

  <\explain|<markup|title-date>>
    D�finit la date de cr�ation de l'article.
  </explain>

  Les balises <markup|title> et <markup|author> utilisent les balises
  <markup|header-title> et <markup|header-author> pour sp�cifier le titre et
  l'en-t�te courants. On peut les changer en r�utilisant les balises
  <markup|header-title> <abbr|resp.> <markup|header-author>. La mise en page
  physique des balises ci-dessus d�pend des balises de bas niveau suivantes :

  <\explain|<markup|title*>>
    Macro avec un argument qui sp�cifie la mise en page physique des titres.
  </explain>

  <\explain|<markup|author*>>
    Macro avec un argument qui sp�cifie la mise en page physique des auteurs.
  </explain>

  <\explain|<markup|address*>>
    Macro avec un argument qui sp�cifie la mise en page physique des
    adresses.
  </explain>

  <\explain|<markup|title-email*>>
    Macro avec un argument qui sp�cifie la mise en page physique des adresses
    email.
  </explain>

  <\explain|<markup|title-date*>>
    Macro avec un argument qui sp�cifie la mise en page physique des dates de
    cr�ation.
  </explain>

  Le d.t.d. <tmdtd|header-title> d�finit aussi la balise <markup|abstract>
  pour les r�sum�s de documents.

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