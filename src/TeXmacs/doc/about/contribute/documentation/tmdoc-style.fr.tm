<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Utilisation du style tmdoc>

  En plus des macros <hlink|droits d'auteur|copyright.fr.tm> et
  <hlink|navigation|traversal.fr.tm>, qui ont d�j� �t� expliqu�es, le style
  <tmstyle|tmdoc> contient un certain nombre d'autres macros et fonctions que
  vous pouvez utiliser si n�cessaire :

  <\explain|<markup|key>>
    Cette macro est utilis�e pour signaler des saisies clavier, telle
    <shortcut|(save-buffer)>. Les macros sp�cifiques <markup|kbd-gen>,
    <markup|kbd-text>, <markup|kbd-math>, <markup|kbd-symb>,
    <markup|kbd-big>, <markup|kbd-large>, <markup|kbd-ia>, <markup|kbd-exec>
    \ et <markup|kbd-table> sont utilis�es pour les saisies clavier
    correspondant � un type sp�cifique d'action ou de mode. Par exemple, la
    macro <markup|kbd-math> correspond aux raccourcis clavier pour les
    op�rations math�matiques, tel <key|math f>, qui d�bute une fraction.
  </explain>

  <\explain|<markup|menu>>
    Cette fonction, compos�e d'un nombre arbitraire d'arguments, fait
    r�f�rence � un menu, tel <menu|File> ou <menu|Document|Language>. Les
    articles de menu sont automatiquement traduits par cette fonction.
  </explain>

  <\explain|<markup|markup>>
    Cette macro est utilis�e pour signaler une macro ou une fonction, telle
    <markup|section>.
  </explain>

  <\explain|<markup|tmstyle>>
    Cette macro indique le nom d'un fichier de style <TeXmacs> ou un module,
    tel <tmstyle|article>.
  </explain>

  <\explain|<markup|tmpackage>>
    Cette macro indique le nom d'un package, tel <tmstyle|std-markup>.
  </explain>

  <\explain|<markup|tmdtd>>
    Cette macro indique le nom d'un d.t.d. <TeXmacs>, tel <tmdtd|number-env>.
  </explain>

  Attention, aucune des marques ci-dessus ne doit �tre traduite. En effet,
  les marques de menus sont automatiquement traduites, de fa�on � assurer la
  synchronisation de leur traduction avec la traduction actuelle des menus de
  <TeXmacs>. En ce qui concerne les marques, styles, packages et
  <abbr|d.t.d.>s, il faut absolument garder le nom original, car il
  correspond souvent au nom d'un fichier.\ 

  Les macros et fonctions suivantes sont utilis�es pour les liens et les
  index ; elles seront am�lior�es plus tard :

  <\explain|<markup|simple-link>>
    Cette macro a pour argument <math|x> une URL et g�n�re un hyperlien de
    nom et destination <math|x>.
  </explain>

  <\explain|<markup|hyper-link>>
    Cette macro correspond � un hyperlien.
  </explain>

  <\explain|<markup|concept-link>>
    Cette macro a pour argument un concept. Plus tard, un hyperlien pourra
    �tre cr�� automatiquement � partir du concept et du reste de la
    documentation.
  </explain>

  <\explain|<markup|only-index>>
    Indexe une cha�ne de caract�res.
  </explain>

  <\explain|<markup|def-index>>
    D�finit un nouveau concept ; le texte est imprim� en italique et index�.
  </explain>

  <\explain|<markup|re-index>>
    R�utilise un concept d�j� d�fini ; le texte est imprim� en roman et mis
    dans l'index.
  </explain>

  Les marques suivantes sont aussi assez fr�quemment utilis�es :

  <\explain|<markup|icon>>
    Lien vers une ic�ne situ�e dans un r�pertoire central, tel
    \ <verbatim|$TEXMACS_PATH/doc/images/pixmaps>.
  </explain>

  <\explain|<markup|screenshot>>
    Lien vers une capture d'�cran. Les captures d'�cran sont stock�es dans
    une r�pertoire central, tel <verbatim|$TEXMACS_PATH/doc/images/screenshots>.
  </explain>

  <\explain|<markup|scheme>>
    Le language <scheme>.
  </explain>

  <\explain|<markup|framed-fragment>>
    Pour afficher un fragment de code dans un cadre appropri�.
  </explain>

  <\explain|<markup|scheme-fragment>>
    Pour du code <scheme> sur plusieurs paragraphes.
  </explain>

  <\explain|<markup|tm-fragment>>
    Pour marquer du code <TeXmacs> en format <scheme>.
  </explain>

  <\explain|<markup|descriptive-table>>
    Pour les tables de description ; on peut utiliser ces tables pour
    documenter des listes de raccourcis clavier, diff�rents types de
    marquage, etc...
  </explain>

  The style <tmstyle|tmdoc> h�rite du style <tmstyle|generic>. Vous devez
  utiliser les macros <markup|em>, <markup|verbatim>, <markup|itemize>,
  <abbr|etc.> contenues dans ce style quand le cas se pr�sente.

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