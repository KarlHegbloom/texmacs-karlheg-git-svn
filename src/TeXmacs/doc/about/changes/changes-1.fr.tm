<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Format de document (0.3.4)>

  Le format de document TeXmacs a profond�ment chang� pour rendre TeXmacs
  compatible avec XML � l'avenir. Tout d'abord, les environnements de style :

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>environment\|open\|close\<gtr\>\<gtr\>,
  </verbatim>

  qui �taient appliqu�s par paires avec <verbatim|\<less\>begin\|env\<gtr\>text\<less\>end\|env\<gtr\>>,
  ont �t� remplac�s par des macros :

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>macro\|body\|open\<less\>body\<gtr\>close\<gtr\>\<gtr\>,
  </verbatim>

  qui sont appliqu�s via une simple expansion de macros
  <verbatim|\<less\>expand\|env\|text\<gtr\>>. De m�me, les paires
  <verbatim|\<less\>set\|var\|val\<gtr\>text\<less\>reset\|var\<gtr\>> de
  changement de variables d'environnement ont �t� remplac�es par une
  construction <verbatim|\<less\>with\|var\|val\|text\<gtr\>> (proche des
  attributs XML). D'un point de vue technique, ces changements conduisent �
  de s�rieux probl�mes quand le corps de <verbatim|text> est constitu� de
  plusieurs paragraphes. Il en r�sulte que des documents mal structur�s
  peuvent parfois s'afficher tout � fait diff�remment dans la nouvelle
  version (bien que je n'ai not� personnellement que des changements mineurs
  dans mes propres documents). De plus, le comportement de l'�diteur par
  rapport aux environnements � paragraphes multiples a l�g�rement chang�,
  pour g�rer un meilleur niveau de structure du document.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
