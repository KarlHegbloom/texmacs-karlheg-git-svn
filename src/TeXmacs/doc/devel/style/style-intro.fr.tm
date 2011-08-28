<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Fichiers de style <TeXmacs>>

  Une des caract�ristiques fondamentales de <TeXmacs> est la possibilit�
  d'�crire des fichiers de style personnalis�s et des modules. L'utilit� des
  fichiers de style est multiple :

  <\itemize>
    <item>Permettre l'abstraction d'�l�ments r�p�titifs dans les textes :
    sections, th�or�mes, �num�rations, etc...

    <item>Cr�er un m�canisme de structuration du texte. Par exemple, on peut
    indiquer qu'une partie du texte est une abr�viation, une citation ou est
    <space|0.2spc>importante<space|0.2spc>.

    <item>�crire des documents de qualit� professionnelle. C'est ce � quoi
    servent les styles de documents standards ; ils ont �t� �crits avec
    beaucoup de soin par des personnes vers�es dans l'art de la typographie
    et l'esth�tisme.
  </itemize>

  Il est possible d'associer � un document un ou plusieurs styles de
  document, qu'ils soient standards ou personnalis�s. Le style principal d'un
  document est d�fini avec <apply|menu|Document|Style>. D'autres styles
  peuvent �tre ajout�s avec <apply|menu|Document|Use package>.

  Pour l'�diteur, chaque style correspond � un fichier <verbatim|.ts>. Les
  fichiers correspondant � chaque style sont trait�s comme des documents
  ordinaires, mais, � la fin, l'�diteur ne conserve que l'environnement
  final, qui sert alors d'environnement initial au document principal. Plus
  pr�cis�ment, les fichiers de style sont trait�s dans l'ordre, ainsi que
  leurs propres fichiers de style, de fa�on r�cursive.

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

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Utiliser paquetage>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
