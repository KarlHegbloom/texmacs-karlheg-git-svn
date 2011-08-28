<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conventions de noms de fichiers>

  La majeure partie des documents doivent �tre ins�r�s en fonction d'un sujet
  dans une arborescence de r�pertoires. Les sous-r�pertoires du r�pertoire
  principal sont les suivants :

  <\description>
    <expand|item*|devel>Documentation pour d�veloppeurs.

    <expand|item*|examples>Exemples de documents <TeXmacs>.

    <expand|item*|incoming>Documents en devenir, sujets � variation.

    <expand|item*|main>Documentation principale.

    <expand|item*|meta>Documentation sur la cr�ation et la compilation de
    documents.
  </description>

  Essayez de ne pas cr�er trop de fichiers par r�pertoire.

  Les noms des fichiers dans le r�pertoire main sont du type
  <verbatim|type-nom.langue.tm>. Dans les autres r�pertoires, ils sont de la
  forme <verbatim|nom.langue.tm>. Ici <verbatim|type> repr�sente un certain
  type de documentation ; ce soit �tre l'un des suivants :

  <\description>
    <expand|item*|adv>Documentation pour utilisateurs exp�riment�s.

    <expand|item*|man>Documentation � inclure dans le guide <TeXmacs>.

    <expand|item*|tut>Documentation � inclure dans le tutoriel <TeXmacs>.
  </description>

  Vous devez essayer de regrouper la documentation sur un sujet donn�, quel
  qu'en soit le type. En effet, ceci permet de retrouver plus facilement tous
  les documents existants sur un sujet particulier. Il peut arriver aussi que
  l'on veuille incorporer dans le guide des documents pr�vus au d�part pour
  le tutoriel. La <verbatim|langue> dans laquelle le document a �t� �crit est
  un code de deux lettres, comme par exemple : <verbatim|en>, <verbatim|fr>,
  etc... Le <verbatim|nom> du fichier doit �tre le m�me quelle que soit la
  langue. Par exemple, <verbatim|man-keyboard.en.tm> ne doit pas �tre traduit
  par <verbatim|man-clavier.fr.tm>, mais devenir man-keyboard.fr.tm.

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
