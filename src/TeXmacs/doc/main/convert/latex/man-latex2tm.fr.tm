<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion de <LaTeX> � <TeXmacs>>

  Le but du programme de conversion de <apply|LaTeX> � <apply|TeXmacs> est de
  vous <with|font shape|italic|aider> � convertir vos documents au format
  <apply|TeXmacs>. <with|font shape|italic|De fait>, il est plus difficile de
  convertir de <apply|LaTeX> � <apply|TeXmacs> que l'inverse. N�anmoins, si
  vous n'utilisez que les commandes les plus courantes de <apply|LaTeX>, vous
  devriez pouvoir convertir vos documents de fa�on satisfaisante. Par
  exemple, tous les fichiers d'aide de <apply|TeXmacs> ont �t� �crits sous
  <apply|LaTeX> de fa�on � valider le programme de conversion de
  <apply|LaTeX> � <apply|TeXmacs>.

  Vous pouvez convertir un document <apply|LaTeX> nomm� <verbatim|name.tex>
  en un document <apply|TeXmacs> avec <apply|menu|File|Import|Latex> et le
  sauvegarder sous le nom <verbatim|name.tm>. Si votre document <apply|LaTeX>
  a �t� �crit correctement, le r�sultat devrait �tre � peu pr�s satisfaisant,
  � l'exception de certaines commandes non reconnues, commandes qui
  appara�tront en rouge. La meilleure solution est de cr�er vos propres
  fichiers de style pour les documents convertis ; dans ces fichiers de
  style, vous d�finirez les commandes non reconnues en vous basant sur le
  style de d�part.

  H�las, dans certains cas, les documents convertis ne sont pas exploitables.
  Cela provient du fait que, dans <apply|TeX> et <apply|LaTeX>, vous pouvez
  modifier dynamiquement l'analyseur syntaxique en utilisant la commande
  <verbatim|\\catcode> par exemple. Dans ce cas, le programme de conversion
  peut faire des choix erron�s sur le mode ou l'environnement. Il en r�sulte
  que du texte peut �tre converti en maths, des maths en verbatim, etc...
  N�anmoins, vous pouvez rep�rer assez facilement les commandes concern�es
  dans le fichier source <apply|LaTeX> <verbatim|name.tex> en le comparant au
  fichier <apply|TeXmacs> converti. Moyennant quelques recherches dans le
  fichier source et suppression du code litigieux, vous devriez pouvoir
  obtenir un document converti correct.

  Nous avons pr�vu d'incorporer au programme de conversion un convertisseur
  de fichiers de style ainsi que quelques autres modules qui faciliteront la
  conversion des commandes d�finies par l'utilisateur dans un document autre
  que celui qui est converti.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>|<with|font
      family|<quote|ss>|Importer>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
