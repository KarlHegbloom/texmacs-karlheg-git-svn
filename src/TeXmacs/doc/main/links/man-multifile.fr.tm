<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Livres et documents ma�tres>

  Quand un document devient tr�s volumineux, vous pouvez le subdiviser en
  plusieurs documents plus petits. Ceci permet de r�utiliser plus facilement
  les composants du document ma�tre dans d'autres ouvrages et am�liore les
  temps de r�action de l'�diteur. On peut ins�rer un fichier dans un autre
  avec <apply|menu|Insert|Link|Include>. Les documents inclus sont mis en
  tampon pour acc�l�rer leur traitement. Utilisez
  <apply|menu|Tools|Update|Inclusions> pour mettre � jour tous les documents
  inclus.

  Quand on �crit un livre, on cr�e, en g�n�ral, autant de fichiers
  <verbatim|c1.tm>, <verbatim|c2.tm> ..., <verbatim|cn.tm> que de chapitres.
  On cr�e, ensuite, un fichier pour le livre <verbatim|book.tm>, dans lequel
  on ins�re les fichiers <verbatim|c1.tm>, <verbatim|c2.tm>, ...,
  <verbatim|cn.tm> en utilisant le m�canisme d�crit ci-dessus. La table des
  mati�res, la bibliographie, etc... sont, en g�n�ral, g�n�r�es dans le
  fichier ma�tre <verbatim|book.tm>.

  Pour afficher les r�f�rences crois�es � d'autres chapitres lorsqu'on �dite
  un fichier <verbatim|ci.tm>, on peut d�finir <verbatim|book.tm> en tant que
  <space|0.2spc>fichier ma�tre<space|0.2spc> des fichiers <verbatim|c1.tm>,
  ..., <verbatim|cn.tm> avec <apply|menu|Document|Master|Attach>. � l'heure
  actuelle, les num�ros de chapitre ne sont pas g�r�s par ce m�canisme. Vous
  devez d�finir la variable d'environnement <verbatim|chapternr> au d�but de
  chaque chapitre pour qu'il soit num�rot� correctement lors de son �dition.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Inclure>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Outils>|<with|font
      family|<quote|ss>|Actualiser>|<with|font
      family|<quote|ss>|Inclusions>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Ma�tre>|<with|font
      family|<quote|ss>|Attacher>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
