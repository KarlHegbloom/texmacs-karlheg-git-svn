<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion de <TeXmacs> � <LaTeX>>

  Le plus souvent, vous souhaiterez convertir un article de <apply|TeXmacs> �
  <apply|LaTeX> pour le soumettre � un journal. Vous pouvez convertir un
  article <apply|TeXmacs> nomm� <verbatim|name.tm> en un fichier
  <apply|LaTeX> file nomm� <verbatim|name.tex> avec
  <apply|menu|File|Export|Latex>. Ensuite, chargez le fichier
  <verbatim|name.tex> dans <apply|LaTeX> et voyez si vous obtenez un r�sultat
  satisfaisant. Si c'est le cas, vous pouvez soumettre au journal le fichier
  <verbatim|name.tex> et le fichier de style <verbatim|TeXmacs.sty>, que vous
  trouverez dans le dossier <verbatim|$TEXMACS_PATH/misc/latex>.

  Le journal auquel vous soumettez votre article utilise, en g�n�ral, son
  propre fichier de style, disons <verbatim|journal.sty>. Dans ce cas, vous
  devrez aussi copier le fichier :

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  dans :

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  et utiliser <verbatim|journal> comme style de document avec
  <apply|menu|Document|Style|Other style>. Vous pouvez aussi modifier
  <verbatim|journal.ts>, de fa�on � ce que la mise en page de votre article
  s'approche le plus possible de celle du journal. Dans certains cas, vous
  devrez aussi dupliquer <verbatim|TeXmacs.sty> et changer certains
  environnements pour qu'ils soient compatibles avec le fichier de style du
  journal <verbatim|journal.sty>.

  Quand le chargement du document converti dans <apply|LaTeX> aboutit � un
  r�sultat non satisfaisant, vous observerez qu'en g�n�ral seules certaines
  parties du texte sont affect�es. Ceci est d�, la plupart du temps, aux
  trois raisons suivantes :\ 

  <\itemize>
    <item>Vous utilisez des propri�t�s sp�cifiques � <apply|TeXmacs>.

    <item>Vous utilisez des propri�t�s de <apply|TeXmacs> qui n'ont pas
    encore �t� impl�ment�es dans l'algorithme de conversion.

    <item>Vous �tes tomb�(e) sur un bogue de l'algorithme de conversion.
  </itemize>

  Ces probl�mes seront �voqu�s plus en d�tail dans la section suivante.

  En cas de probl�mes, vous pourriez �tre tent�(e) de corriger le fichier
  <apply|LaTeX> obtenu par conversion et l'envoyer ainsi au journal.
  N�anmoins, cette fa�on de faire vous conduira vite dans une impasse ; vous
  devrez refaire les corrections � chaque fois que vous convertirez le
  fichier <apply|TeXmacs> <verbatim|name.tm>, apr�s lui avoir fait subir
  quelque changement que ce soit. Il vaut mieux utiliser
  <apply|menu|Insert|Specific|Latex> et <apply|menu|Insert|Specific|Texmacs>
  pour saisir du texte visible dans le fichier converti ou le fichier
  original.

  Par exemple, supposons que le mot <space|0.2spc>anticonstitutionnellement<space|0.2spc>
  a une c�sure correcte dans le fichier source <apply|TeXmacs>, mais pas dans
  le document converti <apply|LaTeX>. Vous proc�derez comme ci-dessous pour
  que la c�sure soit correcte dans les deux fichiers :\ 

  <\enumerate>
    <item>S�lectionnez <space|0.2spc>anticonstitutionnellement<space|0.2spc>.

    <item>Cliquez sur <apply|menu|Insert|Specific|Texmacs> comme sp�cifique �
    <apply|TeXmacs>.

    <item>Cliquez sur <apply|menu|Insert|Specific|Latex>.

    <item>Saisissez le code latex suivant avec la c�sure correcte :
    <verbatim|an\\-ti\\-con\\-sti\\-tu\\-tion\\-nel\\-le\\-ment>.

    <item>Appuyez sur <key|retour chariot> pour activer le texte sp�cifique �
    <apply|LaTeX>.
  </enumerate>

  De m�me, vous pouvez ins�rer des sauts de ligne, sauts de page, espaces
  verticales, des changements de param�tres, etc... sp�cifiques �
  <apply|LaTeX>.

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
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichier>|<with|font
      family|<quote|ss>|Exporter>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>|<with|font
      family|<quote|ss>|Autre>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Sp�cifique>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Sp�cifique>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Sp�cifique>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Sp�cifique>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
