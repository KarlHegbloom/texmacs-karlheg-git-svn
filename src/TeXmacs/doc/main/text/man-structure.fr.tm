<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Structure d'un texte>

  En g�n�ral, les documents importants ont une structure. Ils sont organis�s
  en chapitres, sections et sous-sections ; ils contiennent diff�rentes
  sortes de texte, comme du texte ordinaire, des citations, des notes de bas
  de page, des th�or�mes, etc... Apr�s que vous avez choisi un
  <def-index|style de document> dans <menu|Document|Style>, <TeXmacs> se
  charge de la mise en page, telles la num�rotation des sections, pages et
  th�or�mes, la typographie des citations, notes en bas de page et th�or�mes.

  Actuellement plusieurs styles de document ont �t� impl�ment�s :
  <verbatim|<with|color|dark orange|<with|color|brown|g�n�rique>>>,
  <verbatim|<with|color|brown|article>>, <tmstyle|livre>, <tmstyle|lettre>,
  <tmstyle|examen>, <tmstyle|beamer>, <tmstyle|s�minaire>, <tmstyle|source>.
  Par exemple le style article peut �tre utilis� pour l'�criture d'articles.
  Par ailleurs, il y a des styles pour les revues communes et d'autres pour
  des fins sp�ciales telle que la documentation de <TeXmacs>.

  D�s que vous avez s�lectionn� un style, vous pouvez organiser votre texte
  en sections (voir <menu|Ins�rer|Section>) et utiliser des
  <def-index|environnements> sp�cifiques. Par exemple un th�or�me, une
  proposition, une remarque... (voir <menu|Ins�rer|Enunciation>). Ou encore
  des listes ordinaires (voir <menu|Ins�rer|Liste>) ou des listes num�rot�es
  (voir <menu|Ins�rer|�numeration>). D'autres exemples de balises fr�quemment
  utilis�s sont <with|color|blue|Important> (pour l'�criture de texte \S
  important \T), <with|color|blue|<samp|Nom>> ( pour l'�criture de nom de
  personnes), etc.

  Quand vous vous sentirez plus � l'aise avec <TeXmacs>, vous pourrez ajouter
  de nouveaux environnements dans un fichier de style personnalis�.
  Supposons, par exemple, que vous faites de nombreuses citations et que vous
  voulez qu'elles apparaissent en italique avec des marges gauche et droite
  d'un centim�tre. Au lieu de changer manuellement les propri�t�s du texte et
  du paragraphe � chaque fois que vous faites une citation, il vaut mieux
  cr�er un environnement citation. Cela vous permettra non seulement
  d'ins�rer plus vite une citation, mais aussi de changer syst�matiquement la
  mise en page de toutes vos citations dans le document en ne changeant que
  la d�finition de l'environnement citation. Vous vous trouverez dans ce cas,
  lorsque vous vous rendrez compte <with|font-shape|italic|a posteriori>
  qu'il vaudrait mieux, par exemple, utiliser une police plus petite pour
  afficher les citations.

  Le respect de quelques principes g�n�raux de l'�dition de texte rend facile
  la manipulation de documents structur�s � l'aide de <TeXmacs>. Un concept
  majeur est celui <em|d'environnement courant>, qui s'illustre mieux \ �
  travers un exemple.

  Supposons que nous soyons entrain de saisir un th�or�me classique:

  <\quote-env>
    Le th�or�me suivant est d� � <name|Euler>:

    <\big-focus>
      <\theorem>
        <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<value|math-cursor>>>.
      </theorem>
    </big-focus>
  </quote-env>

  A la position du curseur, les bo�tes bleu cyan indiquent les balises
  actives : Dans ce cas, le curseur est � la fois dans un environnement
  th�or�me et formule. L'environnement active le plus intime (celui de la
  \ formule <small-focus|<math|\<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1>>
  dans notre cas) est appel� l'<em|environnement courant>.

  Le contenu du menu <samp|Focus> et de la <em|barre d'outils focus> (la
  barre d'outils la plus basse) depend fortement du contexte et est une
  fonction de l'environnement courant. Dans notre exemple, la barre d'outils
  focus contient un bouton menu <samp|Formule>; en cliquant sur
  <samp|Equation> dans ce menu, nous obtenons :

  <\quote-env>
    Le th�or�me suivant est d� � <name|Euler>:

    <\big-focus>
      <\theorem>
        \;

        <\big-focus>
          <\equation*>
            \<mathe\><rsup|\<mathpi\>*\<mathi\>>=\<um\>1<value|math-cursor>.
          </equation*>
        </big-focus>
      </theorem>
    </big-focus>
  </quote-env>

  Les boutons se trouvant � gauche de la barre d'outils focus permettent de
  passer rapidement d'une balise donn�es � une autre qui lui est similaire.
  Ainsi, ils vous permettrons de parcourir rapidement toutes les formules et
  �quations dans votre document. Pour plus d'information sur \S <hlink|les
  op�rations d'�dition structur�e|../editing/man-structured-editing.fr.tm> \T
  nous renvoyons au chapitre sur <hlink|outils
  d'�dition|../editing/man-editing-tools.fr.tm>.

  Une deuxi�me notion importante est <em|le mode d'�dition courant>.
  Actuellement, il y'a cinq principaux modes d'�dition : texte,
  math�matiques, programme, graphique et source. En principe, le mode
  d'�dition courant peut �tre d�termin� � partir de l'environnement courant,
  mais le mode change moins souvent que l'environnement. La <em|barre
  d'outils mode> au-dessus de la barre d'outils focus contient plusieurs
  boutons qui sont utiles dans le mode courant. Les contenus des menus
  <samp|Ins�rer> et <samp|Format> d�pendent aussi du mode courant.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Mich�le Garoche, Daouda
  Niang Diatta>

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