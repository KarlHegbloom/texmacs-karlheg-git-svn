<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|�tude de l'exemple ``mycas''>

  La meilleure fa�on d'impl�menter votre premi�re interface avec
  <apply|TeXmacs> est d'examiner soigneusement l'exemple <verbatim|mycas>,
  que vous trouverez dans le r�pertoire <verbatim|$TEXMACS_PATH/misc/mycas>.
  Le fichier <verbatim|mycas.cpp>, dont le contenu est inclus � la fin de
  cette section, contient un programme tr�s simple que l'on peut interfacer
  avec <apply|TeXmacs>. Pour tester ce programme, compilez-le avec :

  <\verbatim>
    \ \ \ \ g++ mycas.cpp -o mycas
  </verbatim>

  et d�placez le fichier binaire <verbatim|mycas> obtenu dans un r�pertoire
  connu de la variable d'environnement syst�me PATH. Quand vous d�marrerez
  <apply|TeXmacs>, un nouvel article <apply|menu|Mycas> sera int�gr� dans le
  menu <apply|menu|Insert|Session>.

  NdT: Si vous utilisez le port Fink de <TeXmacs>, le plus simple est de
  copier le fichier <verbatim|mycas.cpp>, situ� dans le r�pertoire
  /sw/share/TeXmacs.../plugins/mycas/examples, dans ~/bin (cr�ez le
  r�pertoire auparavant s'il n'existe pas d�j�), puis compilez-le comment
  indiqu� ci-dessus.

  <section|�tude du code source pas � pas>

  �tudions le code source de <verbatim|mycas> pas � pas. Tout d'abord, toutes
  les communications se font via les entr�es et sorties standards � l'aide de
  tubes. Pour permettre � <apply|TeXmacs> de savoir quand les sorties syst�me
  sont termin�es, toutes les sorties doivent �tre encapsul�es dans des blocs
  contenant trois caract�res de contr�le sp�ciaux :\ 

  <\verbatim>
    \ \ \ \ #define DATA_BEGIN \ \ ((char) 2)<format|next line> \ \ \ #define
    DATA_END \ \ \ \ ((char) 5)<format|next line> \ \ \ #define DATA_ESCAPE
    \ ((char) 27)
  </verbatim>

  Le caract�re <verbatim|DATA_ESCAPE> suivi de n'importe quel autre caract�re
  <with|mode|math|c> est utilis� pour g�n�rer <with|mode|math|c>, y compris
  dans le cas o� <with|mode|math|c> est l'un des trois caract�res de contr�le
  mentionn�s ci-dessus. Le message affich� au d�marrage de la session montre
  comment utiliser <verbatim|DATA_BEGIN> et <verbatim|DATA_END> :\ 

  <\verbatim>
    \ \ \ \ int<format|next line> \ \ \ main () {<format|next line>
    \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ cout \<less\>\<less\> "Welcome to my test computer
    algebra system for TeXmacs\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";<format|next line> \ \ \ \ \ cout \<less\>\<less\> "(c)
    2001 \ by Joris van der Hoeven\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ fflush (stdout);
  </verbatim>

  La premi�re ligne du <verbatim|main> stipule que le message de d�marrage
  sera imprim� en format <space|0.2spc>verbatim<space|0.2spc>. La fonction
  <verbatim|next_input>, qui est appel�e apr�s la sortie du message, est
  utilis�e pour afficher une invite et sera expliqu�e plus loin. Le
  <verbatim|DATA_END> final ferme le bloc de message de d�marrage et indique
  � <apply|TeXmacs> que <verbatim|mycas> est en attente d'entr�e. N'oubliez
  de vider la sortie standard, de fa�on � ce que <apply|TeXmacs> puisse
  recevoir le message dans son entier.

  La boucle principale commence par demander une saisie � partir de l'entr�e
  standard :\ 

  <\verbatim>
    \ \ \ \ \ \ while (1) {<format|next line> \ \ \ \ \ \ \ char
    buffer[100];<format|next line> \ \ \ \ \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ \ \ \ \ if (strcmp (buffer, "quit") == 0)
    break;
  </verbatim>

  La sortie g�n�r�e doit de nouveau figurer dans un bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>.\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> "You
    typed " \<less\>\<less\> buffer \<less\>\<less\> "\\n";
  </verbatim>

  � l'int�rieur de ce type de bloc, on peut envoyer r�cursivement d'autre
  blocs qui peuvent utiliser des formats diff�rents. Par exemple, le code
  suivant envoie une formule <apply|LaTeX> :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula:
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "latex:" \<less\>\<less\> "$x^2+y^2=z^2$"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> "\\n";
  </verbatim>

  Dans certains cas, il peut �tre utile d'envoyer directement la sortie en
  format <apply|TeXmacs> en utilisant une repr�sentation <apply|scheme> :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And finally a fraction
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "scheme:" \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> ".\\n";
  </verbatim>

  � la fin, il faut de nouveau envoyer <verbatim|DATA_END> et vider la sortie
  standard :\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ fflush
    (stdout);<format|next line> \ \ \ \ \ }<format|next line>
    \ \ \ \ \ return 0;<format|next line> \ \ \ }
  </verbatim>

  Notez qu'il ne faut jamais envoyer plus d'un bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>. D�s que le premier bloc
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> est re�u par <apply|TeXmacs>, le
  syst�me se met en attente d'entr�e. Si vous voulez envoyer plusieurs blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END>, vous devez les inclure dans un
  bloc principal.

  Un <space|0.2spc>canal<space|0.2spc> sp�cifique est utilis� pour envoyer
  l'invite. Les canaux sont sp�cifi�s comme des blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> sp�ciaux :\ 

  <\verbatim>
    \ \ \ \ static int counter= 0;<format|next line><format|next line>
    \ \ \ void<format|next line> \ \ \ next_input () {<format|next line>
    \ \ \ \ \ counter++;<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_BEGIN \<less\>\<less\> "channel:prompt" \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ \ \ cout \<less\>\<less\> "Input "
    \<less\>\<less\> counter \<less\>\<less\> "] ";<format|next line> \ \ \ }
  </verbatim>

  � l'int�rieur d'un canal d'invite, vous pouvez aussi utiliser des blocs
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> imbriqu�s. Ceci permet, par
  exemple, d'utiliser une formule comme invite. Il existe trois canaux
  standards :\ 

  <\description>
    <expand|item*|<verbatim|output>.>Canal de sortie normale par d�faut.

    <expand|item*|<verbatim|prompt>.>Canal d'envoi d'une invite de saisie.

    <expand|item*|<verbatim|input>.>Canal servant � envoyer une valeur par
    d�faut � la prochaine entr�e.
  </description>

  <section|Sortie graphique>

  On peut envoyer des images PostScript en sortie. � supposer qu'il existe
  une image <verbatim|picture.ps> dans votre r�pertoire utilisateur, si vous
  ins�rez les lignes suivantes :

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";<format|next
    line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "ps:";<format|next line> \ \ \ \ \ \ \ fflush (stdout);<format|next line>
    \ \ \ \ \ \ \ system ("cat $HOME/picture.ps");<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> "\\n";
  </verbatim>

  � l'endroit appropri� dans la boucle principale, votre image s'affichera
  dans la sortie.

  <section|Le listing complet>

  <\verbatim>
    #include \<less\>stdio.h\<gtr\><format|next line>#include
    \<less\>stdlib.h\<gtr\><format|next line>#include
    \<less\>string.h\<gtr\><format|next line>#include
    \<less\>iostream.h\<gtr\><format|next line><format|next line>#define
    DATA_BEGIN \ \ ((char) 2)<format|next line>#define DATA_END
    \ \ \ \ ((char) 5)<format|next line>#define DATA_ESCAPE \ ((char)
    27)<format|next line><format|next line>static int counter= 0;<format|next
    line><format|next line>void<format|next line>next_input () {<format|next
    line> \ counter++;<format|next line> \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "channel:prompt" \<less\>\<less\> DATA_END;<format|next
    line> \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";<format|next line>}<format|next line><format|next
    line>int<format|next line>main () {<format|next line> \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";<format|next line> \ cout \<less\>\<less\> "This software
    comes with no warranty whatsoever\\n";<format|next line> \ cout
    \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ next_input ();<format|next line> \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ fflush (stdout);<format|next
    line><format|next line> \ while (1) {<format|next line> \ \ \ char
    buffer[100];<format|next line> \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ if (strcmp (buffer, "quit") == 0)
    break;<format|next line> \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "verbatim:";<format|next line> \ \ \ cout
    \<less\>\<less\> "You typed " \<less\>\<less\> buffer \<less\>\<less\>
    "\\n";<format|next line><format|next line> \ \ \ cout \<less\>\<less\>
    "And now a LaTeX formula: ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:" \<less\>\<less\>
    "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;<format|next line> \ \ \ cout
    \<less\>\<less\> "\\n";<format|next line><format|next line> \ \ \ cout
    \<less\>\<less\> "And finally a fraction ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:" \<less\>\<less\>
    "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ cout \<less\>\<less\> ".\\n";<format|next line><format|next line>
    \ \ \ next_input ();<format|next line> \ \ \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ fflush (stdout);<format|next line>
    \ }<format|next line> \ return 0;<format|next line>}
  </verbatim>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

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
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Studying the source code step by
      step><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      output><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The complete
      listing><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
