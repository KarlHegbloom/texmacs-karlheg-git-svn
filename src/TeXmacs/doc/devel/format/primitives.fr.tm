<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Les primitives <TeXmacs>>

  Cette page est loin d'�tre � jour. Il faudrait un fichier par cat�gorie et
  une page par primitive, avec un exemple.

  <section|Principales primitives de formatage>

  <\description>
    <expand|item*|<verbatim|(document p1 ...
    pn)>><with|mode|math|mode|text|><format|next line>Forme g�n�rale de
    l'arbre d'�dition, document constitu� des paragraphes <verbatim|p1, ...,
    pn>. Peut aussi �tre utilis� dans les sous-arbres pour cr�er un bloc de
    paragraphes multiples dans des contextes o�, en temps normal, seule une
    ligne est possible ; par exemple, les cellules d'un tableau
    multi-paragraphes utilisent des noeuds de <verbatim|documents> imbriqu�s.

    <expand|item*|<verbatim|(concat t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Concat�nation
    horizontale des arbres <verbatim|t1, ..., tn>. Le plus souvent, les
    paragraphes de l'arbre d'�dition principal sont des concat�nations.

    <expand|item*|<verbatim|(surround left right
    body)>><with|mode|math|mode|text|><format|next line>Entoure un texte
    multi-paragraphe <verbatim|body> par <verbatim|left> et <verbatim|right>.

    <expand|item*|<verbatim|(hspace spc)> ou <verbatim|(hspace spc min
    max)>><with|mode|math|mode|text|><format|next line>Espace horizontale de
    longueur <verbatim|spc>. Les param�tres optionnels <verbatim|min> et
    <verbatim|max> indiquent que l'espace peut varier entre ces valeurs.
    Notez que certaines unit�s de longueurs, tel <verbatim|fn>, sont
    automatiquement accompagn�es de param�tres de variation.

    <expand|item*|<verbatim|(vspace* spc)> ou <verbatim|(vspace* spc min
    max)>><with|mode|math|mode|text|><format|next line>Ins�re une espace
    verticale de longueur <verbatim|spc> avant le paragraphe o� cette
    primitive est plac�e.

    <expand|item*|<verbatim|(vspace spc)> ou <verbatim|(vspace spc min
    max)>><with|mode|math|mode|text|><format|next line>Ins�re une espace
    verticale de longueur <verbatim|spc> apr�s le paragraphe o� cette
    primitive est plac�e.

    <expand|item*|<verbatim|(space spc bot
    top)>><with|mode|math|mode|text|><format|next line>Ins�re une espace
    horizontale de longueur <verbatim|spc>, dont la ligne inf�rieure est
    situ�e � la hauteur <verbatim|top> et la ligne inf�rieure � la hauteur
    <verbatim|bot>.

    <expand|item*|<verbatim|(htab spc)>><with|mode|math|mode|text|><format|next
    line>Espace de tabulation de longueur minimale <verbatim|spc>.

    <expand|item*|<verbatim|(split pos1 ...
    posn)>><with|mode|math|mode|text|><format|next line>Scinde le paragraphe
    courant en un tableau de <verbatim|n> colonnes, positionn�es �
    \ <verbatim|pos1, ..., posn>.

    <expand|item*|<verbatim|(move t x y)>><with|mode|math|mode|text|><format|next
    line>D�place l'arbre n de x unit�s de longueur horizontalement et de y
    unit�s de longueur verticalement.

    <expand|item*|<verbatim|(resize t "normal" x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>Transforme le cadre
    <verbatim|t> en un nouveau cadre dont le coin inf�rieur gauche est situ�
    en <verbatim|(x1,y1)> et le coin sup�rieur droit en <verbatim|(x2,y2)>.

    <expand|item*|<verbatim|(resize t "extend" x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>Identique au pr�c�dent,
    mais le nouveau cadre inclut l'ancien.

    <expand|item*|<verbatim|(format "line
    break")>><with|mode|math|mode|text|><format|next line>Saut de ligne
    forc�.

    <expand|item*|<verbatim|(format "new line")>><with|mode|math|mode|text|><format|next
    line>Nouvelle ligne forc�e.

    <expand|item*|<verbatim|(format "line
    separator")>><with|mode|math|mode|text|><format|next line>Op�re comme la
    commande <apply|TeX> <verbatim|&>.

    <expand|item*|<verbatim|(format "next
    line")>><with|mode|math|mode|text|><format|next line>Op�re comme la
    commande <apply|TeX> <verbatim|\\\\>.

    <expand|item*|<verbatim|(format "no line
    break")>><with|mode|math|mode|text|><format|next line>Emp�che un saut de
    ligne.

    <expand|item*|<verbatim|(format "no first
    indentation")>><with|mode|math|mode|text|><format|next line>Emp�che
    l'indentation � gauche du paragraphe.

    <expand|item*|<verbatim|(format "no last
    indentation")>><with|mode|math|mode|text|><format|next line>Emp�che
    l'indentation � droite du paragraphe.

    <expand|item*|<verbatim|(format "enable first
    indentation")>><with|mode|math|mode|text|><format|next line>Force
    l'indentation � gauche du paragraphe.

    <expand|item*|<verbatim|(format "enable last
    indentation")>><with|mode|math|mode|text|><format|next line>Force
    l'indentation � droite du paragraphe.

    <expand|item*|<verbatim|(format "page
    break")>><with|mode|math|mode|text|><format|next line>Saut de page forc�.

    <expand|item*|<verbatim|(format "new page")>><with|mode|math|mode|text|><format|next
    line>Nouvelle page forc�e.

    <expand|item*|<verbatim|(format "no page break
    before")>><with|mode|math|mode|text|><format|next line>Emp�che un saut de
    page avant la ligne.

    <expand|item*|<verbatim|(format "no page break
    after")>><with|mode|math|mode|text|><format|next line>Emp�che un saut de
    page apr�s la ligne.

    <expand|item*|<verbatim|(format "with
    limits")>><with|mode|math|mode|text|><format|next line>Indique que le
    texte pr�c�dent est limit� en hauteur. En cons�quence, les indices et
    exposants sont plac�s en-dessous et au-dessus du texte pr�c�dent.
  </description>

  <section|Constructions math�matiques>

  <\description>
    <expand|item*|<verbatim|(group t)>><with|mode|math|mode|text|><format|next
    line>Consid�re <verbatim|t> comme une entit� (un peu comme entour�
    <verbatim|t> d'accolades dans <apply|TeX>).

    <expand|item*|<verbatim|(left s)>><with|mode|math|mode|text|><format|next
    line>Grand d�limiteur gauche <verbatim|s>.

    <expand|item*|<verbatim|(middle s)>><with|mode|math|mode|text|><format|next
    line>Grand s�parateur <verbatim|s>.

    <expand|item*|<verbatim|(right s)>><with|mode|math|mode|text|><format|next
    line>Grand d�limiteur droit <verbatim|s>.

    <expand|item*|<verbatim|(big s)>><with|mode|math|mode|text|><format|next
    line>Grand op�rateur <verbatim|s>.

    <expand|item*|<verbatim|(lprime s)>><with|mode|math|mode|text|><format|next
    line>Prime � gauche <verbatim|s>.

    <expand|item*|<verbatim|(rprime s)>><with|mode|math|mode|text|><format|next
    line>Prime � droite <verbatim|s>.

    <expand|item*|<verbatim|(below t sub)>><with|mode|math|mode|text|><format|next
    line>Indice <verbatim|sub> en-dessous de <verbatim|t>.

    <expand|item*|<verbatim|(above t sup)>><with|mode|math|mode|text|><format|next
    line>Exposant <verbatim|sup> au-dessus de <verbatim|t>.

    <expand|item*|<verbatim|(lsub script)>><with|mode|math|mode|text|><format|next
    line>Indice � gauche <verbatim|script> du texte qui suit.

    <expand|item*|<verbatim|(lsup script)>><with|mode|math|mode|text|><format|next
    line>Exposant � gauche <verbatim|script> du texte qui suit.

    <expand|item*|<verbatim|(rsub script)>><with|mode|math|mode|text|><format|next
    line>Indice � droite <verbatim|script> du texte pr�c�dent.

    <expand|item*|<verbatim|(rsup script)>><with|mode|math|mode|text|><format|next
    line>Exposant � droite <verbatim|script> du texte pr�c�dent.

    <expand|item*|<verbatim|(frac num den)>><with|mode|math|mode|text|><format|next
    line>Fraction de num�rateur <verbatim|num> et de d�nominateur
    <verbatim|den>.

    <expand|item*|<verbatim|(sqrt t)> ou <verbatim|(sqrt t
    n)>><with|mode|math|mode|text|><format|next line>Racine carr�e de
    <verbatim|t> ou racine <verbatim|n<rsup|i�me>> de <verbatim|t>.

    <expand|item*|<verbatim|(wide t accent)>><with|mode|math|mode|text|><format|next
    line>Large accent au-dessus de <verbatim|t>.

    <expand|item*|<verbatim|(neg t)>><with|mode|math|mode|text|><format|next
    line>Barre <verbatim|t> en diagonale.

    <expand|item*|<verbatim|(tree root t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Arbre de racine
    <verbatim|root> et de branches <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(matrix t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line>Matrice <verbatim|n>,
    <verbatim|m> dont les �l�ments sont les <verbatim|tij>.

    <expand|item*|<verbatim|(table t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line>tableau <verbatim|n> par
    <verbatim|m> dont les �l�ments sont les <verbatim|tij>.

    <expand|item*|<verbatim|(mosaic t11 ... t1m ...... tn1 ... tnm n
    m)>><with|mode|math|mode|text|><format|next line>tableau <verbatim|n> par
    <verbatim|m> dont les �l�ments <verbatim|tij> peuvent �tre align�s et
    fusionn�s.

    <expand|item*|<verbatim|(mosaic item t pos x y
    bg)>><with|mode|math|mode|text|><format|next line>Dans un mosa�que, champ
    dont le contenu est <verbatim|t>, la position <verbatim|pos>, qui s'�tend
    sur <verbatim|x> colonnes et \ <verbatim|y> lignes et a une couleur
    d'arri�re-plan <verbatim|bg>.
  </description>

  <section|Variables d'environnement, macros, fonctions, etc...>

  <\description>
    <expand|item*|<verbatim|(assign var t)>><with|mode|math|mode|text|><format|next
    line>Donne � la variable d'environnement <verbatim|var> la valeur
    <verbatim|t>.

    <expand|item*|<verbatim|(with var1 val1 ... varn valn
    body)>><with|mode|math|mode|text|><format|next line>Donne aux variables
    d'environnement <verbatim|var1, ..., varn> les valeurs <verbatim|val1,
    ..., valn> dans <verbatim|body>.

    <expand|item*|<verbatim|(expand f t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>R�sout la macro
    <verbatim|f> avec les arguments <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(apply f t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Applique la fonction
    <verbatim|f> � <verbatim|t1, ..., tn>.

    <expand|item*|<verbatim|(func arg1 ... argn
    body)>><with|mode|math|mode|text|><format|next line>Fonction d'arguments
    <verbatim|arg1, ..., argn> et de corps <verbatim|body>.

    <expand|item*|<verbatim|(argument var)>><with|mode|math|mode|text|><format|next
    line>Argument de macro <verbatim|var>.

    <expand|item*|<verbatim|(value var)>><with|mode|math|mode|text|><format|next
    line>Valeur de <verbatim|var>.

    <expand|item*|<verbatim|(symbol s)>><with|mode|math|mode|text|><format|next
    line>Pour saisir un symbole universel.

    <expand|item*|<verbatim|(latex cmd)>><with|mode|math|mode|text|><format|next
    line>Pour saisir une commande <apply|LaTeX>.

    <expand|item*|<verbatim|(hybrid cmd)>><with|mode|math|mode|text|><format|next
    line>Pour saisir un appel de fonction <apply|TeXmacs>, une commande
    <apply|LaTeX> ou un symbole universel.

    <expand|item*|<verbatim|(quote t)>><with|mode|math|mode|text|><format|next
    line>Retourne la valeur de <verbatim|t>.

    <expand|item*|<verbatim|(eval t)>><with|mode|math|mode|text|><format|next
    line>�value <verbatim|t>.

    <expand|item*|<verbatim|(delay (cmd t1 ...
    tn))>><with|mode|math|mode|text|><format|next line>�value les arguments
    <verbatim|t1, ..., tn> � <verbatim|u1, ..., un> et retourne
    <verbatim|(cmd u1 ... un)>.
  </description>

  <section|Op�rateurs fonctionnels>

  <\description>
    <expand|item*|<verbatim|(plus t u)>><with|mode|math|mode|text|><format|next
    line>Additionne les nombres ou longueurs <verbatim|t> et <verbatim|u>.

    <expand|item*|<verbatim|(minus t u)>><with|mode|math|mode|text|><format|next
    line>Soustrait le nombre ou la longueur <verbatim|t> au nombre ou � la
    longueur <verbatim|u>.

    <expand|item*|<verbatim|(times t u)>><with|mode|math|mode|text|><format|next
    line>Multiplie le nombre ou la longueur <verbatim|t> par le nombre ou la
    longueur <verbatim|u>.

    <expand|item*|<verbatim|(merge t u)>><with|mode|math|mode|text|><format|next
    line>Concat�ne les cha�nes de caract�res <verbatim|t> et <verbatim|u>.

    <expand|item*|<verbatim|(number t what)>><with|mode|math|mode|text|><format|next
    line>Le nombre <verbatim|t> en format <verbatim|what> (romain, alpha,
    etc.).

    <expand|item*|<verbatim|(translate t from
    into)>><with|mode|math|mode|text|><format|next line>Traduit <verbatim|t>
    de <verbatim|from> en <verbatim|into>.
  </description>

  <section|Autre contenu dynamique>

  <\description>
    <expand|item*|<verbatim|(inactive t)>><with|mode|math|mode|text|><format|next
    line>Rend inactif � l'�dition un op�rateur d'environnement ou un
    op�rateur invisible.

    <expand|item*|<verbatim|(label name)>><with|mode|math|mode|text|><format|next
    line>�tiquette de nom <verbatim|name>.

    <expand|item*|<verbatim|(reference name)>><with|mode|math|mode|text|><format|next
    line>R�f�rence � l'�tiquette de nom <verbatim|name>.

    <expand|item*|<verbatim|(write t aux)>><with|mode|math|mode|text|><format|next
    line>�crit <verbatim|t> dans le tampon auxiliaire de donn�es
    <verbatim|aux>. Cette construction est utilis�e pour cr�er les tables de
    mati�res, bibliographies, glossaires, etc...

    <expand|item*|<verbatim|(specific what
    t)>><with|mode|math|mode|text|><format|next line>Indique que <verbatim|t>
    n'appara�tra que lorsque le document sera converti en format
    <verbatim|what>. Si <verbatim|what> correspond �
    \ <space|0.2spc><verbatim|TeXmacs><space|0.2spc>, <verbatim|t>
    n'appara�tra que dans <apply|TeXmacs>.

    <expand|item*|<verbatim|(postscript file width height x1 y1 x2
    y2)>><with|mode|math|mode|text|><format|next line>Image Postscript (ou
    autre type d'image) contenue dans <verbatim|file>, de largeur
    <verbatim|width>, de longueur <verbatim|length>, rogn�e suivant un cadre
    d�fini par son coin inf�rieur gauche <verbatim|(x1,y1)> et son coin
    inf�rieur droit <verbatim|(x2,y2)>. Si les param�tres <verbatim|width,
    height, x1, y1, x2, y2> sont inexistants, les param�tres par d�faut de
    l'image sont utilis�es. La hauteur et la largeur peuvent aussi �tre
    indiqu�es sous la forme <verbatim|*mag> ou <verbatim|/schrink>, auquel
    cas le(s) param�tre(s) affect�(s) sera (seront) multipli�(s) resp.
    divis�(s) par le facteur <verbatim|mag> resp. <verbatim|schrink>.
  </description>

  <section|Usage priv�>

  <\description>
    <expand|item*|<verbatim|(tuple t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Tuplet <verbatim|(t1,
    ..., tn)>.

    <expand|item*|<verbatim|(collection t1 ...
    tn)>><with|mode|math|mode|text|><format|next line>Jeu <verbatim|{t1, ...,
    tn}>.

    <expand|item*|<verbatim|(associate t u)>><with|mode|math|mode|text|><format|next
    line>Association <verbatim|t -\<gtr\> u>.
  </description>

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
    <associate|preamble|false>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|paragraph hyphenation|professional>
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
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Principales primitives de
      formatage><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Constructions
      math�matiques><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Variables d'environment, macros,
      fonctions, etc...><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Op�rateurs
      fonctionnels><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Autre contenu
      dynamique><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Usage
      priv�><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
