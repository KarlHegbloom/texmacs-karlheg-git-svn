<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Variantes structur�es>

  Lors de la cr�ation d'un environnement comme un th�or�me, une �quation, ou
  une liste, il arrive souvent que l'on veuille changer cet environnement
  <em|a posteriori>. On peut faire ceci en utilisant les raccourcis clavier
  <shortcut|(variant-circulate (focus-tree) #t)> et
  <shortcut|(variant-circulate (focus-tree) #f)> pour boucler � travers la
  liste des <em|variantes structur�es> de la balise la plus � l'int�rieur de
  mani�re directe ou inverse.

  Par exemple, supposons que l'on soit � l'int�rieur d'un th�or�me. En
  appuyant plusieurs fois <shortcut|(variant-circulate (focus-tree) #t)>, on
  peut alors changer le th�or�me en proposition, puis en lemme, en
  corollaire, en conjecture, et enfin revenir au th�or�me. Le raccourci
  <shortcut|(variant-circulate (focus-tree) #f)> permet de boucler en sens
  inverse<nbsp>: th�or�me<nbsp><math|<op|\<rightarrow\>>>
  conjecture<nbsp><math|<op|\<rightarrow\>>>
  corollaire<nbsp><math|<op|\<rightarrow\>>>
  lemme<nbsp><math|<op|\<rightarrow\>>> proposition<nbsp><math|<op|\<rightarrow\>>>
  th�or�me.

  Dans le cas des formules math�matiques, le raccourci
  <shortcut|(variant-circulate (focus-tree) #t)> vous permet de changer une
  formule en ligne comme <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> en formule
  hors-texte

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>
  </equation*>

  en prenant en compte d'�ventuels \S<nbsp>espaces superflus et signes de
  ponctuation<nbsp>\T.

  <TeXmacs> fournit aussi le raccourci <shortcut|(numbered-toggle
  (focus-tree))> pour changer les environnements num�rot�s en environnements
  non num�rot�s et <em|vice versa>. Ceci fonctionne pour les environnements
  les plus courants comme les th�or�mes, les remarques, les tableaux, les
  �quations, etc. Remarquez que <shortcut|(numbered-toggle (focus-tree))>
  change aussi une liste non num�rot�e en liste num�rot�e et <em|vice versa>,
  alors que <shortcut|(variant-circulate (focus-tree) #t)> permet de boucler
  parmi les diff�rents types de listes possibles (points, tirets, fl�ches,
  <abbr|etc.>).

  <tmdoc-copyright|1998--2011|Joris van der Hoeven|Denis Raux>

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