<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cr�ation de votre premi�re interface avec
  <apply|TeXmacs>>

  Pour cr�er votre premi�re interface avec <apply|TeXmacs>, nous vous
  recommandons de suivre les �tapes suivantes :

  <\enumerate>
    <item>Cr�ez une option <verbatim|--texmacs> dans votre programme. Elle
    sera utilis�e pour appeler votre programme dans <apply|TeXmacs>.

    <item>Modifiez vos routines de sorties de telle mani�re que les sorties
    soient envoy�es � <apply|TeXmacs> lorsque votre programme est lanc�e avec
    l'option <verbatim|--texmacs>.

    <item>Cr�ez un script <verbatim|mycas> dans votre PATH qui lancera votre
    programme avec l'option <verbatim|--texmacs>.
  </enumerate>

  Apr�s avoir fait cela, votre programme sera accessible sous le nom de
  <apply|menu|Mycas> via <apply|menu|Text|Session>. Nous vous expliquerons
  plus loin comment afficher votre syst�me sous son vrai nom, comment le
  personnaliser et comment int�grer l'interface dans la distribution
  officielle de <apply|TeXmacs>.

  En g�n�ral, l'�tape 2 est la plus compliqu�e et le temps pass� dessus
  d�pend de l'architecture de votre syst�me. Si vous cr�ez des routines de
  sortie bien pens�es (y compris les routines pour afficher les messages
  d'erreur), il vous suffira de les modifier conform�ment � l'exemple
  <verbatim|mycas> et de r�utiliser des routines de sortie <apply|LaTeX>
  fournies par la plupart des syst�mes.

  Actuellement, <apply|LaTeX> n'est impl�ment� que comme format de
  transmission standard de formules math�matiques, car c'est le format le
  plus utilis�. Nous avons pr�vu d'impl�menter plus tard des formats plus
  s�rs du point de vue s�mantique. Ne perdez pas de vue non plus que vous
  pouvez envoyer vos sorties sous forme d'arbre.

  N�anmoins, nous avons enrichi le format <apply|LaTeX> standard des
  commandes <verbatim|\\*> et <verbatim|\\bignone> qui servent � la
  multiplication et � la fermeture des grands op�rateurs. Ceci permet de
  faire la distinction entre :

  <\verbatim>
    \ \ \ \ a \\* (b + c)
  </verbatim>

  (ou <with|mode|math|a> multipli� par <with|mode|math|b+c>) et :\ 

  <\verbatim>
    \ \ \ \ f(x + y)
  </verbatim>

  (ou <with|mode|math|f> appliqu� � <with|mode|math|x+y>). De m�me, dans :\ 

  <\verbatim>
    \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
  </verbatim>

  la commande <verbatim|\\bignone> est utilis�e pour sp�cifier le domaine
  d'application des op�rateurs <verbatim|\\sum>.

  Il s'av�re que l'utilisation syst�matique des commandes <verbatim|\\*> et
  <verbatim|\\bignone>, ainsi que de sorties <apply|LaTeX> correctes pour les
  autres constructions, permet <with|font shape|italic|a priori> de donner
  une signification claire � votre sortie. Par exemple, on peut ainsi �crire
  des routines suppl�mentaires pour couper et coller des formules entre
  syst�mes diff�rents.

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
    <associate|idx-1|<tuple|3.|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|3.|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
