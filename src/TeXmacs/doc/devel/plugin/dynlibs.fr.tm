<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Transformation de votre syst�me en librairie dynamique>

  Au lieu de connecter votre syst�me � <apply|TeXmacs> via un tube, vous
  pouvez aussi le lier en tant que librairie dynamique. Bien que la
  communication via des tubes soit g�n�ralement plus facile � impl�menter,
  plus robuste et compatible avec une sortie graduelle, la seconde option est
  plus rapide.

  <section|Connexion en tant que librairie dynamique>

  Voici les �tapes � suivre pour lier votre syst�me en tant que librairie
  dynamique :

  <\enumerate>
    <item>Modifiez l'architecture de votre syst�me de telle fa�on que sa plus
    grande partie puisse �tre li�e en tant que librairie partag�e ; le
    binaire deviendra alors un tout petit programme que g�rera les entr�es et
    sorties verbatim et qui sera li� � votre libraire partag� lors de
    l'ex�cution.

    <item>Copiez le fichier <verbatim|$TEXMACS_PATH/include/TeXmacs.h> dans
    le r�pertoire include de votre source et �crivez les routines
    d'entr�e/sortie en suivant le dernier protocole de communication de
    <TeXmacs> d�crit ci-dessous.

    <item>Incluez une ligne de la forme :

    <\verbatim>
      \ \ \ \ (package-declare "myplugin" "libmyplugin.so" "get_name_package"
      "init")
    </verbatim>

    dans le fichier <verbatim|init-myplugin.scm> qui a �t� d�crit dans le cas
    de la communication via des tubes. Ici, <verbatim|libmyplugin.so> est la
    librairie partag�e, <verbatim|get_name_package> la fonction qui sera
    appel�e par <apply|TeXmacs> pour lier votre syst�me � <apply|TeXmacs> et
    <verbatim|init> une cha�ne d'initialisation pour votre package.

    <item>Proc�dez ensuite de la m�me fa�on que dans le cas de la
    communication par tubes.
  </enumerate>

  <section|Le protocole de communication de <TeXmacs>>

  Le protocole de communication de <apply|TeXmacs> est utilis� pour lier
  dynamiquement des librairies � <apply|TeXmacs>. Le fichier
  <verbatim|$TEXMACS_PATH/include/TeXmacs.h> contient les d�clarations de
  toutes les structures de donn�es et toutes les fonctions utilis�es par le
  protocole. En fait, nous pr�voyons une succession de diff�rents protocoles.
  Ils poss�dent tous en commun les structures de donn�es abstraites
  <verbatim|TeXmacs_exports> et <verbatim|package_exports>, ainsi que des
  informations sur les versions du protocole, de <apply|TeXmacs> et de votre
  package.

  La <with|mode|math|n<rsup|i�me>> version \ concr�te du protocole de
  communication doit fournir deux structures de donn�es
  <verbatim|TeXmacs_exports_n> et <verbatim|package_exports_n>. La premi�re
  structure contient toutes les routines et donn�es de <apply|TeXmacs>
  n�cessaires au package. La seconde structure contient toutes les routines
  et donn�es de votre package qui doivent �tre visibles dans <apply|TeXmacs>.

  Pour lier votre syst�me � <apply|TeXmacs>, vous devez impl�menter une
  fonction :

  <\verbatim>
    \ \ \ \ package_exports* get_my_package (int version);
  </verbatim>

  Cette fonction prend en entr�e le dernier protocole de communication de
  <apply|TeXmacs> accept� par votre syst�me. Il doit retourner un pointeur
  vers une instance d'une structure concr�te <verbatim|package_exports_n>, o�
  <verbatim|n> est inf�rieur ou �gal � <verbatim|version>.

  <section|Version 1 du protocole de communication de <TeXmacs>>

  Dans la premi�re version du protocole de communication de <TeXmacs>, votre
  package doit exporter une instance de la structure de donn�es suivante :

  <\verbatim>
    \ \ \ \ typedef struct package_exports_1 {<format|next line>
    \ \ \ \ \ char* version_protocol; /* "TeXmacs communication protocol 1"
    */<format|next line> \ \ \ \ \ char* version_package;<format|next line>
    \ \ \ \ \ char* (*install) (TeXmacs_exports_1* TM, char* options, char**
    errors);<format|next line> \ \ \ \ \ char* (*evaluate) (char* what, char*
    session, char** errors);<format|next line> \ \ \ \ \ char* (*execute)
    (char* what, char* session, char** errors);<format|next line> \ \ \ }
    package_exports_1;
  </verbatim>

  La cha�ne <verbatim|version_protocol> doit contenir <verbatim|"TeXmacs
  communication protocol 1"> et la cha�ne <verbatim|version_package> la
  version de votre package.

  La routine <verbatim|install> est appel�e par <apply|TeXmacs> pour
  initialiser votre syst�me avec les options <verbatim|options>. Elle
  communique � votre syst�me les routines export�es par <apply|TeXmacs> sous
  la forme de <verbatim|TM>. La routine doit retourner un message de statut
  tel que :

  <\verbatim>
    \ \ \ \ "yourcas-version successfully linked to TeXmacs"
  </verbatim>

  Si l'installation �choue, vous devez retourner <verbatim|NULL> et
  <verbatim|*errors> doit contenir un message d'erreur. <verbatim|what> et la
  cha�ne retourn�e ont tous les deux un format sp�cial, dans lequel il est
  possible d'encoder des documents <apply|TeXmacs>. Ce format sera d�crit
  dans la prochaine section.

  La routine <verbatim|evaluate> est utilis�e pour �valuer l'expression
  <verbatim|what> dans une session <apply|TeXmacs> dont le nom est
  <verbatim|session>. Elle doit retourner l'�valuation de <verbatim|what> ou
  <verbatim|NULL> en cas d'erreur. <verbatim|*errors> contient soit un ou
  plusieurs message d'attention ou un message d'erreur, si l'�valuation
  �choue. La commande :

  <\verbatim>
    \ \ \ \ (package-format "yourcas" "input-format" "output-format")
  </verbatim>

  est utilis�e pour sp�cifier les formats d'entr�e/sortie des �valuations, de
  la m�me fa�on que dans le cas des tubes.

  La routine <verbatim|execute> poss�de une sp�cification similaire � celle
  de <verbatim|evaluate>, sauf qu'elle n'est pas utilis�e pour l'�valuation
  d'expressions dans une session <apply|TeXmacs>, mais plut�t pour d'autres
  besoins de communication entre <apply|TeXmacs> et votre package.

  <\remark>
    Toutes les cha�nes retourn�es par les routines <verbatim|install>,
    <verbatim|evaluate> et <verbatim|execute>, ainsi que les messages
    d'attention et d'erreur doivent �tre allou�es avec <verbatim|malloc>.
    Elle seront lib�r�es par <apply|TeXmacs> avec <verbatim|free>.
  </remark>

  La premi�re version du protocole de communication de <apply|TeXmacs>
  suppose aussi que <apply|TeXmacs> exporte une instance de la structure de
  donn�es :

  <\verbatim>
    \ \ \ \ typedef struct TeXmacs_exports_1 {<format|next line>
    \ \ \ \ \ char* version_protocol; /* "TeXmacs communication protocol 1"
    */<format|next line> \ \ \ \ \ char* version_TeXmacs;<format|next line>
    \ \ \ } TeXmacs_exports_1;
  </verbatim>

  La cha�ne <verbatim|version_protocol> contient la version
  <verbatim|"TeXmacs communication protocol 1"> du protocole et
  <verbatim|version_TeXmacs> la version courante de <TeXmacs>.

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
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Connections via dynamically linked
      libraries><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>The TeXmacs communication
      protocol><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Version 1 of the TeXmacs communication
      protocol><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
