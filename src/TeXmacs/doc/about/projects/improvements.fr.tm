<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Am�lioration de l'impl�mentation actuelle>

  <with|color|red|[Doit �tre mis � jour]>

  <section|Gestion de la m�moire>

  Si j'en ai le courage, j'�crirais peut-�tre un jour un collecteur d'espace
  m�moire pour <apply|TeXmacs>.

  <section|Interface graphique>

  Nous souhaitons passer � <with|font shape|small-caps|Guile-Gtk> ou � toute
  autre interface graphique portable.

  <section|Encodage et polices>

  <\itemize>
    <item>Il reste encore quelques changements � faire dans la fa�on dont
    l'encodage des polices est g�r�e. Ceci devrait faciliter la maintenance
    de polices comportant des caract�res issus de diff�rentes polices
    physiques, virtuelles, des caract�res sp�ciaux, etc...

    <item>� l'heure actuelle, la police logique active n'est d�termin�e qu'�
    partir d'un jeu donn�e de variables d'environnement. Il faudrait que la
    police logique soit repr�sent�e par un arbre (au lieu d'une cha�ne de
    caract�res), qui serait �valu� (de fa�on � permettre le remplacement des
    variables d'environnement), puis l'ensemble serait pass� �
    <verbatim|find_font(display,tree)>. La police active serait alors une
    police fusionn�e et les polices pourraient alors d�pendre des variables
    d'environnement (polices � plusieurs couleurs)..
  </itemize>

  <section|Vitesse>

  Pour acc�l�rer la vitesse d'ex�cution du programme, un document n'est pas
  mis � jour dans son entier chaque fois qu'il subit une modification.
  N�anmoins, il reste � mettre en place certaines changements :

  <\itemize>
    <item>Am�liorer la vitesse de chargement (et de sauvegarde) des fichiers.
    Cela acc�l�rera le chargement des polices.

    <item>Encoder les variables d'environnement syst�me ; cela acc�l�rera le
    programme dans son entier.

    <item>Repenser compl�tement la fa�on dont les variables syst�me
    d'environnement de style sont g�r�es lors de la mise en page des
    concat�nations et de paragraphes ; elle est loin d'�tre optimale.

    <item>Am�liorer l'�valuation du contexte actif sous le curseur, en
    particulier l'�valuation de l'environnement. Ce processus prend beaucoup
    de temps actuellement et ralentit la vitesse de d�placement du curseur
    dans les documents complexes utilisant les polices <TeX> (le d�filement
    du texte est bien plus rapide quand on utilise des polices X).
  </itemize>

  <section|Changements divers>

  Il faudrait changer ou stabiliser les impl�mentations suivantes :

  <\itemize>
    <item>Mouvement du curseur entre les lignes d'un m�me paragraphe (il
    faudrait que le curseur passe au d�but de la ligne suivante quand on le
    d�place apr�s la fin de la ligne pr�c�dente).

    <item>Largeurs des barres de fraction, des lignes sup�rieures des
    symboles de racine carr�e et du signe barr�.

    <item>Les cadres ne devraient pas avoir d'origines, mais leurs h�ritiers
    devraient avoir une position.

    <item>S�paration claire entre les fichiers qui d�pendent du syst�me (par
    exemple : <verbatim|fast_alloc.cpp>, <verbatim|file.hpp>,
    <verbatim|dir.hpp>) dans certains r�pertoires et les autres.
  </itemize>

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
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Memory
      management><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      interface><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Encodings and
      fonts><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Speed><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Miscellaneous
      changes><value|toc-dots><pageref|toc-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
