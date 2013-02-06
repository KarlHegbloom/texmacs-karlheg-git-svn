<TeXmacs|1.0.7.17>

<style|tmdoc>

<\body>
  <tmdoc-title|Preferences utilisateur>

  Pour une utilisation optimale, vous pouvez souhaiter configurer <TeXmacs>
  afin qu'il corresponde � vos besoins. Cela peut-�tre fait � l'aide du menu
  \ <menu|Edit|Preferences>. Le plus important est de choisir l'aspect et le
  comportement dans <menu|Edit|Preferences|Look and feel> . Cela vous permet,
  par exemple, de definir des raccourcis clavier de <TeXmacs> identiques �
  ceux utilis�s dans d'autres applications.

  Les personnalisations suivantes sont disponibles :

  <\description>
    <item*|<menu|Look and feel>><label|preferences:look-and-feel>Ce choix
    controle l'aspect et le comportement g�n�ral de <TeXmacs>, et en grande
    partie le comportement du clavier. La <menu|default> d�pend de votre
    syst�me (<menu|Gnome>, <menu|KDE> ou <menu|Emacs> sous <name|Linux>,
    <menu|Mac OS> sous <name|Mac OS>, and <menu|Windows> sous
    <name|Windows>). L'aspect <menu|Emacs> peut-�tre utilis� comme
    alternative sous tous les syst�mes; Cela a �t� le d�faut pour toutes les
    versions de <TeXmacs> avant la 1.0.7.6.

    Plus de details sur \ <hlink|keyboard configuration on different
    systems|man-config-keyboard.en.tm> peuvent �tre trouv�s ci-dessous.

    <item*|<menu|Interactive questions>>Ce choix determine comment
    l'utilisateur sera inform� qu'une entr�e est attendue. Cela peut-�tre
    r�alis� dans une fen�tre s�par�e ou dans la barre de status de
    \ <TeXmacs>.

    <item*|<menu|Details in menus>>Ce choix determine le niveau de detail des
    menus. Les choix les moins souvent utilis�s seront �cart�s par la
    s�lection de <menu|Simplified menus>.

    <item*|<menu|View>>Ce choix est le m�me que celui du menu racine.

    <item*|<menu|Language>>Votre langue pr�f�r�e pour l'interface <TeXmacs> .

    <item*|<menu|Keyboard>><label|preferences:keyboard>En plus de l'aspect
    g�n�ral, quelques selecteurs \ d�terminent le comportement du clavier:

    <\itemize>
      <item>Le <menu|Cyrillic input method> determine <hlink|how to type text
      in Cyrillic languages|man-russian.en.tm>.

      <item>Les guillements peuvent �tre automatiquement ferm�s � l'aide du
      style <menu|Automatic quotes> .

      <item>Les paranth�ses peuvent �tre automatiquement ferm�es � l'aide de
      : \ <menu|Automatically close brackets>.
    </itemize>

    <item*|<menu|Printer>>L'imprimante peut �tre configur�e � l'aide de ce
    sous-menu.

    <item*|<menu|Security>>En th�orie, les documents <TeXmacs> peuvent
    inclure des macros ou des hyperlinks qui permettent l'execution de
    commandes arbitraires (d�finies par l'auteur). En pratique, cette
    fonctionnalit� peut produire un trou de s�curit�. C'est pourquoi la
    p�f�rence <menu|Security> permet � l'utilisateur de d�finir ce qui
    peut-�tre fait par un code executable non certifi�.

    <item*|<menu|Converters>>Le comportement des convertisseurs entre
    \ <TeXmacs> et les divers autres formats peut �tre configur� � partir de
    ce menu. Pour plus de d�tails, vous r�f�rez au <hlink|chapter on
    compatibility with other formats|../convert/man-convert.en.tm>.

    <item*|<menu|Scripts>>D�finit le language de script par d�faut pour les
    scripts externes.

    <item*|<menu|Tools>><TeXmacs> offre quelques outils additionnels �
    destination de l'utilisateur pour des usages particuliers :

    <\itemize>
      <item>Un outil de debogage pour les d�veloppeurs <TeXmacs> .

      <item>Un cr�ateur de liens pour entrer des hyperliens et des
      annotations complexes.

      <item>Un outil de gestion des versions pour comparer deux versions d'un
      document <TeXmacs> .

      <item>Une connection a distance (qui ne fonctionne plus en ce moment).
    </itemize>

    <item*|<menu|Autosave>>Ce choix determine la fr�quence de la sauvegarde
    automatique. Toute edition d'un fichier qui n'est pas en sauvegarde
    automatique, sera perdue en cas de terminaison anormale de <TeXmacs>.
    Cela se produit typiquement apr�s une fausse manoeuvre de la part de
    l'utilisateur, � cause de certains bugs dans <TeXmacs>, ou � cause d'une
    coupure de courant.

    <item*|><item*|<menu|Bibtex command>>L'utilisateur peut d�finier une
    alternative � \ <verbatim|bibtex> pour la compilation des bibliographies
    n�cessitant <BibTeX>. Notez que les versionns r�centes de <TeXmacs>
    incluent un outil alternatif de compilation des bibliographies.
  </description>

  <tmdoc-copyright|1998--2013|Joris van der Hoeven, Denis Raux>

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