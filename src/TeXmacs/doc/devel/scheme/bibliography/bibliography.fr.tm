<TeXmacs|1.0.7.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Styles bibliographiques pour <TeXmacs>>

  <section|Styles bibliographiques>

  Il est possible d'associer � une bibliographie BibTeX un ou plusieurs
  styles, qu'ils soient standards ou personnalis�s. Les styles BibTeX sont
  d�not�s par leur nom usuel. Les styles personnalis�s propres � <TeXmacs>
  sont syst�matiquement pr�fix�s par <verbatim|tm-> (par exemple, le syle
  <verbatim|tm-plain> correspond au style <verbatim|plain> de <TeXmacs>).
  D'autres styles peuvent �tre ajout�s dans le r�pertoire
  <verbatim|$TEXMACS_PATH\\prog\\bibtex>.

  Pour l'�diteur, chaque style correspond � un fichier <verbatim|.scm>. Les
  fichiers correspondant � chaque style sont trait�s comme des programmes
  Scheme ordinaire : il est n�cessaire de respecter scrupuleusement
  l'utilisation des fonctions sp�cifiques aux styles bibliographiques.

  <section|Styles BibTeX>

  Actuellement, les styles BibTeX suivants ont �t� impl�ment�s :
  <verbatim|abbrv>, <verbatim|alpha>, <verbatim|ieeetr>, <verbatim|plain> et
  <verbatim|siam>. Leur utilisation ne n�cessite pas l'installation de
  BibTeX.

  <section|Cr�ation de styles bibliographiques>

  Si les fichiers de style standards de <TeXmacs> ne sont pas adapt�s � vos
  besoins, vous pouvez en cr�er d'autres. N�anmoins, la cr�ation d'un fichier
  de style � partir de rien est une t�che complexe. Nous vous recommandons
  donc de vous servir des fichiers de style ou des modules existants ou de
  les personnaliser.

  <subsection|�tude d'un exemple>

  Un fichier de style bibliographique est un<with|font-shape|italic|> fichier
  plac� dans le r�pertoire <verbatim|$TEXMACS_PATH/prog/bibtex> portant le
  nom du style suivi de l'extension <verbatim|.scm>, par exemple
  <verbatim|example.scm> pour un style <verbatim|example> ; ce dernier sera
  d�not� par <verbatim|tm-example> lors de son utilisation dans un document
  <TeXmacs>.

  Tout fichier de style doit se d�clarer en tant que module � l'aide de la
  d�claration suivante :

  <\scm-fragment>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))
  </scm-fragment>

  Le module <verbatim|bib-utils> contient toutes les fonctions n�cessaires �
  la r�dation et � l'interpr�tation d'un fichier de style bibliographique.

  Tout fichier de style doit se d�clarer en tant que style bibliographique �
  l'aide de la commande suivante :

  <scm-fragment|(bib-define-style "example" "plain")>

  Le premier param�tre de la fonction <scm|bib-define-style> est le nom du
  style courant, et le second param�tre est le nom du style par d�faut,
  <verbatim|plain> dans notre cas. Si une fonction n'est pas d�finie dans le
  style courant, la version du style par d�faut est alors utilis�e
  automatiquement.

  Ainsi, notre fichier de style minimal a l'aspect suivant :

  <\scm-fragment>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))

    \;

    (bib-define-style "example" "plain")
  </scm-fragment>

  Chaque fonction de formatage d�finie dans le style par d�faut peut �tre
  surcharg�e dans le fichier de style courant. Par exemple, la fonction de
  formatage de la date dans le style <verbatim|plain> s'appelle
  <scm|bib-format-date> ; elle red�finissable dans notre style de la mani�re
  suivante :

  <\scm-fragment>
    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-fragment>

  Toute fonction export�e doit �tre pr�fix�e par <verbatim|bib->. Une
  fonction surcharg�e doit �tre suivie de la directive <scm|(:mode
  bib-example?)>, dans laquelle <verbatim|example> est le nom du style en
  cours.

  Voici maintenant � quoi ressemble notre fichier de style
  <verbatim|example.scm> :

  <\scm-fragment>
    (texmacs-module (bibtex example)

    \ \ (:use (bibtex bib-utils)))

    \;

    (bib-define-style "example" "plain")

    \;

    (tm-define (bib-format-date e)

    \ \ (:mode bib-example?)

    \ \ (bib-format-field e "year"))
  </scm-fragment>

  <subsection|Fonctions utiles pour la cr�ation de fichiers de style>

  <\explain>
    <scm|(bib-abbreviate name dot spc)><explain-synopsis|abbr�viation d'un
    nom>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> correspondant � l'abbr�viation du
    nom contenu dans <scm|name> (un arbre <TeXmacs>) : on r�cup�re la liste
    des premi�res lettres de chaque mot, suivi de <scm|dot> (un arbre
    <TeXmacs>) et s�par�es par <scm|spc> (un arbre <TeXmacs>).
  </explain>

  <\explain>
    <scm|(bib-add-period tm)><explain-synopsis|ajout d'un point>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> en ajoutant un point � la fin de
    l'arbre <TeXmacs> <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-default tm)><explain-synopsis|arbre <TeXmacs> par d�faut>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> sans occurence du label
    <verbatim|keepkase>.
  </explain>

  <\explain>
    <scm|(bib-define-style name default)><explain-synopsis|d�claration d'un
    style>
  <|explain>
    Cette fonction d�clare un style nomm� <scm|name> (de type string) avec
    <scm|default> (de type string) le style par d�faut. Le style est alors
    s�lectionn� en choisissant <verbatim|tm-><scm|name> lors de l'ajout d'une
    bibliographie � un document. Quand une fonction de formatage n'est pas
    d�finie dans le style, celle du style <scm|default> est �valu�e en
    remplacement.
  </explain>

  <\explain>
    <scm|(bib-emphasize tm)><explain-synopsis|mise en italique>
  <|explain>
    Cette fonction renvoie un arbre <TeXmacs> correspondant � la mise en
    italique de l'abre <TeXmacs> <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-empty? entry field)><explain-synopsis|test � z�ro d'un champ>
  <|explain>
    Cette fonction renvoie le bool�en <scm|#t> si le champ de l'entr�e
    <scm|entry> dont le nom est <scm|field> (de type string) est vide ou
    inexistant ; elle renvoie <scm|#f> dans le cas contraire.
  </explain>

  <\explain>
    <scm|(bib-field entry field)><explain-synopsis|r�cup�ration d'un champ>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> correspondant au contenu du champ
    de l'entr�e <scm|entry> dont le nom est <scm|field> (de type string),
    sans aucun formatage. Dans certains cas, une donn�e sp�cifique est
    renvoy�e :

    <\itemize-dot>
      <item>si <scm|field> est �gal � <scm|"author"> ou <scm|"editor">, la
      donn�e renvoy�e est un arbre dont le label est <verbatim|bib-names>,
      contenant une liste de noms d'auteurs ; chaque nom d'auteur est un
      arbre dont le label est <verbatim|bib-name> et qui contient quatre
      �l�ments, dans l'ordre : le pr�nom, la particule, le nom et un
      qualificatif (junior ou senior).

      <item>si <scm|field> est �gal � <scm|"page">, la donn�e contient une
      liste d'entiers : soit une liste vide, soit un singleton contenant la
      page r�f�renc�e, soit un couple d�notant l'intervalle des pages
      r�f�renc�es.
    </itemize-dot>
  </explain>

  <\explain>
    <scm|(bib-format-field entry field)><explain-synopsis|formatage basique
    d'un champ>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> correspondant au contenu du champ
    de l'entr�e <scm|entry> dont le nom est <scm|field> (de type string),
    avec un formatage basique.
  </explain>

  <\explain>
    <scm|(bib-format-field-Locase entry field)><explain-synopsis|formatage
    sp�cial d'un champ>
  <|explain>
    Cette fonction est similaire � la fonction <scm|bib-format-entry> ; mais
    le champ est format� en minuscules, avec une majuscule en t�te.
  </explain>

  <\explain>
    <scm|(bib-locase tm)><explain-synopsis|mise en minucule>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> en mettant en minuscule toutes les
    lettres de l'arbre <TeXmacs> <scm|tm>, sauf celles englob�es dans un bloc
    <verbatim|keepkase>.
  </explain>

  <\explain>
    <scm|(bib-new-block tm)><explain-synopsis|nouveau bloc>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> consistant en un bloc contenant
    l'arbre <TeXmacs> <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-new-list sep ltm)><explain-synopsis|liste s�par�e>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> en concat�nant tous les �l�ments
    de la liste <scm|ltm> (ses �l�ments sont des arbres <TeXmacs>) en les
    s�parant par l'arbre <TeXmacs> <scm|sep>.
  </explain>

  <\explain>
    <scm|(bib-new-list-spc ltm)><explain-synopsis|liste s�par�e par des
    blancs>
  <|explain>
    Cette fonction est �quivalente � l'appel de <scm|(bib-new-list " " ltm)>.
  </explain>

  <\explain>
    <scm|(bib-new-sentence ltm)><explain-synopsis|nouvelle phrase>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> correspondant � une phrase
    correctement ponctu�e, contenant tous les �l�ments de la liste <scm|ltm>
    (ses �l�ments sont des arbres <TeXmacs>) s�par�s par des virgules.
  </explain>

  <\explain>
    <scm|(bib-null? v)><explain-synopsis|test � z�ro>
  <|explain>
    Cette fonction renvoie le bool�en <scm|#t> si la valeur <scm|v> est vide
    (<with|font-shape|italic|i.e.> la donn�e vide correspondant � son type,
    comme la liste vide pour le type list) ; elle renvoie <scm|#f> dans le
    cas contraire.
  </explain>

  <\explain>
    <scm|(bib-prefix tm nbcar)><explain-synopsis|pr�fix d'un arbre <TeXmacs>>
  <|explain>
    Cette fonction renvoie une cha�ne de caract�res contenant les <scm|nbcar>
    premiers caract�res de l'arbre <TeXmacs> <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-purify tm)><explain-synopsis|applatissement d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction cr�e une cha�ne de caract�res � partir des suites de
    lettres de l'arbre <TeXmacs> <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-simplify tm)><explain-synopsis|simplification d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie un arbre <TeXmacs> correspondant � la
    simplification de l'abre <TeXmacs> <scm|tm>, c'est-�-dire la
    concat�nation des cha�nes de caract�res adjacentes, l'�limination des
    n�uds inutiles, etc.
  </explain>

  <\explain>
    <scm|(bib-text-length tm)><explain-synopsis|longueur d'un arbre
    <TeXmacs>>
  <|explain>
    Cette fonction renvoie le nombre de caract�res de l'arbre <TeXmacs>
    <scm|tm>.
  </explain>

  <\explain>
    <scm|(bib-translate msg)><explain-synopsis|traduction>
  <|explain>
    Cette fonction traduit le message <scm|msg> (de type string) de l'anglais
    vers la langue du document en cours d'�dition.
  </explain>

  <\explain>
    <scm|(bib-upcase tm)><explain-synopsis|mise en majuscule>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> en mettant en majuscule toutes les
    lettres de l'arbre <TeXmacs> <scm|tm>, sauf celles englob�es dans un bloc
    <verbatim|keepkase>.
  </explain>

  <\explain>
    <scm|(bib-upcase-first tm)><explain-synopsis|majuscule en t�te>
  <|explain>
    Cette fonction cr�e un arbre <TeXmacs> en mettant en majuscule la
    premi�re lettre de l'arbre <TeXmacs> <scm|tm>, sauf si elle est englob�e
    dans un bloc <verbatim|keepkase>.
  </explain>

  <\explain>
    <scm|(bib-with-style style expr)><explain-synopsis|style local>
  <|explain>
    Cette fonction ex�cute l'expression <scm|expr> comme si le style en cours
    �tait <scm|style> (de type string).
  </explain>
</body>

<\initial>
  <\collection>
    <associate|language|french>
    <associate|preamble|false>
  </collection>
</initial>