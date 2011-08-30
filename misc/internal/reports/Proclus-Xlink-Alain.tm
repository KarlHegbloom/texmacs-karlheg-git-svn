<TeXmacs|1.0.3.6>

<style|<tuple|generic|proclus>>

<\body>
  \;

  Je commence par essayer des d�gager quelques diff�rences de conception
  entre Xlink et Proclus. Mon impression en regardant � nouveau Xlink et ton
  adaptation c'est que Xlink est "orient� loci" alors que Proclus est
  "orient� liens". Ils semblent � cet �gard duaux.

  Je pr�cise...

  Ton syst�me me semble offrir ses possibilit�s � partir des loci et en
  particulier d'�tre bien pour g�rer les loci de m�me type ou l'h�ritage : il
  privil�gie le copiage et la reproduction des loci. L'accent mis sur les id
  non uniques et la surchage en t�moigne. L'exemple (traduction du manuel)
  que tu m'as donn� me semble en fait bien relever de cela (j'y reviendrai).

  Proclus s'applique au contraire aux liens (� partir de leurs types) : le
  traitement (l'�quivalent des roles) doit se faire dans une "feuille de
  liens" qui d�terminera le traitement � appliquer suivant les types des
  liens. Mais sans modifier le contenu des liens. Les types deviennent des
  param�tres (auxquels d'autres peuvent �tre ajout�s comme tu le notais
  (notamment l'environnement)) pour des op�rations qui peuvent �tre trait�es
  globalement sur le document (faire ceci pour tous les liens de type 
  application  sauf si la source etc.) ou locale sur un locus consid�r�.
  Dans Proclus, la surchage porte sur les types.

  Cela peut se voir aussi au niveau des conflits que chacun des syst�mes a �
  g�rer. Dans le tien, ce que tu dois consid�rer ce sont les conflits qui
  viennent des loci, dans Proclus, les conflits viennent (viendront...) de la
  gestion d'un lien qui a plusieurs types.

  <locus|Mais peut-�tre ton syst�me peut-il n�anmoins permettre de reproduire
  des loci et des liens � la Proclus. Ce serait bien de tirer cela au clair.
  Cela renvoie aux questions des noms mn�motechniques et de la base de
  donn�es de liens.|<tuple|Proclus-Xlink-Alain|27|Proclus-Xlink-Joris|14|<tuple|th�me>||Proclus-Xlink-Joris|13|<tuple|th�me>|>>

  J'ai aussi toujours, peut-�tre � tort, l'impression que Xlink reste
  tributaire de son contexte de cr�ation li� au web, c'est-�-dire qu'il est
  fait d'une part pour permettre de faire des choses m�me quand les buts sont
  dans des docs dont on n'a pas les droits d'�criture et surtout qu'il est
  con�u pour cr�er des documents dynamiques mais en fait "fig�s", sur 
  lesquels on n'est plus en train de travailler. Autrement dit, Xlink est
  toujours fond� sur un mod�le s�parant auteur-lecteur ou encore le temps de
  cr�ation du document et le temps de lecture du document : l'auteur peut
  cr�er des choses sophistiqu�es et tr�s dynamiques, mais quand il le fait il
  n'est pas dans de bonnes conditions de lecture de son document, quant au
  lecteur il ne peut pas �crire. Il y a une distinction nette, une
  succession, entre le moment de la composition du document et le moment de
  sa lecture. Il me semble l� aussi que ton exemple avec le Manuel illustre
  malgr� tout un peu ceci (dans la mesure o� tu veux �viter au traducteur de
  cr�er un locus parce que cela est assez compliqu�, tu ne veux donc qu'il
  n'agisse qu'� un certain niveau m�me si ici le traducteur est bcp plus
  qu'un lecteur, mais il n'est qu'un lecteur du point de vue des liens).

  Proclus au contraire est fond� sur l'id�e de brouiller cette opposition et
  permet de cr�er des liens au moment de la lecture, ce moment �tant
  indiscernable de celui de l'�criture. Les liens dans Proclus sont � la fois
  des outils de travail et de lecture (d'o� le nom "Proclus"...). L'usage des
  liens dans ton syst�me (mais je m'en fais peut-�tre une mauvaise id�e) me
  semble assez similaire � celui des environnements : l'environnement est
  tr�s puissant mais pr�d�fini dans une feuille de style (dans un langage peu
  convivial, i.e. l'utilisateur n'est pas vraiment sens� trop y toucher) mais
  ensuite on peut en cr�er des instances tr�s ais�ment (c'est pour cela que
  Proclus peut �ventuellement �tre reproduit � l'int�rieur de celui-ci).

  \;

  Enfin, tu mets l'accent sur des liens <em|logiques>, dans Proclus les liens
  servent � stocker une information relationnelle. Mais peut-�tre est-ce
  seulement parce que tu mets l'accent sur la partie qui n'est pas disponible
  dans Proclus.

  \;

  J'essaye maintenant de voir <locus|<strong|les possibilit�s offertes par
  les noms mn�motechniques>|<tuple|Proclus-Xlink-Alain|12|Proclus-Xlink|6|<tuple|th�me>|>>,
  sans consid�rer pour l'instant la possibilit� qu'ils soient communs �
  plusieurs loci (ce serait bien que tu compl�tes la liste)

  1) cr�er un lien sans se d�placer vers le but.

  2) cr�er un lien sans avoir le droit d'�criture dans le doc. contenant le
  but.

  3) cr�er un lien avec un doc. absent lors de la cr�ation du lien.

  4) <locus|cr�er un lien avec un doc. sur lequel plusieurs personnes
  travaillent (c'est un pb que je rencontre dans l'usage de proclus au sein
  du groupe de travail sur l'histoire de la g�om�trie alg�brique r�elle) : pb
  de conflit des identifiants absolus si les personnes travaillent sur des
  copies du doc. Peut-�tre ton syst�me complexe d'identifiant absolu pourrait
  r�soudre cela. C'est � voir (il y a toujours le pb de la fusion des loci
  cr��s dans deux copies diff�rentes du m�me doc. sur une m�me partie de
  texte).|<tuple|Proclus-Xlink-Alain|13|Proclus-Xlink-Joris|13|<tuple|th�me>||Proclus-Xlink-Joris|10|<tuple|th�me>|>>\ 

  5) permet l'identification de locus diff�rents, l� encore, avec les
  avantages des points 1)-4).

  \;

  Mais tout n'est peut-�tre pas aussi idyllique... car :

  1) il faut tout de m�me conna�tre les noms mn�motechniques (sans perdre les
  avantages de 1)-4)). Y a-t-il alors une diff�rence fondamentale (dans
  l'impl�mentation) entre conna�tre les noms mn�motechniques et conna�tre les
  identifiants uniques?

  2) <locus|Risque de ne pas pouvoir cr�er de lien r�ciproque ou alors de le
  faire au prix de perdre les avantages de 1)-4). Cela nous renvoie � la base
  de liens.|<tuple|Proclus-Xlink-Alain|16|Proclus-Xlink-Joris|13|<tuple|r�f�rence>|>>

  \;

  Je distinguerai a) le confort lors de la cr�ation de liens apport� par les
  noms mn�motechniques de b) les fonctionalit�s nouvelles qu'ils permettent
  d'avoir.

  a) me semble tout � fait compatible avec Proclus tel qu'il est impl�ment�.

  b) Ce serait bien que tu donnes des exemples concrets de liens qui ne te
  semblent pas �tre couverts par Proclus, non pas dans sa version actuelle,
  mais dans ses �volutions possibles ;-). Ton exemple du triangle ne me
  convainc pas bcp...

  \;

  J'essaye maintenant de voir <locus|les possibilit�s offertes par
  <strong|les identifiants non uniques>|<tuple|Proclus-Xlink-Alain|15|Proclus-Xlink-Joris|18|<tuple|th�me>|>>
  (ce serait bien que tu compl�tes la liste) <hspace|1spc>:

  <\itemize-minus>
    <item>identifier des loci sans cr�er de lien <space|0.25fn>(gain de
    robustesse) <space|0.25fn>;
  </itemize-minus>

  Mais alors toujours la m�me question <hspace|1spc>: en quoi cela concerne
  les liens<space|0.25fn>? Ne pourrait-il pas s'agir d'une sorte d'extension
  du syt�me de liens mais autonome par rapport � celui-ci. Un syst�me
  d'identification entre loci �tant d�fini, il y aurait des options
  d'extension des liens tenant compte des identifications de loci.

  <section|R�ponses diverses>

  A lire � partir du fichier <locus|Proclus-Xlink-Joris|<tuple|Proclus-Xlink-Alain|7|Proclus-Xlink-Joris|11|<tuple|r�f�rence>|>>.

  <locus|Sur l'int�r�t pour le calcul formel je n'ai rien � dire. C'est toi
  qui peut mieux appr�cier si cela rel�ve d'un traitement par les liens. Pour
  le reste, cela me semble inutilement compliqu� si c'est seulement pour ne
  pas utiliser de \ nom absolu pour un document. M�me dans ton syst�me un nom
  absolu revient tout simplement � tjrs consid�rer le document comme un locus
  et � lui attribuer un label et ensuite � utiliser ce label comme nom
  absolu. Tu risques d'avoir besoin de ce locus par d�faut, ne serait-ce que
  pour pr�ciser la langue du document par exemple. Tu disposes donc sans pb
  d'un nom absolu. Qu'y a-t-il ensuite de si compliqu� � g�n�rer des
  identifiants automatiques ? Il suffit d'avoir quelque part dans le doc. une
  variable qui contient le num�ro du dernier id cr�� comme dans toute base de
  donn�es.<next-line>Dans Proclus ce locus pourrait toujours �tre ("nom abs",
  0). On pourrait y acc�der par le menu (car il ne contiendrait aucun texte)
  et il faudrait le prot�ger contre l'effacement.|<tuple|Proclus-Xlink-Alain|8|Proclus-Xlink-Joris|10|<tuple|question|th�me>|>>

  \;

  \;

  \<gtr\> 2) Equivalence entre identifiants uniques ou differents loci avec
  le meme identifiant unique

  <locus|Cela ne passe pas n�cessairement (ce qui ne l'exclut pas) par un
  traitement � partir du nom du locus. Cela peut se faire � partir de types
  de lien avec les types "p�re", "fils", "fr�re" (on peut trouver mieux comme
  noms...) avec les h�ritages automatiques que tu devines.<next-line>Je me
  demande si les cas qui ne rel�vent pas de ce traitement rel�vent vraiment
  d'un traitement par liens. Peut-�tre ne faut-il pas assimiler les
  glossaires, indexes, etc. � des liens. Je ne sais pas... Cela oblige �
  \ stocker de l'information qui est en fait redondante. Par exemple, les
  r�f�rences � un th�or�me doivent-elles trait�es comme des liens quand les
  th�or�mes sont num�rot�s et que leur application est signal�e de mani�re
  st�r�otyp�e dans les d�monstrations?<next-line>C'est un pb que je rencontre
  � l'usage avec Proclus. Comment g�rer ce qui est reli� en tant que m�me
  id�e<space|0.25fn>? Ce que je fais actuellement c'est donner un nom � cette
  id�e, en faire un locus plac� plus ou moins n'importe o� et de cr�er
  ensuite des liens avec ce locus. M�me si l'on pouvait retrouver cette id�e
  facilement en lui attribuant un id. mn�motechnique (ce qui am�liore d�j�
  bcp les choses), cela serait-il le traitement le plus
  satisfaisant<space|0.25fn>? La plupart des liens de ce document sont en
  d�finitive de cette esp�ce<space|0.25fn>! Il s'agit alors simplement de
  d�finir des champs typ�s. C'est bien s�r tr�s simple. On pourrait mettre
  autant de types que l'on voudrait et ensuite on d�finirait le traitement
  que l'on veut des champs de m�me type en d�finissant la port�e de ce
  traitement (quels fichiers<space|0.25fn>?). Des types logiques plus
  sophistiqu�s comme tu les envisages peuvent �tre utiles. Mais s'agit-il
  toujours de liens<space|0.25fn>?<next-line>Je me demande si l'on n'en
  arrive pas � une distinction entre <hspace|1spc>:<next-line>identification
  (liaison) par le locus <hspace|1spc>: c'est essentiellement ton
  syst�me.<next-line>identification (liaison) par un lien <hspace|1spc>:
  c'est essentiellement Proclus.|<tuple|Proclus-Xlink-Alain|9|Proclus-Xlink-Joris|12|<tuple|th�me|question>|>>\ 

  \;

  <locus|Ce qui suit peut �tre aussi � consid�rer (ce n'est pas une
  n�cessairement une alternative) : pouvoir utiliser n'importe quelle partie
  d'un locus comme identifiant sans avoir pr�alablement � la d�clarer comme
  tel. Par exemple, un th�or�me qui est dans un locus contient "somme de
  carr�s", il pourrait �tre int�ressant de pouvoir cr�er un lien en appelant
  "somme de carr�s" (ou un ensemble de mots : "somme, carr�s, Pythagore"), le
  syst�me se d�brouillant pour ensuite d�terminer l'identifiant (absolu...)
  qui correspond (et bien s�r d'indiquer quand il ne trouve pas ou s'il y a
  plusieurs r�ponses). Cela me semble int�ressant � l'usage et �vite d'avoir
  � se souvenir exactement si le nom mn�motechnique �tait "th pyth",
  "Pythagore", "Th�or�me de Pythagore", "Pyth", etc... Qu'il s'agisse d'un
  th�or�me pourrait �tre aussi indiqu� (comme tu l'envisageais dans les
  attribus). Cette solution me semble assez int�ressante car relativement
  facile � impl�menter, elle offre certains des avantages des noms
  mn�motechniques tout en �vitant d'avoir � g�rer un syst�me de
  correspondance et de conflits entre les noms
  mn�motechniques.|<tuple|Proclus-Xlink-Alain|10|Proclus-Xlink|6|<tuple|th�me|question>|>>

  <locus|Ces consid�rations sur le nom mn�motechnique me font voir que
  Proclus force actuellement les liens � �tre r�ciproques et oblige notamment
  que le document contenant le but soit actuellement accessible
  (pr�sent+droits d'�criture). C'est incontestablement un d�faut. Il n'y a
  pas de raison de forcer la r�ciprocit� et il est surtout utile de pouvoir
  cr�er des liens avec un doc. soit absent soit pour lequel je n'ai pas les
  droits d'�criture. Le seul moyen de faire cela me semble effectivement
  d'avoir des noms mn�motechniques distincts de l'identifiant.<next-line>Il
  faudrait (et il est possible) ajouter � Proclus la possibilit� de cr�er des
  liens unidirectionnels avec un identifiant mn�motechnique pour le but. Mais
  il est bien clair que l'on perd dans ces conditions le lien r�ciproque (ce
  qui est emb�tant), ce qui ne sera peut-�tre pas le cas dans ton
  syst�me.<next-line>Si en plus on veut des liens r�ciproques, il faut une
  base de liens annexes.<next-line>Et l� il faut que tu m'expliques comment
  un document avec lequel des liens ont �t� cr��s en son absence saura, une
  fois pr�sent bien s�r..., les liens cr�es avec
  lui.|<tuple|Proclus-Xlink-Alain|11|Proclus-Xlink|6|<tuple|th�me>||Proclus-Xlink-Joris|13|<tuple|th�me>|>>\ 

  \;

  <locus|Je me demande s'il ne faudrait pas d�finir un  copier-lier . Cela
  consisterait � choisir entre liens de type  p�re,  fils ,  fr�re , le
  texte affich� �tant par d�faut le m�me. <next-line>Un copier normal
  oublierait le locus.<next-line>Et basta<space|0.25fn>!<next-line>Il serait
  int�ressant (au moins dans Proclus) d'avoir une op�ration  consolidation 
  <hspace|1spc>: tous les liens enregistr�s sur un fr�re ou un p�re seraient
  alors effectivement enregistr�s dans le locus consid�r� afin de pouvoir
  pr�server les liens m�me si l'un des documents contenant un fr�re ou un
  p�re dispara�t. C'est aussi certainement un aspect sur lequel ton syst�me
  est plus solide, d'un autre c�t�, quand les liens sont stock�s dans les
  loci on risque moins de perdre <em|toute> l'information relative aux liens
  (qui est souvent � moiti� conserv�e dans le lien r�ciproque stock�
  ailleurs) mais on risque plus souvent d'en perdre un peu en d�truisant un
  locus...|<tuple|Proclus-Xlink-Alain|14|Proclus-Xlink-Joris|14|<tuple|th�me>||Proclus-Xlink-Joris|17|<tuple|th�me>|>>

  \;

  <locus|<strong|Impl�mentation des noms mn�motechniques sur
  Proclus>.<next-line>De la m�me mani�re que pour les abs.
  names.<next-line>Cr�ation d'un dictionnaire dans un fichier global annexe.
  Les noms mn�motechniques n'interviennent qu'en surface <hspace|1spc>: ils
  ne sont pas stock�s dans le locus, les liens sont toujours enregistr�s �
  partir des identifiants absolus.|<tuple|Proclus-Xlink-Alain|17|Proclus-Xlink|6|<tuple|th�me>|>>

  \;

  <locus|Concernant l'exemple que tu m'as donn� du manuel dans les
  diff�rentes langues.<next-line>En fait, je ne vois plus bien son int�r�t...
  Je ne vois pas ce qu'il y a de compliqu� pour le traducteur � cr�er un lien
  qui pourrait n'�tre d'ailleurs ici qu'un lien html (le lien inverse n'�tant
  pas tr�s int�ressant). <next-line>Dans Proclus, le traducteur cr�erait tout
  simplement un lien de type "r�f�rence" vers la traduction en \ y renvoyant
  �ventuellement par un nom mn�motechnique si le document n'est pas
  accessible (car c'est effectivement qch d'utile). Au pire, il peut cr�er un
  lien de type "traduction" et ensuite au niveau de la "feuille de liens" il
  sera fait ce qu'il faut pour trouver la bonne langue. Mais il n'est pas
  difficile pour un traducteur de conna�tre sa langue (!) et cela peut �tre
  mis parmi les types par d�faut.<next-line>Je comprends que tu veux stocker
  � part la structure des liaisons commune aux diff�rentes langues de
  traduction, est-ce vraiment tr�s important? Pourrais-tu me donner un
  exemple o� cela serait vraiment int�ressant?<next-line>Encore une fois, le
  gain possible que je vois dans ton syst�me c'est de pouvoir cr�er sans
  avoir acc�s au doc. contenant le but des liens r�ciproques, alors qu'il est
  clair que dans l'impl�mentation actuelle de Proclus cela n'est pas possible
  (mais peut �tre fait en renon�ant au lien
  r�ciproque).|<tuple|Proclus-Xlink-Alain|19|Proclus-Xlink|6|<tuple|th�me>|>>

  <\locus>
    Cela n�cessite des identifiants mn�motechniques qui requi�rent
    eux-m�mes<next-line>

    de pouvoir conna�tre ces identifiants sans avoir acc�s au doc.<next-line>

    de retrouver le locus � partir de cet identifiant<next-line>

    J'aimerais mieux comprendre comment tu fais cela.<next-line>

    \ Ton syst�me supporte-t-il bien par exemple l'exportation de seulement
    quelques-uns de tes\ 

    documents li�s<space|0.25fn>?<next-line>

    Tu va devoir exporter ta base de liens. Il n'y en qu'une
    seule<space|0.25fn>? <abbr|etc.>

    \;
  </locus|<tuple|Proclus-Xlink-Alain|20|Proclus-Xlink|6|<tuple|th�me>||Proclus-Xlink-Joris|14|<tuple|th�me>|>>

  <locus|Bilan sur le copier-coller<next-line>Actuellement, tu peux copier un
  locus dans le doc. d'o� il provient<space|0.25fn>; toutes les copies seront
  des locus fr�res (il ne faut pas changer le
  texte).|<tuple|Proclus-Xlink-Alain|22>>
</body>

<\initial>
  <\collection>
    <associate|absolute-name|Proclus-Xlink-Alain>
    <associate|language|french>
    <associate|locus-num|27>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|proclus-active-types|<tuple|r�f�rence|citation|application|li�|r�ciproque|utilise|g�n�ralisation|r�f�rent|exemple|d�veloppement|id�e|d�finition|th�or�me|commentaire
    sur|commentaire|contre-exemple|cas particulier|question|th�me|analyse|Fran�ais|Latin|Anglais|Allemand|bla|blou|r�ponse|r�ponose|question>>
    <associate|proclus-type-list|<tuple|r�f�rence|citation|application|li�|r�ciproque|utilise|g�n�ralisation|r�f�rent|exemple|d�veloppement|id�e|d�finition|th�or�me|commentaire
    sur|commentaire|contre-exemple|cas particulier|question|th�me|analyse|Fran�ais|Latin|Anglais|Allemand|bla|blou|r�ponse|r�ponose|question>>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|R�ponses
      diverses> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <pageref|auto-1><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>