<TeXmacs|1.0.7.10>

<style|tmdoc>

<\body>
  <tmdoc-title|R�gles g�n�rales applicables aux pr�fixes>

  Comme il existe de nombreux raccourcis clavier, il est important de d�finir
  des r�gles de classification de fa�on � les m�moriser facilement. En
  g�n�ral, les raccourcis clavier d'une m�me cat�gorie ont le m�me pr�fixe.
  Les pr�fixes actives d�pendent fortement du mode de clavier choisi dans
  <menu|�diter|Pr�f�rences|Aspect de l'interface>. Dans l'interface actuelle
  de <TeXmacs>, les pr�fixes les plus courants sont :\ 

  <\description>
    <item*|<prefix|C->>Les raccourcis clavier bas�s sur la touche contr�le
    sont utilis�s pour les commandes d'�dition courantes. Ils d�pendent de
    l'\Sapparence\T d�finie avec <menu|�dition|Pr�f�rences>. Si vous utilisez
    une apparence <name|Emacs>, les raccourcis clavier de forme <prefix|C->
    correspondent aux commandes <name|Emacs>. Exemple : <key|C-y> pour coller
    du texte.

    <item*|<prefix|A->>La touche alt est utilis�e pour les commandes qui
    d�pendent du mode dans lequel vous �tes. Par exemple, <key|text s> g�n�re
    du texte <strong|important> en mode texte et une racine carr�e
    <math|<sqrt|>> en mode maths. Notez que <key|escape escape> est
    �quivalent � <prefix|A->.

    <item*|<prefix|M->>La touche meta est utilis�e pour les commandes
    <TeXmacs> g�n�rales, celles qui peuvent �tre utilis�es dans tous les
    modes. Par exemple, <key|executable> est utilis� pour ins�rer une balise
    executable, ce qui est utile pour l'<hlink|�criture de fichiers de
    styles|../../../devel/style/style.fr.tm>. Le raccourci <key|executable +>
    constitue un exemple pour l'insertion d'une addition.

    <item*|<prefix|M-A->>La touche hyper est utilis�e pour g�n�rer des
    symboles sp�ciaux, tels les caract�res grecs en mode maths. Vous pouvez
    configurer votre clavier de telle sorte que la touche majuscule fixe joue
    le r�le de la touche hyper. La touche de fonction <prefix|math:greek> est
    �quivalente � <prefix|M->.
  </description>

  Rappelons que les touches sp�ciales de modification utilis�es pour g�n�rer
  les pr�fixes <prefix|M-> et <prefix|M-A-> peuvent �tre
  <hlink|configur�es|../../config/man-config-kbd-modkeys.fr.tm> avec
  <menu|�dition|Pr�f�rences>.

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