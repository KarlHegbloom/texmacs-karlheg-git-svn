<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configuration des touches sp�ciales>

  <apply|TeXmacs> utilise 5 touches sp�ciales : <key|majuscule temporaire>,
  <key|ctrl>, <key|alt>, <key|meta> et <key|hyper>, dont les abr�viations
  seront les suivantes dans ce guide : <key|S->, <key|C->, <key|A->, <key|M->
  et <key|H->. Les touches <key|majuscule temporaire> et <key|ctrl> sont
  pr�sentes sur tous les claviers et la touche <key|alt> sur la plupart
  d'entre eux. La majorit� des claviers des PC r�cents ont aussi une touche
  <key|windows>, qui est l'�quivalent de la touche <key|meta> de <TeXmacs>.

  Avant de vous lancer dans la reconfiguration de votre clavier, v�rifiez que
  cela est vraiment n�cessaire. Si votre clavier poss�de des touches
  correspondantes aux touches <key|majuscule>, <key|ctrl>, <key|alt> et
  <key|meta> et qui fonctionnent de fa�on satisfaisante, il est plus que
  probable que vous n'ayez rien � faire. Cependant, si vous d�sirez utiliser
  la touche <key|majuscule fixe> pour saisir des symboles math�matiques, vous
  devrez alors faire correspondre la touche <key|majuscule fixe> � la touche
  <key|hyper>.

  Pour reconfigurer votre clavier, s�lectionner la touche logique �
  reconfigurer et faites-la correspondre � la touche physique d�sir�e avec
  <apply|menu|Edit|Preferences|Keyboard>. Par exemple, en s�lectionnant
  <apply|menu|Windows key|Envoyer vers modificateur M>, la touche
  <key|windows> sera mise en correspondance avec la touche <key|meta>. De
  m�me, en s�lectionnant <apply|menu|Touche Majuscule fixe|Envoyer vers
  modificateur H>, la touche <key|majuscule fixe> sera mise en correspondance
  avec la touche <key|hyper>.

  Malheureusement, X Window ne permet qu'une reconfiguration globale. Si,
  donc, vous reconfigurez la touche <key|majuscule fixe> dans
  <apply|TeXmacs>, alors cette reconfiguration s'appliquera aussi � toutes
  les autres applications. Par cons�quent, il est primordial que vous ne
  reconfiguriez que les touches que vous n'utilisez jamais dans d'autres
  applications. Par exemple, la touche <key|windows> n'est pas tellement
  utilis�e, il est donc en g�n�ral possible de la reconfigurer sans que cela
  tire � cons�quences. Vous pouvez aussi changer la configuration globale de
  fa�on appropri�e. Utilisez pour ce faire la commande <verbatim|xmodmap> ;
  voir la page man correspondante pour de plus amples informations.

  Dans certains cas, votre clavier poss�de les touches <key|alt>, <key|meta>
  et <key|hyper>, mais leur fonctionnement ne vous convient pas. Vous pouvez
  alors changer leur comportement en faisant correspondre les touches
  <key|A->, <key|M-> et <key|H-> � d'autres touches logiques dans le premier
  groupe de sous-menus du menu <apply|menu|Edit|Preferences|Keyboard>.

  Par exemple et pour des raisons de compatibilit� avec Emacs, vous pouvez
  �changer les touches <key|meta> ou <key|windows> avec la touche <key|alt>
  sans pour autant que ce changement soit global. Pour ce faire, recherchez
  quelles sont les modificateurs qui correspondent � ces touches ; en
  g�n�ral, <key|Mod1> correspond � <key|alt> et <key|Mod4> � <key|meta> ou
  <key|windows>. Vous effectuerez ensuite le changement dans
  <apply|menu|Edit|Preferences|Keyboard>, en s�lectionnant
  <apply|menu|Modificateur A|�quivalent de Mod4> et <apply|menu|Modificateur
  M|�quivalent de Mod1>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Mich�le Garoche>

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Pr�f�rences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Touche windows>|<with|font
      family|<quote|ss>|Envoyer vers modificateur M>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Touche Majuscule
      fixe>|<with|font family|<quote|ss>|Envoyer vers modificateur
      H>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Pr�f�rences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Pr�f�rences>|<with|font
      family|<quote|ss>|Clavier>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modificateur A>|<with|font
      family|<quote|ss>|�quivalent de Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modificateur M>|<with|font
      family|<quote|ss>|�quivalent de Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
