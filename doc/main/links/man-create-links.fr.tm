<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Cr�ation d'�tiquettes, de liens et de r�f�rences>

  Vous pouvez cr�er une nouvelle �tiquette inactive avec \ <expand|kbd-gen|!>
  ou <apply|menu|Insert|Link|Label> et une r�f�rence � cette �tiquette avec
  <expand|kbd-gen|?> ou <apply|menu|Insert|Link|Reference>. Faites attention
  � mettre l'�tiquette � un endroit o� sa num�rotation est correcte. Lors de
  l'�tiquetage de sections, il vaut mieux placer l'�tiquette juste apr�s le
  nom de la section. Lors de l'�tiquetage d'�quations, il vaut mieux placer
  l'�tiquette juste au d�but de l'�quation.

  On peut cr�er des hyperliens vers d'autres documents avec
  <expand|kbd-ia|\<gtr\>> ou <apply|menu|Insert|Link|Hyperlink>. Le premier
  champ de l'hyperlien est le texte associ�, affich� en bleu lorsque le lien
  est activ�. Le deuxi�me champ contient le nom d'un document, qui peut �tre
  sur la toile. Comme d'habitude pour les hyperliens, un lien de la forme
  <verbatim|#<with|font shape|italic|�tiquette>> pointe vers une �tiquette �
  l'int�rieur du document et un lien de la forme <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|�tiquette>> pointe vers une
  �tiquette dans le document situ� � l'adresse <verbatim|<with|font
  shape|italic|url>>.

  De m�me, on peut associer une action � un texte ou une image avec
  <expand|kbd-ia|*> ou <apply|menu|Insert|Link|Action>. Le second champ
  contient alors un script Guile/Scheme, qui est ex�cut� quand on
  double-clique sur le texte apr�s son activation. Pour des raisons de
  s�curit�, ces scripts sont parfois refus�s. Par d�faut, on vous demande si
  vous acceptez le script ; on peut changer ce comportement par d�faut avec
  <apply|menu|Edit|Preferences|Security>. Notez que la commande Guile/Scheme
  :\ 

  <\verbatim>
    \ \ \ \ (system "commande-shell")
  </verbatim>

  �value <verbatim|commande-shell> en tant que commande shell.

  Enfin, vous pouvez inclure directement d'autres documents dans un document
  donn� avec <expand|kbd-ia|i> ou <apply|menu|Insert|Link|Include>. Cela vous
  permet, par exemple, d'inclure le listing d'un programme dans votre
  document de telle fa�on que les modifications dans votre programme se
  refl�tent automatiquement dans le document.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|�tiquette>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|R�f�rence>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Hyperlien>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Action>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Options>|<with|font
      family|<quote|ss>|Securit�>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Lien>|<with|font family|<quote|ss>|Inclure>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
