<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|R�gles g�n�rales applicables aux pr�fixes>

  Comme il existe de nombreux raccourcis clavier, il est important de d�finir
  des r�gles de classification de fa�on � les m�moriser facilement. En
  g�n�ral, les raccourcis clavier d'une m�me cat�gorie ont le m�me pr�fixe.
  Les pr�fixes les plus courants sont :\ 

  <\description>
    <expand|item*|<prefix|C->>Les raccourcis clavier bas�s sur
    la touche contr�le sont utilis�s pour les commandes d'�dition courantes.
    Ils d�pendent de l'apparence d�finie avec
    <apply|menu|�dition|Pr�f�rences>. Si vous utilisez une apparence
    <name|Emacs>, les raccourcis clavier de forme <prefix|C->
    correspondent aux commandes <name|Emacs>. Exemple : <key|C-y> pour coller
    du texte.

    <expand|item*|<prefix|A->>La touche alt est utilis�e pour
    les commandes qui d�pendent du mode dans lequel vous �tes. Par exemple,
    <key|text s> g�n�re du texte <strong|important> en mode texte et
    une racine carr�e <with|mode|math|<sqrt|>> en mode maths. Notez que
    <key|escape escape> est �quivalent � <prefix|A->.

    <expand|item*|<prefix|M->>La touche meta est utilis�e pour
    les commandes <apply|TeXmacs> g�n�rales, celles qui peuvent �tre
    utilis�es dans tous les modes. Par exemple, <shortcut|(make-label)> g�n�re une
    �tiquette. Elle est aussi utilis�e pour certaines commandes d'�dition
    avec l'apparence Emacs, comme <key|A-w> pour copier du texte. Notez que
    <key|escape> est �quivalent � <prefix|M->.

    <expand|item*|<prefix|H->>La touche hyper est utilis�e
    pour g�n�rer des symboles sp�ciaux, tels les caract�res grecs en mode
    maths. Vous pouvez configurer votre clavier de telle sorte que la touche
    majuscule fixe joue le r�le de la touche hyper. La touche de fonction
    <prefix|math:greek> est �quivalente � <prefix|M->.
  </description>

  Rappelons que les touches sp�ciales de modification utilis�es pour g�n�rer
  les pr�fixes <prefix|M-> et <prefix|H-> peuvent �tre
  <apply|hyper-link|configur�es|../../config/man-config-kbd-modkeys.fr.tm>
  avec <apply|menu|�dition|Pr�f�rences>.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|�dition>|<with|font
      family|<quote|ss>|Pr�f�rences>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|�dition>|<with|font
      family|<quote|ss>|Pr�f�rences>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
