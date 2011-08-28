<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|V�rifier l'orthographe>

  Si le programme <verbatim|ispell> est install� sur votre syst�me, vous
  pouvez l'utiliser pour v�rifier les fautes d'orthographe avec
  <shortcut|(spell-start)> or <apply|menu|Edit|Spell>. Notez que vous devez
  v�rifier que vous avez install� les dictionnaires correspondant aux langues
  utilis�es dans vos textes ; c'est en g�n�ral le cas pour l'anglais.

  Apr�s activation de la v�rification d'orthographe (soit sur le texte
  int�gral, soit sur une zone particuli�re du texte), les options suivantes
  sont � votre disposition dans le pied de page en cas de faute d'orthographe
  :

  <\description>
    <expand|item*|a)>Accepter le mot tel quel ainsi que ses occurrences
    suivantes.

    <expand|item*|r)>Remplacer le mot par un mot nouveau � saisir.

    <expand|item*|i)>Indiquer que le mot <space|0.2spc>litigieux<space|0.2spc>
    est en fait correct et qu'il doit �tre incorpor� dans votre dictionnaire
    personnel.

    <expand|item*|1-9)>Choisir une des suggestions propos�es.
  </description>

  Notez que <verbatim|ispell> n'effectue qu'une correction orthographique. Il
  ne sait pas d�tecter les fautes de grammaire.

  Le v�rificateur d'orthographe utilise le dictionnaire correspondant � la
  langue active � la position du curseur (ou du d�but d'une s�lection). Seul
  le texte �crit dans cette langue est v�rifi�. Si votre document contient
  plusieurs langues, vous devrez lancer le v�rificateur d'orthographe pour
  toutes ces langues.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|�diter>|<with|font
      family|<quote|ss>|Orthographe>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
