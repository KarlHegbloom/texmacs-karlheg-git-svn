<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Environnements math�matiques>

  Le d.t.d. <tmdtd|env-math> sp�cifie quels environnements math�matiques
  peuvent �tre utilis�s en mode texte. En d'autres termes, ces environnements
  doivent �tre utilis�s en mode texte, mais leur corps contient des formules
  ou tableaux de formules math�matiques.

  <\explain|<markup|equation>>
    �quation num�rot�e.
  </explain>

  <\explain|<markup|equation*>>
    �quation non num�rot�e.
  </explain>

  <\explain|<markup|eqnarray>>
    Liste d'�quations num�rot�es (� ne pas encore utiliser).
  </explain>

  <\explain|<markup|eqnarray*>>
    Liste d'�quations non num�rot�es.
  </explain>

  En environnement <markup|eqnarray*>, on peut utiliser la balise
  <markup|eq-number> pour num�roter l'�quation.\ 

  <\warning>
    La num�rotation des �quations dans les tableaux n'est pas encore � son
    optimum. La balise <markup|eqnarray> est identique � <markup|eqnarray*>
    pour le moment. Quand la balise <markup|eqnarray> sera impl�ment�e
    correctement, vous aurez acc�s � une balise <markup|no-number> pour
    supprimer la num�rotation d'une �quation et un package de style pour
    appliquer une num�rotation � gauche de l'�quation..
  </warning>

  <\warning>
    Il n'existe, pour l'instant, pas d'option pour num�roter les �quations �
    gauche. N�anmoins, vous pouvez utiliser la balise <markup|leq-number>
    pour ce faire. La balise <markup|next-number> vous permet �galement
    d'afficher directement le num�ro et d'incr�menter le compteur d'�quation.
  </warning>

  <\warning>
    �vitez d'utiliser les environnements AMS-<TeX> <verbatim|align>,
    <verbatim|gather> et <verbatim|split>. N�anmoins, si vous d�sirez le
    faire, ils sont disponibles sous les noms suivants : <markup|align>,
    <markup|gather>, <markup|eqsplit> de m�me que leurs variantes :
    <markup|align*>, <markup|gather*> et <markup|eqsplit*>. Nous pr�voyons
    d'impl�menter des environnements plus puissants plus tard.
  </warning>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

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