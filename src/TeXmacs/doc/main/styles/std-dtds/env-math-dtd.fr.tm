<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Environnements math�matiques>

  Le d.t.d. <tmdtd|env-math> sp�cifie quels environnements math�matiques
  peuvent �tre utilis�s en mode texte. En d'autres termes, ces environnements
  doivent �tre utilis�s en mode texte, mais leur corps contient des formules
  ou tableaux de formules math�matiques.

  <\description>
    <expand|item*|<markup|equation>>�quation num�rot�e.

    <expand|item*|<markup|equation*>>�quation non num�rot�e.

    <expand|item*|<markup|eqnarray>>Liste d'�quations num�rot�es (� ne pas
    encore utiliser).

    <expand|item*|<markup|eqnarray*>>Liste d'�quations non num�rot�es.
  </description>

  En environnement <markup|eqnarray*>, on peut utiliser la balise
  <markup|eqnumber> pour num�roter l'�quation.\ 

  <\warning>
    La num�rotation des �quations dans les tableaux n'est pas encore � son
    optimum. La balise <markup|eqnarray> est identique � <markup|eqnarray*>
    pour le moment. Quand la balise <markup|eqnarray> sera impl�ment�e
    correctement, vous aurez acc�s � une balise <markup|nonumber> pour
    supprimer la num�rotation d'une �quation et un package de style pour
    appliquer une num�rotation � gauche de l'�quation..
  </warning>

  <\warning>
    Il n'existe, pour l'instant, pas d'option pour num�roter les �quations �
    gauche. N�anmoins, vous pouvez utiliser la balise <markup|leqnumber> pour
    ce faire. La balise <markup|nextnumber> vous permet �galement d'afficher
    directement le num�ro et d'incr�menter le compteur d'�quation.
  </warning>

  <\warning>
    �vitez d'utiliser les environnements AMS-<TeX> <verbatim|align>,
    <verbatim|gather> et <verbatim|split>. N�anmoins, si vous d�sirez le
    faire, ils sont disponibles sous les noms suivants : <markup|align>,
    <markup|gather>, <markup|eqsplit> de m�me que leurs variantes :
    <markup|align*>, <markup|gather*> et <markup|eqsplit*>. Nous pr�voyons
    d'impl�menter des environnements plus puissants plus tard.
  </warning>

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|1|?>>
    <associate|idx-9|<tuple|1|?>>
    <associate|idx-10|<tuple|1|?>>
    <associate|idx-11|<tuple|1|?>>
    <associate|idx-12|<tuple|2|?>>
    <associate|idx-13|<tuple|2|?>>
    <associate|idx-14|<tuple|3|?>>
    <associate|idx-15|<tuple|3|?>>
    <associate|idx-16|<tuple|3|?>>
    <associate|idx-17|<tuple|3|?>>
    <associate|idx-18|<tuple|3|?>>
    <associate|idx-19|<tuple|3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-math>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|equation*>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnumber>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqnarray>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nonumber>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|leqnumber>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|nextnumber>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|align*>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|gather*>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|eqsplit*>>|<pageref|idx-19>>
    </associate>
  </collection>
</auxiliary>
