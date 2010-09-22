<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formules math�matiques>

  Pour saisir des formules math�matiques, vous devez d'abord passer en
  <space|0.2spc>math mode<space|0.2spc> en appuyant sur la touche <key|$>
  ou en ins�rant une �quation (avec <apply|menu|Text|Mathematics|Equation>).
  En mode maths, il existe des commandes sp�cifiques et des raccourcis pour
  saisir des symboles et des formules math�matiques. Par exemple, le pr�fixe
  <prefix|H-> permet de saisir des symboles grecs (rappelez-vous que <prefix|H->
  est �quivalent � <prefix|math:greek>, <key|escape escape escape> ou <prefix|A-C->).

  L'�diteur traduit les formules selon certaines r�gles. Cette
  caract�ristique, qui sera d�velopp�e dans les versions suivantes, est utile
  lors de la communication avec un logiciel de calcul formel. Pour l'instant,
  vous devez saisir explicitement le signe multipli� <key|*> entre deux
  symboles <with|mode|math|a> et <with|mode|math|b>. Par d�faut, la saisie de
  <key|a b> donne <with|mode|math|mode|text|ab> et non <with|mode|math|a*b>.

  <\traverse>
    <apply|branch|Principaux objets math�matiques|keyboard/man-main.fr.tm>

    <apply|branch|Symboles math�matiques|keyboard/man-symbols.fr.tm>

    <apply|branch|Grands symboles|keyboard/man-big.fr.tm>

    <apply|branch|Grands d�limiteurs|keyboard/man-large.fr.tm>

    <apply|branch|Larges accents|keyboard/man-wide.fr.tm>
  </traverse>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Math�matiques>|<with|font
      family|<quote|ss>|�quation>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
