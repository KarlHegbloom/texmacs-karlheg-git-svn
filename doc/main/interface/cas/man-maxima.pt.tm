<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Maxima>

  <name|Maxima> n�o � somente um dos mais antigos e melhores sistemas de
  �lgebra computacional dispon�veis, ele � tamb�m um dos �nicos sistemas
  gerais cuja distribui��o � livre. Voc� pode obt�-lo em:

  <\verbatim>
    \ \ \ \ http://www.ma.utexas.edu/users/wfs/maxima.html
  </verbatim>

  A vers�o suportada � <name|Maxima> 5.6 baseada no <name|GCL>. Para a vers�o
  <name|Maxima> 5.6 baseada no <name|CLisp>, edite o seu arquivo
  <verbatim|tm_maxima> e substitua <verbatim|-load> por <verbatim|-i>. Para a
  vers�o <name|Maxima> 5.9-pre, substitua <verbatim|-load> por <verbatim|-p>.
  Problemas conhecidos:

  <\itemize>
    <item>Se voc� pressionar <key|return> quando um comando n�o
    completou, (tipicamente, terminado por <verbatim|;> ou <verbatim|$>), a
    interface vai travar.

    <item> Se voc� fizer com que o prompt do interpretador Lisp apare�a, a
    interface vai travar.

    <item>O comando <verbatim|info> n�o est� implementado (ele � definido no
    Lisp subjacente, e � muito dif�cil de implementar de maneira port�til).

    <item>Alguns comandos do depurador funcionam, mas alguns (incluindo
    <verbatim|:c>) n�o, e ningu�m sabe porque.

    <item>O comando <verbatim|load> as vezes se comporta misteriosamente.
  </itemize>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

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
    <associate|language|portuguese>
  </collection>
</initial>
