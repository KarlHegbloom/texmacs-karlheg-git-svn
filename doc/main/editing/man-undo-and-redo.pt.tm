<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Desfazendo e refazendo altera��es>

  � poss�vel desfazer gradualmente todas as mudan�as que voc� fez em um
  documento a partir do momento em que voc� iniciou o <apply|TeXmacs>. Isto
  pode ser feito com o menu <apply|menu|Edit|Undo> ou usando as teclas
  <expand|kbd-gen|[> ou <key|C-/>. Altera��es desfeitas podem ser refeitas
  com <apply|menu|Edit|Redo> ou <expand|kbd-gen|]>.

  Para economizar mem�ria, o n�mero de a��es sucessivas que podem ser
  desfeitas � limitado normalmente a 100. � poss�vel aumentar este n�mero
  adicionando um comando como

  <\verbatim>
    \ \ \ \ (set-maximal-undo-depth 1000)
  </verbatim>

  ao seu arquivo pessoal de inicializa��o (ver <apply|menu|Help|Scheme>).
  Quando voc� especifica um n�mero negativo como a profundidade m�xima, um
  n�mero arbitr�rio de altera��es (sujeito � disponibilidade de mem�ria do
  computador) de altera��es podem ser desfeitas.

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Desfazer>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Refazer>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Ajuda>|<with|font
      family|<quote|ss>|Scheme>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
