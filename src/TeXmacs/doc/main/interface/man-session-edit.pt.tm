<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Editando sess�es interativas>

  Dentro dos campos de entrada de sess�es interativas, as teclas de cursor
  tem significado especial: quando voc� move o cursor para cima ou para
  baixo, voc� move a entrada para os campos de entrada anteriores ou
  posteriores. Quando voc� usa as teclas de movimento para a esquerda ou
  direita, voc� nunca deixa o campo de entrada atual, voc� precisa usar o
  mouse para mover o cursor lateralmente para fora de um campo de entrada.

  Os menus <apply|menu|Session|Insert fields> e <apply|menu|Session|Remove
  fields> fornecem algumas facilidades para editar os campos de entrada,
  sa�da e textos. A maioria das opera��es aplica-se diretamente a um par
  correspondente de campos de entrada e sa�da. Opcionalmente, um campo com um
  texto explicativo pode ser associado com um campo de entrada com
  <apply|menu|Session|Insert fields|Insert text field>. Atalhos do teclado
  para inser��o de campos s�o <key|A-<expand|key-up>> (insere acima) a
  <key|A-<expand|key-down>> (insere abaixo). Atalhos de teclado para campos
  de texto/entrada/sa�da correspondentes s�o \ <key|A-<expand|key-backspace>>
  (remove para tr�s) e <key|A-<expand|key-delete>> (remove os campos
  correntes).

  � poss�vel criar ``sub-sess�es'' usando <apply|menu|Session|Insert
  fields|Fold input field> ou <key|A-<expand|key-right>>. Neste caso, o campo
  corrente de texto, entrada ou sa�da torna-se o corpo de uma sub-sess�o
  ``desdobrada''. Esta sub-sess�o consiste de um texto explicativo junto a
  uma seq��ncia de campos de entrada e sa�da. Subsess�es podem ser
  ``dobradas'' e ``desdobradas'' com \ <key|M-A-<expand|key-up>> e
  <key|M-A-<expand|key-down>>, respectivamente. A formata��o gr�fica das
  subsess�es na tela � muito atraente quando se usa o pacote
  <tmpackage|varsession> que est� em <apply|menu|Document|Use
  package|Program>.

  Outras opera��es de edi��o �teis para campos de text/entrada/sa�da s�o
  <apply|menu|Session|Remove fields|Remove all output fields>, que � �til
  para criar sess�es de demonstra��o que ser�o executadas mais tarde, e
  <apply|menu|Session|Split session>, que pode ser usada para dividir uma
  se��o em v�rias partes para inclus�o em um artigo.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdor>

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
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Insert fields>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Remove fields>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Insert fields>|<with|font family|<quote|ss>|Insert
      text field>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Insert fields>|<with|font family|<quote|ss>|Fold
      input field>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|brown>|varsession>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usar pacote>|<with|font
      family|<quote|ss>|Programa>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Remove fields>|<with|font family|<quote|ss>|Remove
      all output fields>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|ss>|Sess�o>|<with|font
      family|<quote|ss>|Split session>>|<pageref|idx-8>>
    </associate>
  </collection>
</auxiliary>
