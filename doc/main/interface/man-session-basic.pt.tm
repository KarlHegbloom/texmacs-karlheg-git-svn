<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Uso b�sico>

  Uma se��o interativa pode ser iniciada com o menu
  <apply|menu|Text|Session>. Uma se��o consiste de uma seq��ncia de ambientes
  de entrada e sa�da, possivelmente com algum texto entre eles. Quando voc�
  pressiona <key|return> dentro de um ambiente de entrada de uma
  sess�o, o texto dentro deste ambiente � executado e o resultado � mostrado
  no ambiente de sa�da.

  Quando voc� envia um comando para um interpretador, o aplicativo tenta
  execut�-lo. V�rios comandos podem ser executados ao mesmo tempo no mesmo
  documento, mas a sa�da s� ser� ativa para a sess�o na qual o cursor est�
  localizado, e no local exato do cursor. Desta forma, recomendamos que o uso
  de buffers diferentes para execu��es em paralelo.

  Para cada tipo de aplicativo externo, voc� pode escolher entre compartilhar
  um �nico processo por todas as sess�es abertas, ou iniciar um processo
  diferente para cada sess�o. Mais precisamente, quando voc� insere uma
  sess�o com <apply|menu|Text|Session|Other>, voc� pode especificar tanto o
  ``tipo da sess�o'' (Shell, Pari, Maxima, etc.) quando um ``nome da sess�o''
  (o nome padr�o � ``default''). Sess�es com nomes diferentes correspondem a
  processos diferentes, e sess�es com o mesmo nome compartilham um mesmo
  processo.

  Para terminar o processo subjacente a uma dada sess�o, voc� pode usar
  <apply|menu|Session|Close session>. Quando voc� tecla
  <key|return> na entrada de um sistema que n�o est� conectado,
  ele ser� reiniciado automaticamente. Voc� tamb�m pode usar
  <apply|menu|Session|Interrupt execution> para interromper a execu��o de um
  comando. V�rios aplicativos, no entanto, n�o tem esta funcionalidade.

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
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Sess�o>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
