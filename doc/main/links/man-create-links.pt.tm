<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Criando r�tulos, liga��es e referencias>

  Voc� pode criar um novo r�tulo, que � inicializado desativado, usando
  \ <expand|kbd-gen|!> ou <apply|menu|Insert|Link|Label> e uma refer�ncia
  para este r�tulo usando <expand|kbd-gen|?> ou
  <apply|menu|Insert|Link|Reference>. Tome cuidado e insira o r�tulo em um
  ponto no qual seu n�mero ser� correto. Quando rotular se��es, por exemplo,
  a posi��o recomendada � logo ap�s o nome da se��o. Quando voc� rotular
  equa��es, o local recomendado � dentro da equa��o, em seu in�cio.

  � poss�vel criar hiper liga��es para outros documentos usando
  <expand|kbd-ia|\<gtr\>> ou <apply|menu|Insert|Link|Hyperlink>. O primeiro
  campo da hiper liga��o � o texto associado, que � exibido em azul quando a
  hiper liga��o est� ativa. O segundo campo cont�m o nome de um documento,
  que pode inclusive estar na rede. Como � usual para hiper liga��es, uma
  liga��o da forma <verbatim|#<with|font shape|italic|label>> aponta para o
  mesmo documento e uma liga��o da forma <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|label>> aponta para um r�tulo no
  documento localizado na <verbatim|<with|font shape|italic|url>>.

  Da mesma forma, uma a��o pode ser associada a um trecho de texto ou gr�fico
  usando <expand|kbd-ia|*> ou <apply|menu|Insert|Link|Action>. O segundo
  campo agora cont�m um script Guile/Scheme, que � executado sempre que voc�
  clica duplamente naquele texto, desde que o mesmo esteja ativo. Por motivos
  de seguran�a, a execu��o destes scripts n�o � autom�tica. O comportamento
  padr�o � perguntar para voc� se voc� aceita a execu��o; isto pode ser
  alterado em \ <apply|menu|Options|Security>. Note que o comando
  Guile/Scheme

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  executa <verbatim|shell-command> como um comando do seu shell.

  Finalmente, voc� pode incluir outros documentos diretamente dentro de um
  dado documento usando <expand|kbd-ia|i> ou
  <apply|menu|Insert|Link|Include>. Isto permite, por exemplo, que seja
  inclu�da uma listagem de um programa no seu texto, de forma que altera��es
  no programa sejam automaticamente refletidas no texto.

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
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Etiqueta>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Refer�ncia>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Hiperlink>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|A��o>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Op��es>|<with|font
      family|<quote|ss>|Seguran�a>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Incluir>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
