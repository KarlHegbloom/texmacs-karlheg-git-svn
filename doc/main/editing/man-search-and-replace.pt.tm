<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Encontrar e Substituir>

  Voc� pode iniciar uma busca pressionando <key|C-s> ou com
  <apply|menu|Edit|Search>. Durante a busca, a ``cadeia procurada'' �
  mostrada no lado esquerdo do rodap�. Cada caracter que voc� digita �
  adicionado a esta cadeia, e a pr�xima ocorr�ncia da cadeia � mostrada com
  um quadro vermelho. Quando a tecla <key|C-s> � pressionada uma segunda vez
  durante uma busca, a pr�xima ocorr�ncia � procurada. Um sinal sonoro indica
  que n�o existem mais outras ocorr�ncias da cadeia procurada; pressionar
  <key|C-s> novamente faz com que a busca recomece do in�cio do documento.
  Voc� pode usar a tecla <key|backspace> para apagar caracteres
  digitados durante a busca.

  Normalmente, o texto � procurado para a frente, come�ando da posi��o
  corrente do cursos. Voc� tamb�m pode procurar para tr�s, usando <key|C-r>.
  Durante a busca, apenas o texto no mesmo modo e na mesma linguagem vai ser
  examinado. Em outras palavras, quando voc� busca por <with|mode|math|x> no
  modo matem�tico, voc� n�o vai encontrar nenhum x comum no texto normal. �
  uma limita��o atual do programa que a cadeia de busca s� pode conter texto
  simples e n�o s�mbolos matem�ticos ou texto estruturado mais complicado.

  Uma opera��o de substitui��o � iniciada pressionando a tecla <key|C-=> ou
  <apply|menu|Edit|Replace>. Voc� tem que informar qual � a cadeia procurada
  e a cadeia pela qual esta ser� substitu�da. A cada ocorr�ncia da cadeia
  procurada, voc� deve informar se deseja substituir a cadeia (y), n�o
  substitui-la (n), ou substituir esta e todas as ocorr�ncias posteriores
  (a). Assim como na procura, substitui��o � limitada ao mesmo modo e � mesma
  l�ngua.

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
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Procurar>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Substituir>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
