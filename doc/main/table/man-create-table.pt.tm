<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Criando Tabelas>

  Para criar uma tabela, voc� pode usar tanto <apply|menu|Insert|Table> ou um
  dos atalhos abaixo:

  <\description>
    <expand|item*|<expand|kbd-table|N t>>Criar uma tabela regular.

    <expand|item*|<expand|kbd-table|N T>>Criar uma tabela regular com c�lulas
    centradas.

    <expand|item*|<expand|kbd-table|N b>>Criar um ``bloco'', cujas c�lulas
    s�o separadas por linhas.

    <expand|item*|<expand|kbd-table|N B>>Cria um bloco cujas c�lulas s�o
    centradas.
  </description>

  No modo matem�tico, algumas outras estruturas similares a tabelas est�o
  dispon�veis:

  <\description>
    <expand|item*|<expand|kbd-table|N m>>Criar uma matriz.

    <expand|item*|<expand|kbd-table|N d>>Criar um determinante.

    <expand|item*|<expand|kbd-table|N c>>Criar uma lista de op��es.
  </description>

  O ambiente <verbatim|\\eqnarray*> � tamb�m um tipo especial de estrutura
  semelhante a uma tabela, que se estende por toda uma linha. Voc� pode
  iniciar uma lista de equa��es usando <apply|menu|Insert|Mathematics|Equations>.

  Em uma tabela rec�m criada, seu tamanho � o m�nimo (em geral
  <with|mode|math|1\<times\>1>) e suas c�lulas s�o vazias. Novas linhas e
  colunas podem ser inseridas com as combina��es <key|A-<expand|key-left>>,
  <key|A-<expand|key-right>>, <key|A-<expand|key-up>> e
  <key|A-<expand|key-down>>. Por exemplo, <key|A-<expand|key-right>> \ cria
  uma nova coluna � direita da posi��o corrente do cursor. Voc� tamb�m pode
  come�ar uma nova linha ap�s a posi��o corrente do cursor digitando
  <key|<expand|key-return>>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Ramiro Brito
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
      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Tabela>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Inserir>|<with|font
      family|<quote|ss>|Matem�ticos>|<with|font
      family|<quote|ss>|Equa��es>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
