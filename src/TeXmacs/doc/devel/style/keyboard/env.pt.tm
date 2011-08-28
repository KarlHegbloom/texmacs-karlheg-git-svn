<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, fun��es e vari�veis do ambiente>

  As principais combina��es de teclas que voc� deve conhecer para escrever
  arquivos de estilo s�o as seguintes:

  <\description>
    <expand|item*|<key|M-=>>cria uma nova atribui��o. O primeiro
    argumento � o nome do novo comando e o segundo uma express�o.

    <expand|item*|<key|M-w>>permite mudar localmente uma ou mais
    vari�veis do ambiente. Comandos `with' s�o da forma 
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<r\
    sub|n>\|b\<rangle\>>, onde os <with|mode|math|x<rsub|i>> s�o os nomes das
    vari�veis, os <with|mode|math|a<rsub|i>> seus valores locais, e
    <with|mode|math|b> o texto ao qual aplica-se a vari�vel local.

    <expand|item*|<key|M-m>>cria um macro. Argumentos para o macro 
    podem ser inseridos com a tecla <key|tab>.  

    <expand|item*|<key|M-f>>cria uma fun��o. Argumentos para a fun��o
    podem ser inseridas com a tecla  <key|tab>.

    <expand|item*|<key|inactive #>>obt�m o valor de um argumento de um macro.

    <expand|item*|<key|inactive v>>obt�m o valor de uma vari�vel do ambiente.

    <expand|item*|<key|inactive e>>expande o macro com zero ou mais
    argumentos.

    <expand|item*|<key|inactive a>>aplica a fun��o a zero ou mais
    argumentos.
  </description>

  Mais precisamente, durante a expans�o de um macro
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> criado por
  <key|inactive e>, acontecem a seguinte seq��ncia:

  <\itemize>
    <item>Se <with|mode|math|a> n�o � uma cadeia de caracteres ou um macro,
    ent�o <with|mode|math|a> � avaliado uma vez. Disto resulta ou um nome de
    um macro ou uma macro express�o <with|mode|math|f>.

    <item>Se foi obtido um nome de um macro, ent�o <with|mode|math|f> � 
    substituido pelo valor da vari�vel do ambiente <with|mode|math|f>.
    Se, ap�s isto, <with|mode|math|f> ainda n�o � uma macro express�o,
    ent�o retornamos <with|mode|math|f>.

    <item>Sejam <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> os argumentos
    de <with|mode|math|f> e <with|mode|math|b> seu corpo (argumentos
    desnecess�rios s�o descartados; uma cadeia de caracteres vazia � tomada
    como o valor padr�o dos argumentos n�o fornecidos). Ent�o cada
    <with|mode|math|x<rsub|i>> � substituido para cada 
    <with|mode|math|y<rsub|i>> em <with|mode|math|b> e este valor � retornado.

  </itemize>

  Fun��es s�o similares a macros, com a exce��o de que argumentos de uma
  aplica��o de fun��o s�o avaliados e n�o podem ser editados diretamente
  (primeiro voc� precisa desativar a aplica��o da fun��o, editar os
  argumentos, e reativ�-la). Al�m disso, 
  <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> s�o agora vari�veis do
  ambiente local, �s quais s�o dados os valores de 
  <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>>. Estas vari�veis locais
  n�o s�o lembradas quando uma fun��o retorna uma fun��o que envolve estas
  vari�veis.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven &
  Ramiro Brito Willmersdorf>

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
    <associate|language|english>
  </collection>
</initial>
