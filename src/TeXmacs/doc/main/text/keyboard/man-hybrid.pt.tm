<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Comandos h�bridos e emula��o do <LaTeX>>

  <apply|TeXmacs> permite que voc� entre com comandos do <apply|LaTeX>
  diretamente do teclado. Primeiro voc� deve digitar a tecla <key|\\> para
  entrar no modo de comando h�brido <apply|LaTeX>/<apply|TeXmacs>. Em
  seguida, voc� deve digitar o comando que voc� quer executar. Assim que voc�
  tenha terminado de digitar o comando, o rodap� esquerdo mostrar� algo como:

  <\verbatim>
    \ \ \ \ \<less\>return\<gtr\>: action to be undertaken
  </verbatim>

  Quando voc� digitar <key|<expand|key-return>>, seu comando ser� executado.
  Por exemplo, no modo matem�tico voc� pode criar uma fra��o digitando
  \ <key|\\ f r a c <expand|key-return>>.

  Se o comando que voc� digitou n�o � um comando (reconhecido) do
  <apply|LaTeX>, ent�o primeiramente verificamos se o comando � um macro,
  fun��o ou ambiente (fornecido pelo arquivo de estilo) do <apply|TeXmacs>.
  Caso isto aconte�a, a expans�o do macro, a aplica��o da fun��o ou a cria��o
  do ambiente � feita (com o n�mero correto de argumentos). Caso contr�rio,
  supomos que seu comando corresponde a uma vari�vel do ambiente e usamos o
  seu valor. A tecla \ <key|\\> � sempre equivalente a um dos comandos
  <expand|kbd-ia|l>, <expand|kbd-ia|e>, <expand|kbd-ia|a>, <expand|kbd-ia|#>
  ou <expand|kbd-ia|v>.

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
