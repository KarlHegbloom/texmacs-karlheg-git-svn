<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Estilos e pacotes padr�o do <TeXmacs>>

  Est�o implementados atualmente os seguintes estilos:

  <\itemize>
    <item>Book (livro);

    <item>Article (artigo);

    <item>Letter (carta);

    <item>Seminar (semin�rio, para transpar�ncias).
  </itemize>

  Cada um destes estilos exporta um certo n�mero de fun��es e ambientes
  padronizados listados abaixo. Todos os formatos padr�o do futuro dever�o
  suprir pelo menos os comandos e ambiente abaixo, e n�s sugerimos que
  usu�rios que implementem seus pr�prios arquivos de estilo fa�am o mesmo.

  <\itemize>
    <item>Divis�o to texto em se��es.

    <item>Ambientes para listas numeradas e n�o numeradas.

    <item>Ambientes semelhantes a equa��es.

    <item>Ambientes semelhantes a teoremas.

    <item>Ambientes de programa��o
  </itemize|>

  Chamamos aten��o para o fato de que ambientes para teoremas n�o s�o
  padronizados no <apply|LaTeX>, e isto � uma grande fonte de
  incompatibilidades. Novos ``teoremas'' podem ser adicionados com o comando
  <verbatim|newtheorem>. Tamb�m � poss�vel introduzir novos ``coment�rios''
  com o comando <verbatim|newremark>; ``coment�rios'' s�o distintos de
  teoremas no sentido em que n�o s�o tipografados com uma fonte enfatizada.

  Claramente, ambientes de programa��o tamb�m n�o existem no <apply|LaTeX>.
  Tais ambientes encontram-se em desenvolvimento.

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
