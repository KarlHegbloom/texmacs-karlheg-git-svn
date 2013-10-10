<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Marca��o Padr�o>

  Uma variedades de marca��es padr�o s�o definidas em <tmdtd|std-markup>.
  Todas as etiquetas de conte�do textual a seguir aceitam um argumento. A
  maioria pode ser encontrada no menu <menu|Insert|Content tag>.

  <\explain|<markup|strong>>
    Indica uma regi�o <strong|importante> do texto. Voc� pode introduzir esta
    etiqueta com <menu|Insert|Content tag|Strong>.
  </explain>

  <\explain|<markup|em>>
    Emfatiza um trecho do texto como em ``a coisa <em|verdadeira>''. Esta
    etiqueta corresponde � entrada de menu <menu|Insert|Content
    tag|Emphasize>.
  </explain>

  <\explain|<markup|dfn>>
    Para defini��es como ``um <dfn|gnu> � um bicho cabeludo''. Esta etiqueta
    corresponde a <menu|Insert|Content tag|Definition>.
  </explain>

  <\explain|<markup|samp>>
    Uma seq��ncia de caracteres literais como <samp|ae> a ligatura �. Voc�
    pode inserir esta etiqueta com <menu|Insert|Content tag|Sample>.
  </explain>

  <\explain|<markup|name>>
    O nome de alguma coisa coisa ou conceinto, como o sistema operacional
    <name|Linux>. Esta etiqueta pode ser inserida com <menu|Insert|Content
    tag|Name>.
  </explain>

  <\explain|<markup|person>>
    O nome de uma pessoa como <name|Joris>. Esta etiqueta corresponde a
    <menu|Insert|Content tag|Person>.
  </explain>

  <\explain|<markup|cite*>>
    Uma cita��o bibliogr�fica como um livro ou uma revista, por exemplo:
    <cite*|Moby Dick>, de Melville. Esta etiqueta, que encontra-se em
    <menu|Insert|Content tag|Cite>, n�o deve ser confundida com
    <markup|cite>. A �ltima tamb�m � usada para cita��es, por�m o argumento
    desta refere-se a uma entrada em um banco de dados de refer�ncias
    bibliogr�ficas.
  </explain>

  <\explain|<markup|abbr>>
    Uma abrevia��o. Por exemplo, eu trabalho no <abbr|C.N.R.S.> Um abrevia��o
    � criada com <menu|Insert|Content tag|Abbreviation> ou com o atalho de
    teclado <key|text a>.
  </explain>

  <\explain|<markup|acronym>>
    Um acr�nimo � uma abrevia��o formada com a primeira letra de cada palavra
    de uma frase, como <acronym|HTML> ou <acronym|IBM>. Em particular, as
    letras n�o separadas por pontos. Voc� pode inserir um acr�nimo com
    <menu|Insert|Content tag|Acronym>.
  </explain>

  <\explain|<markup|verbatim>>
    Texto literal como a sa�da de um programa de computador. Por exemplo: o
    programa disse: <verbatim|hello>. Voc� pode digitar texto literal com
    <menu|Insert|Content tag|Verbatim>. Esta etiqueta tamb�m pode ser usada
    como um ambiente para um trecho com v�rios par�grafos.
  </explain>

  <\explain|<markup|kbd>>
    Texto que deveser digitado no teclado. Por exemplo: por favor tecle
    <kbd|return>. Esta etiqueta corresponde � entrada do menu
    <menu|Insert|Content tag|Keyboard>.
  </explain>

  <\explain|<markup|code*>>
    C�digo fonte de um programa de computador, como em ``<code*|cout
    \<less\>\<less\> 1+1;> imprime <verbatim|2>''. Esta etiqueta � inserida
    com <menu|Insert|Content tag|Code>. Para trechos mais longos de c�digo,
    voc� deve usar o ambiente <markup|code>.
  </explain>

  <\explain|<markup|var>>
    Vari�veis em um programa de computador, como <verbatim|cp <var|src-file>
    <var|dest-file>>. Esta etiqueta corresponde � entrada de menu
    <menu|Insert|Content tag|Variable>.
  </explain>

  <\explain|<markup|math>>
    Esta � a etiqueta que ser� usada no futuro para matem�tica dentro de
    texto normal. Por exemplo: <math|sin<rsup|2> x+cos<rsup|2> x=1> � bem
    conhecida.
  </explain>

  <\explain|<markup|op>>
    Esta � a etiqueta que pode ser usada dentro de express�es matem�ticas
    para indicar que um operador deve ser considerado por si s�, sem qualquer
    argumento. Por exemplo: a opera��o <math|<op|+>> � uma fun��o de
    <math|\<bbb-R\><rsup|2>> em <math|\<bbb-R\>>. Esta etiqueta pode
    tornar-se obsoleta.
  </explain>

  <\explain|<markup|tt>>
    � uma etiqueta de marca��o f�sica que existe para compatibilidade com
    <name|HTML>, mas n�o recomendamos seu uso. use.
  </explain>

  A seguir listamos os ambientes padr�o:

  <\explain|<markup|verbatim>>
    Descrito acima.
  </explain>

  <\explain|<markup|code>>
    Similar a <markup|code*>, mas para trechos de c�digo com v�rias linhas.
  </explain>

  <\explain|<markup|quote>>
    Ambiente para cita��es curtas (um par�grafo).
  </explain>

  <\explain|<markup|quotation>>
    Ambiente para cita��es longas (v�rios par�grafos).
  </explain>

  <\explain|<markup|verse>>
    Ambiente para poesia.
  </explain>

  <\explain|<markup|center>>
    Esta � uma etiqueta de marca��o f�sica para centralizar uma ou v�rias
    linhas de texto. Existe para compatibilidade com <name|HTML>, mas n�o
    recomendamos seu uso.
  </explain>

  Alguns ambientes padr�o para texto tabular s�o

  <\explain|<markup|tabular*>>
    Tabelas centradas.
  </explain>

  <\explain|<markup|block>>
    Tabelas alinhadas � esquerda com uma moldura padr�o de <verbatim|1ln> de
    largura.
  </explain>

  <\explain|<markup|block*>>
    Tabelas centradas com uma moldura padr�o de <verbatim|1ln> de largura.
  </explain>

  As etiquetas sequintes n�o aceitam argumentos:

  <\explain|<markup|TeXmacs>>
    O logotipo do <TeXmacs>.
  </explain>

  <\explain|<markup|TeX>>
    O logotipo <TeX>.
  </explain>

  <\explain|<markup|LaTeX>>
    O logotipo do <LaTeX>.
  </explain>

  <\explain|<markup|hflush>>
    Usada por desenvolvedores para empurrar o texto para a direita na
    defini��o de ambientes.
  </explain>

  <\explain|<markup|hrule>>
    Uma linha horizontal como a que aparece abaixo:

    <hrule>
  </explain>

  Todas as etiquetas abaixo aceitam um ou mais argumentos.

  <\explain|<markup|overline>>
    Para <overline|grifado acima>, que pode se extender por v�rias linhas.
  </explain>

  <\explain|<markup|underline>>
    Para <underline|texto sublinhado>, que pode se extender por v�rias
    linhas.
  </explain>

  <\explain|<markup|fold>>
    Macro com dois argumentos. O primeiro � exibido e o segundo ignorado:
    este macro corresponde a apresenta��o ``dobrada'' de um trecho do
    documento associado com um breve t�tulo ou resumo. O segundo argumento
    pode ser mostrado com <menu|Insert|Switch|Unfold>.
  </explain>

  <\explain|<markup|switch>>
    Macro com dois argumentos <var|x> e <var|y>, onde <var|y> � um conjunto
    de representa��es poss�veis para a escolha e <var|x> a representa��o
    atual. As teclas de fun��o <key|F9>, <key|F10>, <key|F11> e <key|F12>
    podem ser usadas para alternar entre as diferentes representa��es.
  </explain>

  <\explain|<markup|phantom>>
    Fun��o com um argumento <var|x>. Esta etiqueta ocupa tanto espa�o quanto
    o argumento <var|x> ocuparia quando tipografado, por�m <var|x> n�o �
    exibido. Por exemplo, o texto ``fantasma'' como um argumento para
    <markup|phantom> fornece ``<phantom|fantasma>''.
  </explain>

  <\explain|<markup|set-header>>
    Fun��o com um argumento para mudar de forma permanente o cabe�alho.
    Perceba que algumas etiquetas no arquivo de estilo, como etiquetas de
    sess�o, podem sobrepor-se a esta defini��o manual.
  </explain>

  <\explain|<markup|set-footer>>
    Fun��o com um argumento para alterar de forma permanente o rodap�.
  </explain>

  <tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito Willmersdorf>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|portuguese>
  </collection>
</initial>