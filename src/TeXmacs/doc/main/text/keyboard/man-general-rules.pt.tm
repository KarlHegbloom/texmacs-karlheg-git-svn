<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Regras gerais para prefixos>

  J� que h� tantos atalhos de teclado, � importante ter alguma maneira de
  classific�-los em v�rias categorias, para que sua memoriza��o seja mais
  f�cil. Como uma regra geral, atalhos do teclado que s�o da mesma categoria
  tem o mesmo prefixo. Os principais prefixos s�o:

  <\description>
    <expand|item*|<prefix|C->>Atalhos prefixados pela tecla
    control s�o usados freq�entemente para comandos de edi��o. Eles dependem
    particularmente do ``look and feel'' selecionado em
    <apply|menu|Edit|Preferences>. Por exemplo, se voc� usar o look and feel
    compat�vel com o <name|Emacs>, atalhos da forma
    \ <prefix|C-> correspondem a comandos do <name|Emacs>,
    como <key|C-y> para colar texto.

    <expand|item*|<prefix|A->>A tecla alt � usada para
    comandos que dependem do modo no qual voc� est�. Por exemplo,
    <key|text s> produz texto <strong|real�ado> no modo texto e uma
    raiz quadrada <with|mode|math|<sqrt|>> no modo matem�tico. Note que
    \ <key|escape escape> equivale a \ <prefix|A->.

    <expand|item*|<prefix|M->>A tecla meta � usada para
    comandos gerais do <apply|TeXmacs>, que podem ser usados em qualquer
    modo. Por exemplo, <shortcut|(make-label)> produz um r�tulo. Ela � usada
    tamb�m para outros comandos de edi��o, como <key|A-w> para copiar texto
    se voc� usa o look and feel do <name|Emacs>. Note que
    <key|escape> equivale a <prefix|M->.

    <expand|item*|<prefix|M-A->>A tecla modificadora do usu�rio
    � usada para inserir s�mbolos especiais como letras gregas no modo
    matem�tico. Voc� pode configurar seu teclado para que a tecla <key|capslock>
    funcione como a tecla hiper. A tecla <prefix|math:greek> equivale a <prefix|M-A->.
  </description>

  Lembramos que as teclas modificadoras usadas para a obten��o dos prefixos
  <prefix|M-> e <prefix|M-A-> podem ser <apply|hyper-link|configuradas|../../config/man-config-kbd-modkeys.pt.tm>
  em <apply|menu|Edit|Preferences>.

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
    <associate|preamble|false>
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
      family|<quote|ss>|Prefer�ncias>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Editar>|<with|font
      family|<quote|ss>|Prefer�ncias>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
