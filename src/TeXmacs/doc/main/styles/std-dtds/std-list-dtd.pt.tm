<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Listas padr�o>

  As listas padr�o do <TeXmacs> s�o definidas em <tmdtd|std-list>. As listas
  sem numera��o s�o:

  <\description>
    <expand|item*|<markup|itemize>>A marca antes de cada �tem depende do
    n�vel do aninhamento.

    <expand|item*|<markup|itemize-minus>>Usa <with|mode|math|-> para a marca.

    <expand|item*|<markup|itemize-dot>>Usa <with|mode|math|\<bullet\>> para a
    marca.

    <expand|item*|<markup|itemize-arrow>>Usa <with|mode|math|\<rightarrow\>>
    para a marca.
  </description>

  Listas numeradas correspondem aos seguintes ambientes:

  <\description>
    <expand|item*|<markup|enumerate>>O tipo do n�mero depende do n�vel de
    aninhamento.

    <expand|item*|<markup|enumerate-numeric>>Numera os itens com 1, 2, 3,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-roman>>Numera os itens com i, ii, iii,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Roman>>Numera os itens com I, II, III,
    <abbr|etc.>

    <expand|item*|<markup|enumerate-alpha>>Numera os itens com a), b), c),
    <abbr|etc.>

    <expand|item*|<markup|enumerate-Alpha>>Numera os itens com A, B, C,
    <abbr|etc.>
  </description>

  Os ambientes abaixo s�o usados para listas descritivas.

  <\description>
    <expand|item*|<markup|description>>O ambiente padr�o para listas
    descritivas (normalmente <markup|description-compact>).

    <expand|item*|<markup|description-compact>> Alinha os itens da lista �
    esquerda e coloca as descri��es imediatamente depois deles.

    <expand|item*|<markup|description-dash>>Parecido com
    <markup|description-compact>, mas usa um  para separar cada item de sua
    descri��o.

    <expand|item*|<markup|description-align>>Alinha as descri��es � esquerda,
    enquanto que os itens s�o alinhados � direita.

    <expand|item*|<markup|description-long>>Coloca os itens e suas descri��es
    em linhas diferentes.
  </description>

  Novos itens em uma lista s�o indicados atrav�s da etiqueta <markup|item> ou
  da etiqueta un�ria <markup|item*>, no caso de descri��es. Desenvolvedores
  podem encontrar ainda outras macros em <tmdtd|std-list> para definir outras
  estruturas de listas, por�m estes macros n�o s�o considerados est�veis.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-21|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-minus>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-dot>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|itemize-arrow>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-numeric>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-roman>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Roman>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-alpha>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|enumerate-Alpha>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-dash>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-compact>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-align>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|description-long>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|item*>>|<pageref|idx-20>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|std-list>>|<pageref|idx-21>>
    </associate>
  </collection>
</auxiliary>
