<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Arquivos de estilo do <TeXmacs>>

  Uma das melhores caracter�sticas do <TeXmacs> � a possibilidade de escrever
  seus pr�prios arquivos de estilo. Arquivos de estilo tem multiplas fun��es:

  <\itemize>
    <item> Permitem a abstra��o de elementos repetitivos em textos como
    se��es, teoremas, enumera��es, etc.

    <item>Formam um mecanismo que permite que voc� estruture o seu texto. Por
    exemplo, voc� pode indicar que um trecho do seu texto � uma abrevia��o,
    uma cita��o ou ``importante''.

    <item>Alguns estilo padr�o permitem que voc� escreva documentos com
    apar�ncia profissional, porque foram projetados com muito cuidado por
    pessoas com muito conhecimento muito sobre tipografia e est�tica.
  </itemize>

  � poss�vel associar um ou mais estilos a um documento. O estilo principal
  do documento � escolhido no menu <apply|menu|Document|Style>, e estilos
  adicionais podem ser adicionado a partir de <apply|menu|Document|Use
  package>.

  Do ponto de vista do programa, cada estilo corresponde a um arquivo
  <verbatim|.ts>. Os arquivos correspondentes a cada estilo s�o processados
  como se fossem documentos comuns, porem o editor mant�m apenas o ambiente
  final de cada arquivo de estilo como o ambiente inicial do documento. Os
  arquivos de estilo s�o processados na ordem em que foram listados, assim
  como os arquivos de estilo usados dentro destes, recursivamente.

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

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Usar pacote>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
