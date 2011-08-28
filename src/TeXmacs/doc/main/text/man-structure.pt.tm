<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Escrevendo texto estruturado>

  Normalmente, documentos longos s�o estruturados: s�o organizados em
  cap�tulos, se��es e subse��es, cont�m diferentes tipos de textos, como
  texto normal, cita��es, notas de rodap�, teoremas, etc. Depois que voc�
  escolhe um <em|estilo de documento> em <apply|menu|Document|Style>, o
  pr�prio <apply|TeXmacs> cuida da diagrama��o, gerando automaticamente a
  numera��o de se��es, p�ginas e teoremas, bem como uma diagrama��o atraente
  para as notas de rodap� e cita��es e assim por diante.

  Quatro estilos de documentos padr�o foram implementados at� agora: carta,
  artigo, livro e semin�rio. O estilo semin�rio � usado para cria��o de
  transpar�ncias. Assim que voc� tenha escolhido um estilo, voc� pode
  organizar seu texto em se��es (ver <apply|menu|Insert|Section>), e usar
  <em|ambientes> espec�ficos. Exemplos de ambientes s�o teoremas,
  proposi��es, observa��es e similares (ver <apply|menu|Insert|Environment>).
  Outros exemplos s�o listas de �tens (ver <apply|menu|Insert|Itemize>) ou
  listas numeradas (ver <apply|menu|Insert|Enumerate>).

  Quando voc� se acostumar mais com o <apply|TeXmacs>, ser� poss�vel
  adicionar seus pr�prios ambientes nos seu pr�prios arquivos de estilo.
  Suponha, por exemplo, que voc� freq�entemente faz cita��es e que voc�
  deseja que estas apare�am em it�lico, com margens de 1cm � esquerda e �
  direita. Ao inv�s de mudar manualmente as propriedades do texto e do
  par�grafo todas as vezes em que voc� faz uma cita��o, � melhor criar um
  ambiente. N�o s� a cria��o de uma cita��o ser� mais r�pida, mas tamb�m
  torna-se poss�vel mudar sistematicamente a diagrama��o de suas cita��es ao
  longo de todo o documento, simplesmente pela mudan�a da defini��o deste
  ambiente. Esta situa��o pode ocorrer, por exemplo, se voc� descobre, <em|a
  posteriori,> que voc� prefere que as cita��es apare�am em uma fonte menor.

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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Se��o>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Ambiente>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Listar>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Texto>|<with|font
      family|<quote|ss>|Enumerar>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
