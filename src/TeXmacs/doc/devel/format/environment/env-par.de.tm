<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Absatz-Layout>

  <\explain>
    <var-val|par-mode|justify><explain-synopsis|Absatzausrichtung>
  <|explain>
    Diese Kontextvariable legt fest wie Zeilen ausgerichtet werden, also wie
    sie in Bezug auf die Absatzr�nder gesetzt werden. Es gibt vier m�gliche
    Werte: <verbatim|left>, <verbatim|center>, <verbatim|right> und
    <verbatim|justify>, die den Ausrichtungen linksb�ndig, zentriert,
    rechtsb�ndig und Blocksatz entsprechen:

    \;

    <\big-table>
      <\quote-env>
        <\quote-env>
          <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
            <\with|par-mode|left>
              Dieser Absatz ist linksb�ndig ausgerichtet. Dieser Absatz ist
              linksb�ndig ausgerichtet. Dieser Absatz ist linksb�ndig
              ausgerichtet. Dieser Absatz ist linksb�ndig ausgerichtet.Dieser
              Absatz ist linksb�ndig ausgerichtet.
            </with>
          </cell>|<\cell>
            <\with|par-mode|center>
              Dieser Absatz ist zentriert ausgerichtet. Dieser Absatz ist
              zentriert ausgerichtet. Dieser Absatz ist zentriert
              ausgerichtet. Dieser Absatz ist zentriert ausgerichtet.\ 
            </with>
          </cell>>|<row|<\cell>
            <\with|par-mode|right>
              Dieser Absatz ist rechtsb�ndig ausgerichtet. Dieser Absatz ist
              rechtsb�ndig ausgerichtet. Dieser Absatz ist rechtsb�ndig
              ausgerichtet. Dieser Absatz ist rechtsb�ndig ausgerichtet.
            </with>
          </cell>|<\cell>
            Dieser Absatz ist im Blocksatz gesetzt. Dieser Absatz ist im
            Blocksatz gesetzt. Dieser Absatz ist im Blocksatz gesetzt. Dieser
            Absatz ist im Blocksatz gesetzt. Blocksatz ist die Vorgabe.
          </cell>>>>>
        </quote-env>
      </quote-env>
    <|big-table>
      Unterst�tzte Absatzausrichtungen.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-hyphen|normal><explain-synopsis|Qualit�t der Trennungen>
  <|explain>
    Dieser Parameter steuert die Qualit�t der Trennungs-Algorithmen. M�gliche
    Werte sind \ <verbatim|normal> und <verbatim|professional>. Der
    professionelle Trennungs-Algorithmus verwendet einen Algorithmus der den
    ganzen Absatz umfasst. Der normale ist schneller.

    <\big-table>
      <\with|font-base-size|10>
        <\quote-env>
          <\quote-env>
            <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
              <\with|par-hyphen|normal>
                Die Unterschiede zwischen den verschiedenen m�glichen
                Trennungs-Algorithmen sieht man in der Regel erst bei
                l�ngeren Abs�tzen und zwar dann, wenn die Abs�tze in schmale
                Kolumnen gesetzt werden.\ 
              </with>
            </cell>|<\cell>
              <\with|par-hyphen|professional>
                Die Unterschiede zwischen den verschiedenen m�glichen
                Trennungs-Algorithmen sieht man in der Regel erst bei
                l�ngeren Abs�tzen und vor allem dann, wenn die Abs�tze in
                schmale Kolumnen gesetzt werden. \ 
              </with>
            </cell>>>>>
          </quote-env>
        </quote-env>
      </with>
    <|big-table>
      Vergleich zwischen verschiedenen Trennungs-Algorithmen. Links der
      normale Algorithmus, rechts der professionelle. Auch wenn auf der
      rechten Seite noch einige unsch�ne L�cken verbleiben, so wurden von dem
      professionellen Algorithmus doch die unsch�nen L�cken in der vorletzten
      Zeile um das Wort \RAbs�tze`` vermieden.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-width|auto><explain-synopsis|Absatzbreite>
  <|explain>
    Diese Kontextvariable steuert die Breite der Abs�tze. Normalerweise wird
    die Absatzbreite automatisch aus der Papierbreite und den Breiten der
    R�nder berechnet.
  </explain>

  <\explain>
    <var-val|par-left|0cm>

    <var-val|par-right|0cm><explain-synopsis|Linker und rechter Rand>
  <|explain>
    Diese Kontextvariablen bestimmen die Breite des linken bzw. rechten
    Randes von Abs�tzen und zwar in Bezug auf die Vorgabewerte, die von dem
    Seitenlayout bestimmt werden.\ 

    Ein Beispiel:

    <\tm-fragment>
      Dieser Text benutzt die Vorgabe.

      <\with|par-left|1cm>
        Dieser Text hat einen linken Rand von <verbatim|1cm.>
      </with>

      <\with|par-left|2cm>
        Dieser Text hat einen linken Rand von <verbatim|2cm.>
      </with>

      <\with|par-left|3cm>
        Dieser Text hat einen linken Rand von <verbatim|3cm.>
      </with>

      <\with|par-left|3cm|par-right|3cm>
        Die linken und die rechten R�nder dieses Textes sind beide auf
        <verbatim|3cm> gesetzt worden.
      </with>
    </tm-fragment>

    Layoutelemente wie Auflistungen, <markup|itemize>, oder
    <markup|quote-env>, die verschachtelt werden k�nnen, berechnen in der
    Regel neue Randabst�nde als Funktion der alten, indem sie vorgegebene
    Abst�nde hinzuf�gen oder abziehen. Das verdeutlicht die typischen
    Makrodefinition f�r <markup|quote-env> des folgenden Beispiels:

    <\tm-fragment>
      <inactive*|<assign|quote-env|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.5fn>|<right-flush><vspace|0.5fn>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|par-first|0fn|par-par-sep|0.25fn|<arg|body>>>>>>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-first|1.5fn><explain-synopsis|Erstzeileneinzug>
  <|explain>
    Der <src-var|par-first>-Parameter spezifiziert den Erstzeileneinzug. Ein
    solcher Einzug hat den Sinn, den Beginn eines neuen Absatzes zu
    kennzeichnen. Eine andere Alternative ist ein erh�hter Zeilenabstand.

    <\big-table>
      <\with|par-hyphen|professional>
        <\quote-env>
          <\quote-env>
            <block|<tformat|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-hyphen|t>|<table|<row|<\cell>
              <\with|par-par-sep|0fn>
                <\with|par-first|1.5fn>
                  In den <TeXmacs>-Basis-Stilen
                  <active*|<tmstyle|<translate|article|english|german>>> und
                  <tmstyle|<translate|book|english|german>> wird der Anfang
                  eines neuen Absatzes durch den Einzug der ersten Zeile
                  gekennzeichnet.

                  Der <TeXmacs>-<tmstyle|<translate|generic|english|german>><tmstyle|>-
                  und der <tmstyle|<translate|letter|english|german>>-Basis-Stil
                  benutzen dagegen senkrechten Abstand.
                </with>
              </with>
            </cell>|<\cell>
              Der <TeXmacs>-<tmstyle|<translate|generic|english|german>><tmstyle|>-
              und der <tmstyle|<translate|letter|english|german>>-Basis-Stil
              benutzen einen zus�tzlichen senkrechten Abstand zur
              Kennzeichnung des Beginns von neuen Abs�tzen.

              <\with|par-par-sep|0.5fn>
                <\with|par-first|0fn>
                  In den <TeXmacs>-Basis-Stilen
                  <active*|<tmstyle|<translate|article|english|german>>> und
                  <tmstyle|<translate|book|english|german>> wird der Anfang
                  eines neuen Absatzes dagegen durch den Einzug der ersten
                  Zeile gekennzeichnet..
                </with>
              </with>
            </cell>>>>>
          </quote-env>
        </quote-env>
      </with>
    <|big-table>
      <label|par-first-tab>Zwei klassische Weisen zur Kennzeichnung eines
      neuen Absatzes.
    </big-table>
  </explain>

  <\explain>
    <var-val|par-par-sep|0.5fn*><explain-synopsis|Zus�tzlicher Abstand
    zwischen Abs�tzen>
  <|explain>
    Der <src-var|par-par-sep>-Parameter legt den Abstand zwischen zwei auf
    einander folgenden Abs�tzen fest. Der Abstand wird in
    \ <hyper-link|kontextabh�ngigen L�ngeneinheiten|../basics/lengths.de.tm>
    gemessen. In der Regel erzeugt <TeXmacs> keinen vergr��erten
    Zeilenabstand zwischen auf einander folgenden Abs�tzen, es sei denn, es
    w�rde kein vern�nftiger Seitenumbruch gefunden. Darum wird die
    kontextabh�ngige L�ngeneinheiten <verbatim|fn*> benutzt. Normalerweise
    wird der Erstzeileneinzug benutzt (Tabelle <reference|par-first-tab>).
  </explain>

  <\explain>
    <var-val|par-line-sep|0.025fn*><explain-synopsis|Leerraum zwischen
    Zeilen>
  <|explain>
    Dieser Parameter legt die <em|Breite des Leerraums> zwischen den Zeilen
    in einem Absatz fest. Dies entspricht nicht dem, was man normalerweise
    mit Zeilenabstand bezeichnet. Die �bliche Definition des Zeilenabstands
    ist die Summe des Leerraums und der Schrifth�he.

    <\tm-fragment>
      <\with|par-line-sep|1fn>
        Ein doppelter \RZeilenabstand'' entspricht
        \ <var-val|par-line-sep|1fn>. Dieser wird oft von faulen Menschen
        verwendet, die vorgeben wollen, viele Seiten geschrieben zu haben,
        die sich aber um das Wohlergehen der W�lder nicht k�mmern..
      </with>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|par-sep|0.2fn><explain-synopsis|Minimaler vertikaler Abstand
    zwischen Zeilen>
  <|explain>
    Diese Variable definiert einen Mindestabstand zwischen Boxen in den
    einzelnen Zeilen. Das verhindert Kollisionen von besonders gro�en K�sten
    mit solchen in vorausgehenden bzw. den nachfolgenden Zeilen.
  </explain>

  <\explain>
    <var-val|par-hor-sep|0.5fn><explain-synopsis|Minimaler horizontaler
    Abstand>
  <|explain>
    Wenn ein Absatz mehrere besonders gro�e Boxen enth�lt, dann versucht
    <TeXmacs> die auf einander folgenden Zeilen in einander zu schieben,
    solange Boxen nicht mit einander kollidieren:

    <\with|font-base-size|10>
      <\tm-fragment>
        Betrachten Sie einen Bruch, der sich tiefer als die Unterl�ngen der
        normalen Schrift erstreckt wie beispielsweise <with|mode|math|den
        Bruch <frac|1|x+1>> und einen Ausdruck, der h�her als normal ist wie
        <with|mode|math|\<mathe\><rsup|\<mathe\><rsup|x>>>. Wie Sie sehen
        versucht <TeXmacs> eine kompakte Darstellung zu erreichen.

        Wenn der Bruch <with|mode|math|<frac|1|x+1>> und der besonders hohe
        Ausdruck aber an der falschen Stelle liegen, wie
        <with|mode|math|\<mathe\><rsup|\<mathe\><rsup|x>>> hier, dann bleiben
        die Boxen im Abstand <src-var|par-sep>.
      </tm-fragment>
    </with>

    Wenn der horizontale Abstand zwischen zwei gro�en Boxen kleiner ist als
    <src-var|par-hor-sep> dann wird das als Kollision betrachtet.
  </explain>

  <\explain>
    <var-val|par-fnote-sep|0.2fn><explain-synopsis|Minimaler Abstand zwischen
    Fu�noten>
  <|explain>
    Dieser Parameter definiert den Abstand zwischen auf einander folgenden
    Fu�noten.
  </explain>

  <\explain>
    <var-val|par-columns|1><explain-synopsis|Spaltenanzahl>
  <|explain>
    Diese Variable legt die Anzahl der Spalten fest. Innerhalb eines
    Dokuments k�nnen unterschiedliche Spaltenanzahlen verwendet werden.
  </explain>

  <\explain>
    <var-val|par-columns-sep|2fn><explain-synopsis|Spaltenabstand>
  <|explain>
    Die Variable definiert die horizontale Breite des Leerraums zwischen
    Spalten.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
    <associate|preamble|false>
    <associate|sfactor|4>
  </collection>
</initial>