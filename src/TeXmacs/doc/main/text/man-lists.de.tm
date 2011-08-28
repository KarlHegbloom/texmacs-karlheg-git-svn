<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Auflistung, Aufz�hlung>

  Mit <menu|Insert|Itemize> starten Sie eine nicht nummerierte Aufz�hlung, eine
  Auflistung. Mit der Taste <key|Entf> beenden Sie sie. Zur Kennzeichnung der
  einzelnen Eintr�ge k�nnen Sie au�er der Voreinstellung noch
  <with|mode|math|\<bullet\>> (gro�e Punkte), <with|mode|math|<op|->>
  (Spiegelstrich) oder <with|mode|math|<op|\<rightarrow\>>> (Pfeile) w�hlen.
  Aufz�hlungen k�nnen <em|verschachtelt> werden, wie das folgende Beispiel
  zeigt: \ \ 

  <\itemize>
    <item>Erster Punkt.

    <item>Nun kommt eine weitere Aufz�hlung:

    <\itemize>
      <item>Ein Unterpunkt.

      <item>Ein weiterer Unterpunkt.
    </itemize>

    <item>Der letzte Punkt.
  </itemize>

  F�r die Beendigung innerer Listen gilt das gleiche wie bei der �u�eren;
  \ <key|Entf> f�hrt zur n�chst �u�eren zur�ck. Die voreingestellte Version
  ver�ndert, wie oben gezeigt, die Darstellung der Aufz�hlungsmarke. Die
  �u�erste Aufz�hlung benutzt <with|mode|math|\<bullet\>>, die n�chst innere
  \ <with|mode|math|<op|\<circ\>>>, usw.. Wenn Sie sich innerhalb einer
  Aufz�hlung befinden, startet die Eingabetaste <shortcut|(kbd-return)>
  automatisch einen neuen Eintrag. Wenn Sie Eintr�ge haben, die �ber mehrere
  Abs�tze gehen, k�nnen Sie \ <shortcut|(kbd-shift-return)> benutzen, um einen neuen
  Absatz im gleichen Eintrag zu beginnen.

  Nummerierte Aufz�hlungen, die mit <menu|Insert|Enumerate> gestartet werden,
  verhalten sich entsprechend, nur dass die Eintr�ge fortlaufend nummeriert
  werden. Es folgt ein Beispiel, dass mit <menu|Insert|Enumerate|Roman>
  gestartet wurde:

  <\enumerate-Roman>
    <item>Ein erster Punkt.

    <item>Ein zweiter Punkt.

    <item>Und schlie�lich der letzte Punkt.
  </enumerate-Roman>

  Der letzte Typ sind beschreibende Auflistungen, bei denen Sie einen
  beschreibenden Text selbst eingeben k�nnen. Sie starten diese mit einem
  Befehl aus dem Men� <menu|Insert|Description> z.B.
  <menu|Insert|Description|Compact>, wie im Beispiel unten gezeigt. Sie k�nnen
  dann den beschreibenden Text eingeben und mit linken Pfeiltaste
  \ <key|links> bzw. der Pfeiltaste unten <key|unten> zur Eingabe des
  Eintrags wechseln:

  <\description-compact>
    <item*|Gnu>Ein haariges aber friedliches Tier

    <item*|M�cke>Weibliche stechen
  </description-compact>

  Mit <menu|Insert|Description|Long> erhalten Sie:

  <\description-long>
    <item*|Gnu>Ein haariges aber friedliches Tier

    <item*|M�cke>Weibliche stechen
  </description-long>

  \;

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
  </collection>
</initial>