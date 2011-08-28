<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Listenkontext benutzen >

  Die Standard-<TeXmacs>-Listen werden in <tmdtd|std-list> definiert.\ 

  Es gibt die folgenden unnummerierten Listen:

  <\explain|<explain-macro|itemize|body>>
    Die Marke vor jedem Punkt h�ngt von der Tiefe der Gliederung ab
    (<LaTeX>-Stil).\ 
  </explain>

  <\explain|<explain-macro|itemize-minus|body>>
    Benutzt <with|mode|math|-> .
  </explain>

  <\explain|<explain-macro|itemize-dot|body>>
    Benutzt <with|mode|math|\<bullet\>> .
  </explain>

  <\explain|<explain-macro|itemize-arrow|body>>
    Benutzt <with|mode|math|\<rightarrow\>> .
  </explain>

  Nummerierte Listen:

  <\explain|<explain-macro|enumerate|body>>
    Die Art der Nummerierung h�ngt von der Tiefe der Gliederung ab
    (<LaTeX>-Stil).
  </explain>

  <\explain|<explain-macro|enumerate-numeric|body>>
    Nummeriere so: 1, 2, 3 <abbr|<localize|etc>.>
  </explain>

  <\explain|<explain-macro|enumerate-roman|body>>
    Nummeriere so: \ i, ii, iii <abbr|<localize|etc>.>
  </explain>

  <\explain|<explain-macro|enumerate-Roman|body>>
    Nummeriere so: I, II, III <abbr|<localize|etc>.>\ 
  </explain>

  <\explain|<explain-macro|enumerate-alpha|body>>
    \RNummeriere'' so: a), b), c) <abbr|<localize|etc>.>\ 
  </explain>

  <\explain|<explain-macro|enumerate-Alpha|body>>
    \RNummeriere'' so: A), B), C) <abbr|<localize|etc>.>\ 
  </explain>

  Beschreibende Auflistungen:

  <\explain|<explain-macro|description|body>>
    Das Konstrukt f�r die Vorgabe beschreibender Aufz�hlungen (meist
    <markup|description-compact>).
  </explain>

  <\explain|<explain-macro|description-compact|body>>
    Stelle die Punkte der Liste linksb�ndig dar, dann das Trennzeichen
    <strong|.> und positioniere den Text anschlie�end einem kurzem Abstand.
  </explain>

  <\explain|<explain-macro|description-dash|body>>
    �hnlich <markup|description-compact>, \ mit Trennzeichen \V .
  </explain>

  <\explain|<explain-macro|description-align|body>>
    Das Trennzeichen <strong|.> wird an einer festen Stelle gesetzt. Rechts
    davon der Text. Links vom Trennzeichen wird rechtsb�ndig zum Trennzeichen
    die Beschreibung gesetzt.
  </explain>

  <\explain|<explain-macro|description-long|body>>
    Setzt die Beschreibung und den eigentlichen Text auf zwei verschiedene
    Zeilen.
  </explain>

  Neue Punkte in einer Liste werden mit <markup|item> oder im Fall von
  beschreibenden Auflistungen mit <markup|item*> eingef�gt. <markup|item> hat
  keine Argumente, der Text folgt anschlie�end an den Befehl, <markup|item*>
  wie <markup|item>, hat ein Argument die Beschreibung. Wenn das
  experimentelle <tmdtd|structured-list> Paket benutzt wird k�nnen diese
  Befehle ein weiteres optionales Rumpfargument erhalten. In Zukunft sollen
  alle \Ritem``-Tags so sein.

  Als Vorgabe werden alle Unterlisten so nummeriert wie in den normalen
  Listen. Jeder Listen-Kontext <markup|<em|list>> besitzt aber die Variante
  <markup|<em|list>*>, deren Punkte als Pr�fix den zugeh�rigen Punkt in der
  Hauptliste erhalten.

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
  </collection>
</initial>