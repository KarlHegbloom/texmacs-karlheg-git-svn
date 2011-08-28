<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Glossare>

  Die folgenden Makros dienen im Haupt-Text zur Einf�gung von
  Glossar-Eintr�gen:

  <\explain|<explain-macro|glossary|entry>>
    Einen Eintrag <src-arg|entry> \ in das Glossar einf�gen.
  </explain>

  <\explain|<explain-macro|glossary-dup|entry>>
    dient dazu eine zus�tzliche Seiten-Nummer f�r einen Eintrag
    <src-arg|entry> hinzuzuf�gen, der bereits eingetragen wurde.
  </explain>

  <\explain|<explain-macro|glossary-explain|entry|explanation>>
    Ein Glossar-Eintrag <src-arg|entry> mit einer Erkl�rung
    <src-arg|explanation>.
  </explain>

  <\explain|<explain-macro|glossary-line|entry>>
    Glossar-Eintrag <src-arg|entry> ohne Seitenzahl.
  </explain>

  Die folgenden Makros k�nnen redefiniert werden, um die Darstellung
  anzupassen:

  <\explain|<explain-macro|glossary-1|entry|where>>
    Makro zur Darstellung eines Glossar-Eintrags und der entsprechenden
    Seitenzahl(en) <with|color|brown|<em|where>>.
  </explain>

  <\explain|<explain-macro|glossary-2|entry|explanation|where>>
    Makro zur Darstellung eines Glossar-Eintrags, seiner Erkl�rung und der
    entsprechenden Seitenzahl(en) <with|color|brown|<em|where>>.
  </explain>

  <\explain|<explain-macro|glossary-dots>>
    Makro zur Erzeugung der Punkte zwischen Glossar-Eintrag und der
    entsprechenden Seitenzahl(en).
  </explain>

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