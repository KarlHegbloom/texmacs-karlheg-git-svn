<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Das Dokumenten-Format (0.3.4)>

  Das Format der TeXmacs-Dokumente wurde tief greifend ge�ndert, um TeXmacs
  kompatibler mit XML zu machen. Vor allem wurden Kontexte wie

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>environment\|open\|close\<gtr\>\<gtr\>,
  </verbatim>

  die aus zu einander geh�renden Start- und Stopp-Tag-Paaren wie z.B.
  <verbatim|\<less\>begin\|env\<gtr\>text\<less\>end\|env\<gtr\>> geh�ren,
  durch Makros der Form

  <\verbatim>
    \ \ \ \ \<less\>assign\|env\|\<less\>macro\|body\|open\<less\>body\<gtr\>close\<gtr\>\<gtr\>
  </verbatim>

  ersetzt, die einfach mit <verbatim|\<less\>expand\|env\|text\<gtr\>>
  expandiert werden. Ganz entsprechend wurden Paare von zusammengeh�rigen
  Kontext-Variablen-Definitionen der Form
  <verbatim|\<less\>set\|var\|val\<gtr\>text\<less\>reset\|var\<gtr\>> durch
  ein <verbatim|\<less\>with\|var\|val\|text\<gtr\>>-Konstrukt ersetzt. Diese
  �nderungen haben brachten allerdings einige Komplikationen, besonders wenn
  ein Text sich �ber mehrere Abs�tze erstreckt. Deshalb kann es passieren,
  dass ung�nstig formatierte �ltere Dokumente in der neuen Version etwas
  anders dargestellt werden k�nnen. Diese Komplikationen f�hrten auch dazu,
  dass das Verhalten des Editors in Bezug auf Dokumente mit mehreren Abs�tzen
  ein wenig ver�ndert werden musste.

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