<TeXmacs|1.0.7.8>

<style|tmdoc>

<\body>
  <tmdoc-title|Typing large delimiters>

  Brackets inside mathematical formulas should always match: as soon as you
  enter an opening bracket ``<math|(>'', <TeXmacs> will automatically insert
  the matching closing bracket ``<math|)>''. You may disable this feature
  using <menu|Edit|Preferences|Keyboard|Automatic brackets|Disable>.
  Attention (see also below): brackets in old documents will be automatically
  be upgraded to matching brackets.

  Sometimes, you do not want the closing bracket, or you may want to replace
  it by another closing bracket. No problem: if your cursor is just before
  the closing bracket inside <math|<around|(|a,b<value|cursor>|)>>, then
  pressing<nbsp><key|]> will turn the expression into
  <math|<around|(|a,b|]><value|cursor>>. Alternatively, deletion of a bracket
  will actually turn it into an <hlink|invisible
  bracket|../semantics/man-semantics-symbols.en.tm#nobracket>, after which
  you can replace it by an arbitrary opening or closing bracket.

  By default, the sizes of the brackets are adjusted to the expression in
  between. Small delimiters are created using the <prefix|math:small> prefix,
  and keep their sizes independently of the enclosed expression.
  Alternatively, you may use <shortcut|(alternate-toggle (focus-tree))> in
  order to toggle between large and small delimiters.

  For some delimiters, such as <math|\|>, the opening and closing delimiters
  coincide. For instance, entering a vertical bar <key|\|> will produce an
  absolute value. In order to obtain a small bar-separator <math|\|>, you
  will need to type <shortcut|\|>. The binary relation ``divides'' is entered
  using <shortcut|\<divides\>>. The big bar-separator is most conveniently
  entered using the shortcut <key|\| S-var> (or, alternatively, using
  <shortcut|(math-separator "\|" #t)>). In <TeX> and <LaTeX>, such large
  separators do not exist; they are used for producing the vertical bars in
  formulas like

  <\equation*>
    <around*|\<langle\>|<frac|a|b+c><mid|\|><frac|p|q+r><mid|\|><frac|a|b+c>|\<rangle\>>.
  </equation*>

  There may be as many middle delimiters between a left and a right delimiter
  as one wishes.

  In <TeXmacs>, large delimiters may either be ``left delimiters'', ``right
  delimiters'' or ``middle delimiters''. By default, <math|(,[,{> and
  <math|\<langle\>> are left delimiters, <math|),],}> and <math|\<rangle\>>
  are right delimiters. But there status can be changed using the
  <prefix|math:left>, <prefix|math:right> and <prefix|math:middle> key
  combinations. For instance, <key|math:left )> produces <math|)>, considered
  as a large left delimiter.

  Sometimes you may want large delimiters of a particular size, instead of
  self-adjusting ones. This can be achieved by resizing the expression in
  between the brackets using <menu|Format|Transform|Resize object>.

  <tmdoc-copyright|1998--2010|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>