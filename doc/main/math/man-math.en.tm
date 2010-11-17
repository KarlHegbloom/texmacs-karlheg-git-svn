<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Mathematical formulas>

  To type mathematical formulas, you need first to enter ``math mode'' using
  one of the following methods:

  <\description>
    <item*|<menu|Insert|Mathematics|Formula> or <key|$>>

    This entry corresponds to small <em|inline formulas> like
    <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> inside a textual paragraph. Note
    that formulas are typeset specially so they do not take too much vertical
    space. For example, limits are always displayed on the left. Limits can
    be displayed below in formulas with <menu|Format|Formula style|on>. In
    formulas, formula style is off by default.

    <item*|<menu|Insert|Mathematics|Equation> or <shortcut|(make-equation*)>>

    This entry is used for entering bigger <em|displayed equations>, like

    <\equation*>
      x<rsup|n>+y<rsup|n>=z<rsup|n>,
    </equation*>

    which are typeset in a paragraph of their own. You may use the shortcut
    <shortcut|(numbered-toggle (focus-tree))> in order to give the equation a number (or to
    remove the number of an equation). Also, <shortcut|(variant-circulate (focus-tree)
    #t)> allows you to switch between inline formulas and displayed
    equations.

    <item*|<menu|Insert|Mathematics|Equations> or
    <shortcut|(make-eqnarray*)>>

    This entry allows you to create an <markup|eqnarray*>, a three columns
    wide table-like environment (see <hlink|creating
    tables|../table/man-create-table.en.tm>). This environment is typically
    used for lists of multiple relations like

    <\eqnarray*>
      <tformat|<table|<row|<cell|x+0>|<cell|=>|<cell|x>>|<row|<cell|x+(\<um\>x)>|<cell|=>|<cell|0>>|<row|<cell|x+y>|<cell|=>|<cell|y+x>>|<row|<cell|(x+y)+z>|<cell|=>|<cell|x+(y+z)>>>>
    </eqnarray*>

    The first column is centered to the right, the second one at the middle
    and the last one at the left. An other typical use of the
    <markup|eqnarray*> environment is a step by step computation like

    <\eqnarray*>
      <tformat|<table|<row|<cell|<left|(>\<mathe\><rsup|sin x>+sin
      \<mathe\><rsup|x><right|)><rprime|'>>|<cell|=>|<cell|<left|(>\<mathe\><rsup|sin
      x>)<rprime|'>+<left|(>sin \<mathe\><rsup|x><right|)><rprime|'>>>|<row|<cell|>|<cell|=>|<cell|(sin
      x)<rprime|'>*\<mathe\><rsup|sin x>+<left|(>\<mathe\><rsup|x><right|)><rprime|'>*sin
      \<mathe\><rsup|x>>>|<row|<cell|>|<cell|=>|<cell|\<mathe\><rsup|sin
      x>*cos x+\<mathe\><rsup|x>*sin \<mathe\><rsup|x>,>>>>
    </eqnarray*>

    in which many entries of the left column are left open.
  </description>

  In math mode, you have specific commands and key-combinations to type
  mathematical symbols and formulas. For instance, the <prefix|math:greek>
  prefix can be used in order to enter Greek symbols.

  The editor favors typing mathematics with a certain meaning. This feature,
  which will be developed more in future releases, is useful when
  communicating with a computer algebra package. At this moment, you should
  for instance explicitly type the multiplication <key|*> between symbols
  <math|a> and <math|b>. By default, typing <key|a b> will yield
  <with|mode|math|mode|text|ab> and not <math|a*b>.

  <\traverse>
    <branch|Main mathematical constructs|keyboard/man-main.en.tm>

    <branch|Mathematical symbols|keyboard/man-symbols.en.tm>

    <branch|Big operators|keyboard/man-big.en.tm>

    <branch|Large delimiters|keyboard/man-large.en.tm>

    <branch|Wide accents|keyboard/man-wide.en.tm>

    <branch|Mathematical semantics|semantics/man-semantics.en.tm>
  </traverse>

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
    <associate|language|english>
  </collection>
</initial>