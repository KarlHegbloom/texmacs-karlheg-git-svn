<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Box operation primitives>

  <\explain>
    <explain-macro|move|content|delta-x|delta-y><explain-synopsis|adjust
    position>
  <|explain>
    This primitive moves the box with the specified <src-arg|content> by
    <src-arg|delta-x> to the right and <src-arg|delta-y> upwards. It may be
    used for fine-grained positioning.
  </explain>

  <\explain>
    <explain-macro|resize|content|left-lim|bot-lim|right-lim|top-lim><explain-synopsis|adjust
    size>
  <|explain>
    Resize the box for the <src-arg|content> according to new left, bottom,
    right and top limits <src-arg|left-lim>, <src-arg|bot-lim>,
    <src-arg|right-lim> and <src-arg|top-lim>. The limits may be either be
    empty strings (in which case the old limit is taken), an absolute
    coordinate, or a limit computed as a function of the old limit.

    In the last case, the limit string should be of the form
    <verbatim|\<less\>pos\<gtr\>\<less\>op\<gtr\>\<less\>len\<gtr\>>. The
    first character <verbatim|\<less\>pos\<gtr\>> indicates a position in the
    old box and should be either <verbatim|l> (left), <verbatim|b> (bottom),
    <verbatim|c> (center), <verbatim|r> (right) or <verbatim|t> (top). The
    second character <verbatim|\<less\>op\<gtr\>> indicates the operation
    which will be performed on this position and the remaining length string
    <verbatim|\<less\>len\<gtr\>> in order to yield the new position.
    Possible operations are <verbatim|+>, <verbatim|->, <verbatim|[> and
    <verbatim|]>. The brackets <verbatim|[> and <verbatim|]> stand for
    ``minimum'' and ``maximum''. For instance, the code

    <\tm-fragment>
      <inactive*|(<resize|Hopsa|l-5mm||r+5mm||>)>
    </tm-fragment>

    widens the box for ``Hopsa'' by <verbatim|5mm> on each side:

    <\tm-fragment>
      (<resize|Hopsa|l-5mm||r+5mm||>)
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|if*|condition|content><explain-synopsis|conditional
    appearance of box>
  <|explain>
    The box with the <src-arg|content> is displayed as usual if the
    <src-arg|condition> is satisfied and displayed as whitespace otherwise.
    This primitive is used in particular for the definition of the
    <markup|phantom> macro. For instance, the non-text
    ``<if*|false|phantom>'' is produced using
    <inactive*|<if*|false|phantom>>.
  </explain>

  <\explain>
    <explain-macro|repeat|content|pattern><explain-synopsis|fill line>
  <|explain>
    This primitive can be used to decorate some <src-arg|content> with a
    given <src-arg|pattern>. For instance, when defining the macro

    <\tm-fragment>
      <inactive*|<assign|wipe-out|<macro|x|<repeat|<arg|x>|<with|color|red|/>>>>>
    </tm-fragment>

    the code <inactive*|<wipe-out|obsolete>> produces
    <with|wipe-out|<macro|x|<repeat|<arg|x>|<with|color|red|/>>>|<wipe-out|obsolete>>.
    The <markup|repeat> primitive may also be used to fill the current line
    with a given content, like the dots in tables of
    contents.<repeat|<htab|5mm>|...>
  </explain>

  <\explain>
    <explain-macro|datoms|foo|content>

    <explain-macro|dlines|foo|content>

    <explain-macro|dpages|foo|content><explain-synopsis|decorations>
  <|explain>
    These primitives are used to decorate <em|a posteriori> the lines of a
    paragraph, the lines of a page, or the pages of a document. Currently,
    only decorations of atoms on lines of a paragraph have been implemented.

    The first argument <src-arg|foo> is a macro which will be applied to all
    boxes in the line and the second argument <src-arg|content> is the part
    of the paragraph to which the decoration will be applied. For instance,
    the construction

    <\tm-fragment>
      <inactive*|<style-with|src-compact|none|<datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|<arg|body>>>>
    </tm-fragment>

    may be used in order to visualize the boxes in a given paragraph:

    <\quote-env>
      <datoms|<macro|x|<active*|<block|<tformat|<table|<row|<cell|<arg|x>>>>>>>>|Here
      is a sufficiently long paragraph. Here is a sufficiently long
      paragraph. Here is a sufficiently long paragraph. Here is a
      sufficiently long paragraph. Here is a sufficiently long paragraph.
      Here is a sufficiently long paragraph.>
    </quote-env>

    When used in combination with the <markup|repeat> primitive, one may for
    instance produce the dotted lines in tables of contents using the macro

    <\tm-fragment>
      <inactive*|<style-with|src-compact|none|<assign|toc-dots|<macro|<style-with|src-compact|none|<datoms|<macro|x|<repeat|<arg|x>|<space|0.2fn>.<space|0.2fn>>>|<htab|5mm>>>>>>>
    </tm-fragment>

    Notice that the <markup|datoms> primitive is quite fragile, because the
    <src-arg|foo> macro has no access to the environment in which
    <src-arg|content> is typeset.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>