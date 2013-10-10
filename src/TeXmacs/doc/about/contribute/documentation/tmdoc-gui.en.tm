<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|Graphical user interface related markup>

  The following markup elements can be used in order to describe various
  graphical user interface elements, such as keyboard shortcuts, menus or
  icons.

  <\explain|<markup|shortcut>>
    This macro is used to indicate a keyboard shortcut for a <scheme>
    command. For instance, the shortcut for <scm|(new-buffer)> is
    <shortcut|(new-buffer)>.
  </explain>

  <\explain|<markup|key>>
    This unary macro is used for explicit keyboard input. For instance, when
    giving <rigid|<verbatim|A C-b return>> as argument, the result is <key|A
    C-b return>.
  </explain>

  <\explain|<markup|menu>>
    This function with an arbitrary number of arguments indicates a menu like
    <menu|File> or <menu|Document|Language>. Menu entries are automatically
    translated by this function.
  </explain>

  <\explain|<markup|submenu>>
    Consider the following sentence:

    <\quote-env>
      ``You may use the <submenu|File|Load> and <submenu|File|Save> entries
      of the <menu|File> menu in order to load and save files.''
    </quote-env>

    In this example, the menu entries <submenu|File|Load> and
    <submenu|File|Save> were marked using the <markup|submenu> tag, which
    takes the implicit <menu|File> menu as its first invisible argument. This
    invisible argument is still taken into account when building the index
    (for instance). In a similar way, we provide <markup|subsubmenu> and
    <markup|subsubsubmenu> tags.
  </explain>

  <\explain|<markup|icon>>
    Can be used in order to specify one of the <TeXmacs> icons, such as
    <icon|tm_open.xpm> and <icon|tm_save.xpm>. The macro takes one argument
    with the file name of the icon (the full path is not needed).
  </explain>

  <\explain|<markup|screenshot>>
    Similar to the <markup|icon> tag, but for screenshots.
  </explain>

  <\explain|<markup|cursor>>
    This macro can be used to indicate a cursor position, as in
    <math|a<rsup|2>+b<rsup|2<cursor>>=c<rsup|2>>.
  </explain>

  <\explain|<markup|small-focus>, <markup|small-envbox>>
    This macro can be used for indicating the visual aids around the current
    focus and the further outer context (<abbr|e.g.>
    <math|<small-envbox|a+<frac|b|<small-focus|c>>>>), in the case of inline
    elements.
  </explain>

  <\explain|<markup|big-focus>, <markup|big-envbox>>
    Block versions of <markup|small-focus> and <markup|small-envbox>.
  </explain>

  Notice that the contents of none of the above tags should be translated
  into foreign languages. Indeed, for menu tags, the translations are done
  automatically, so as to keep the translations synchronized with the
  translations of the actual <TeXmacs> menus. In the cases of markup, styles,
  packages and <abbr|d.t.d.>s, it is important to keep the original name,
  because it often corresponds to a file name.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven>

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