<TeXmacs|1.0.7.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Positioning and resizing objects>

  The <prefix|structured:geometry> prefix may be used for positioning and
  resizing objects. For instance, inside a cell of a<nbsp>table, you may use
  <key|structured:geometry right> to align the cell more to the right. Behind
  a space introduced <em|via> <menu|Format|Space>, the same key allows you to
  increase the width of space. More generally, the following shortcuts are
  implemented:

  <\description>
    <item*|<shortcut|(geometry-horizontal (focus-tree) #f)>>Decrease the horizontal size of an
    object, or align more to the left.

    <item*|<shortcut|(geometry-horizontal (focus-tree) #t)>>Increase the horizontal size of an
    object, or align more to the right.

    <item*|<shortcut|(geometry-vertical (focus-tree) #t)>>Decrease/increase the vertical size of
    an object, or align more to the bottom.

    <item*|<shortcut|(geometry-vertical (focus-tree) #f)>>Increase/decrease the vertical size of an
    object, or align more to the top.

    <item*|<shortcut|(geometry-extremal (focus-tree) #f)>>Decrease the horizontal offset of an
    object, or left align.

    <item*|<shortcut|(geometry-extremal (focus-tree) #t)>>Increase the horizontal offset of an
    object, or right align.

    <item*|<shortcut|(geometry-incremental (focus-tree) #t)>>Decrease the vertical offset of an
    object, or align at the bottom.

    <item*|<shortcut|(geometry-incremental (focus-tree) #f)>>Increase the vertical offset of an
    object, or align at the top.

    <item*|<shortcut|(geometry-default (focus-tree))>>Revert the geometry (size, position,
    alignment) to the defaults.

    <item*|<shortcut|(geometry-variant (focus-tree) #t)>, <shortcut|(geometry-variant (focus-tree)
    #f)>>Circulate among available length units for specifying the geometry.

    <item*|<shortcut|(geometry-speed (focus-tree) #f)>, <shortcut|(geometry-speed (focus-tree) #t)>>Decrease
    or increase the step size when positioning or resizing.
  </description>

  Particular tags to which the shortcuts apply are the following

  <\description>
    <item*|Spaces>Both horizontal and vertical spaces from the
    <menu|Format|Space> menu. You should put the cursor just after the space
    tag for the shortcuts to apply.

    <item*|Box modifiers>The tags <markup|move>, <markup|shift>,
    <markup|resize> and <markup|clipped><compound|markup|> from the
    <menu|Format|Transform> menu.

    <item*|Animations>The durations of animations can be modified using
    <shortcut|(geometry-horizontal (focus-tree) #f)> and <shortcut|(geometry-horizontal (focus-tree) #t)>.

    <item*|Images>The size and alignment of images can be changed.
  </description>

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