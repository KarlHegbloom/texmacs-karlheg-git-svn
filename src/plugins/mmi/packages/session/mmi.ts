<TeXmacs|1.0.7.18>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mmi|1.0>

    <\src-purpose>
      Markup for Mmi sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|tmdoc-markup>

  <\active*>
    <\src-comment>
      Customize input/output fields
    </src-comment>
  </active*>

  <assign|mmi-prompt|<macro|nr|<with|prog-language|verbatim|Mmi]<specific|html|&nbsp;>
  >>>

  <assign|mmi-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<with|color|red|<arg|prompt>>|<with|color|dark
  brown|<arg|body>>>>>>

  <assign|mmi-output|<macro|body|<generic-output|<with|prog-language|verbatim|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Other macros and environments.
    </src-comment>
  </active*>

  <assign|text|<macro|body|<with|mode|text|par-mode|justify|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>