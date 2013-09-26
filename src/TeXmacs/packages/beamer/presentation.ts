<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|presentation|1.0|presentation|1.0>

    <\src-purpose>
      Presentation style.
    </src-purpose>

    <src-copyright|2007--2010|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|alt-colors|ornaments|varsession>

  <\active*>
    <\src-comment>
      Global document layout
    </src-comment>
  </active*>

  <assign|page-type|4:3>

  <assign|page-medium|beamer>

  <assign|page-screen-left|5mm>

  <assign|page-screen-right|5mm>

  <assign|page-screen-top|5mm>

  <assign|page-screen-bot|5mm>

  <assign|magnification|1.7>

  <assign|font-family|ss>

  <assign|name|<macro|body|<with|font-family|rm|font-shape|small-caps|<arg|body>>>>

  <assign|item-vsep|<macro|0fn>>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|title-bar-color|dark blue>

  <assign|title-color|white>

  <assign|tit|<macro|body|<with|ornament-color|<value|title-bar-color>|<ornament|<htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|<value|title-color>|math-color|<value|title-color>|<arg|body>>>>|0fn|0.333fn><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Customized session elements
    </src-comment>
  </active*>

  <assign|session|<\macro|language|session|body>
    <\with|prog-language|<arg|language>|prog-session|<arg|session>>
      <\small>
        <render-session|<arg|body>>
      </small>
    </with>
  </macro>>

  <assign|folded-body|<macro|body|<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Miscellaneous
    </src-comment>
  </active*>

  <assign|img|<macro|body|<with|ornament-color|white|<ornament|<arg|body>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>