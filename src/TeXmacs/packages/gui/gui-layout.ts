<TeXmacs|1.0.6.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-layout|1.0>

    <\src-purpose>
      Layout subroutines for the <TeXmacs> GUI
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Environment variables for GUI layout
    </src-comment>
  </active*>

  <assign|gui-small-sep|0.25em>

  <assign|gui-med-sep|0.5em>

  <assign|gui-big-sep|0.75em>

  <assign|raised-color|light grey>

  <assign|lowered-color|#e0e0e0>

  <assign|bright-color|<pattern|crinkle-paper.png||>>

  <assign|bright-color|#f0f0f0>

  <assign|sunny-color|white>

  <assign|shadow-color|grey>

  <assign|short-width|0em>

  <\active*>
    <\src-comment>
      Tables
    </src-comment>
  </active*>

  <assign|dense-tile|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0px>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|-1|1|1|-1|cell-tsep|0px>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|short-tile|<macro|body|<tformat|<twith|table-valign|C>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|wide-tile|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<arg|body>>>>

  <assign|association-tile|<macro|body|<tformat|<twith|table-valign|C>|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<cwith|1|-1|1|-1|cell-hyphen|t>|<cwith|1|-1|1|1|cell-lsep|0px>|<cwith|1|-1|2|-1|cell-lsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-rsep|0px>|<cwith|1|1|1|-1|cell-tsep|0px>|<cwith|2|-1|1|-1|cell-tsep|<value|gui-med-sep>>|<cwith|1|-1|1|-1|cell-bsep|0px>|<cwith|1|-1|1|1|cell-halign|r>|<cwith|1|-1|1|1|cell-hyphen|n>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Framed boxes
    </src-comment>
  </active*>

  <assign|short-highlight|<macro|bg|sunny|shadow|pad|body|<with|ornament-color|<arg|bg>|ornament-sunny-color|<arg|sunny>|ornament-shadow-color|<arg|shadow>|ornament-hpadding|<arg|pad>|ornament-vpadding|<arg|pad>|<style-with|src-compact|none|<ornament|<vcorrect|<arg|body>>>>>>>

  <assign|wide-highlight|<\macro|bg|sunny|shadow|pad|body>
    <\with|ornament-color|<arg|bg>|ornament-sunny-color|<arg|sunny>|ornament-shadow-color|<arg|shadow>|ornament-hpadding|<arg|pad>|ornament-vpadding|<arg|pad>>
      <\ornament>
        <\wide-normal>
          <vcorrect|<arg|body>>
        </wide-normal>
      </ornament>
    </with>
  </macro>>

  <assign|block-highlight|<\macro|bg|sunny|shadow|pad|body>
    <\with|ornament-color|<arg|bg>|ornament-sunny-color|<arg|sunny>|ornament-shadow-color|<arg|shadow>|ornament-hpadding|<arg|pad>|ornament-vpadding|<arg|pad>>
      <\ornament>
        <\wide-normal>
          <arg|body>
        </wide-normal>
      </ornament>
    </with>
  </macro>>

  <assign|short-raise|<macro|body|<short-highlight|<value|raised-color>|<value|sunny-color>|<value|shadow-color>|<value|gui-small-sep>|<arg|body>>>>

  <assign|short-lower|<macro|body|<short-highlight|<value|lowered-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>|<arg|body>>>>

  <assign|short-bright|<macro|body|<short-highlight|<value|bright-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>|<arg|body>>>>

  <assign|wide-raise|<\macro|body>
    <wide-highlight|<value|raised-color>|<value|sunny-color>|<value|shadow-color>|<value|gui-small-sep>|<arg|body>>
  </macro>>

  <assign|wide-lower|<\macro|body>
    <wide-highlight|<value|lowered-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>|<arg|body>>
  </macro>>

  <assign|wide-bright|<\macro|body>
    <wide-highlight|<value|bright-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>|<arg|body>>
  </macro>>

  <assign|block-raise|<\macro|body>
    <\block-highlight|<value|raised-color>|<value|sunny-color>|<value|shadow-color>|<value|gui-small-sep>>
      <arg|body>
    </block-highlight>
  </macro>>

  <assign|block-lower|<\macro|body>
    <\block-highlight|<value|lowered-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>>
      <arg|body>
    </block-highlight>
  </macro>>

  <assign|block-bright|<\macro|body>
    <\block-highlight|<value|bright-color>|<value|shadow-color>|<value|sunny-color>|<value|gui-small-sep>>
      <arg|body>
    </block-highlight>
  </macro>>

  <\active*>
    <\src-comment>
      Spacing markup for graphical user interfaces
    </src-comment>
  </active*>

  <assign|gui-ruler|<macro|around|left|right|height|col|<dense-tile|<tformat|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|1|1|-1|cell-height|<arg|around>>|<cwith|2|2|1|-1|cell-height|1px>|<cwith|3|3|1|-1|cell-height|<arg|height>>|<cwith|4|4|1|-1|cell-height|1px>|<cwith|5|5|1|-1|cell-height|<arg|around>>|<cwith|2|2|1|-1|cell-background|<value|shadow-color>>|<cwith|4|4|1|-1|cell-background|<value|sunny-color>>|<cwith|3|3|1|1|cell-background|<value|shadow-color>>|<cwith|3|3|2|2|cell-background|<arg|col>>|<cwith|3|3|3|3|cell-background|<value|sunny-color>>|<cwith|1|-1|1|1|cell-width|<arg|left>>|<cwith|1|-1|1|1|cell-hmode|exact>|<cwith|1|-1|3|3|cell-width|<arg|right>>|<cwith|1|-1|3|3|cell-hmode|exact>|<cwith|1|-1|2|2|cell-hpart|1>|<table|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>|<row|<cell|>|<cell|>|<cell|>>>>>>>

  <assign|gui-vspace|<macro|height|<dense-tile|<tformat|<cwith|1|-1|1|1|cell-vmode|exact>|<cwith|1|1|1|1|cell-height|<arg|height>>|<table|<row|<cell|>>>>>>>

  <assign|gui-hrule|<macro|<gui-ruler|<value|gui-med-sep>|0px|0px|0px|<value|lowered-color>>>>

  <assign|gui-smallskip|<macro|<gui-vspace|<value|gui-small-sep>>>>

  <assign|gui-medskip|<macro|<gui-vspace|<value|gui-med-sep>>>>

  <assign|gui-bigskip|<macro|<gui-vspace|<value|gui-big-sep>>>>

  <assign|gui-tab|<macro|<tabular|<tformat|<twith|table-width|1par>|<twith|table-hmode|exact>|<cwith|1|1|1|1|cell-lsep|0px>|<cwith|1|1|1|1|cell-rsep|0px>|<cwith|1|1|1|1|cell-bsep|0px>|<cwith|1|1|1|1|cell-tsep|0px>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-vcorrect|n>|<table|<row|<\cell>
    \;
  </cell>>>>>>>

  <\active*>
    <\src-comment>
      Widget headers
    </src-comment>
  </active*>

  <assign|header-bar-sub|<macro|l|c|r|<dense-tile|<tformat|<cwith|1|-1|1|-1|cell-hyphen|n>|<cwith|1|1|1|1|cell-hpart|1>|<cwith|1|1|3|3|cell-hpart|1>|<cwith|1|1|3|3|cell-halign|r>|<cwith|1|1|2|2|cell-halign|c>|<cwith|1|1|1|1|cell-halign|l>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|3|3|cell-hyphen|t>|<cwith|1|1|2|2|cell-width|>|<cwith|1|1|2|2|cell-hmode|auto>|<table|<row|<\cell>
    <arg|l>
  </cell>|<cell|<arg|c>>|<\cell>
    <arg|r>
  </cell>>>>>>>

  <assign|header-bar|<macro|body|<style-with|src-compact|none|<header-bar-sub|<gui-ruler|0px|1px|0px|5px|<value|lowered-color>>|<arg|body>|<gui-ruler|0px|0px|1px|5px|<value|lowered-color>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>