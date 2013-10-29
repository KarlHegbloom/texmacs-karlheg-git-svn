<TeXmacs|1.0.7.20>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-utils|1.0>

    <\src-purpose>
      This package contains useful macros for writing style files.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Helper macros for wide block environments (work also for inline
      content, like section titles).
    </src-comment>
  </active*>

  <assign|hflush|<macro|<htab|0fn|0>>>

  <assign|right-flush|<macro|<htab|0fn|first>>>

  <assign|left-flush|<macro|<htab|0fn|last>>>

  <assign|wide-normal|<macro|body|<surround|<no-indent>|<htab|0fn|first>|<arg|body>>>>

  <assign|wide-centered|<macro|body|<surround|<no-indent><htab|0fn|last>|<htab|0fn|first>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Helper macros for vertically padded environments
    </src-comment>
  </active*>

  <assign|padding-above|0.5fn>

  <assign|padding-below|0.5fn>

  <assign|large-padding-above|1fn>

  <assign|large-padding-below|1fn>

  <assign|padded-normal|<macro|before|after|body|<surround|<vspace*|<arg|before>><no-indent>|<htab|0fn|first><vspace|<arg|after>>|<arg|body>>>>

  <assign|padded-centered|<macro|before|after|body|<surround|<vspace*|<arg|before>><no-indent><htab|0fn|last>|<htab|0fn|first><vspace|<arg|after>>|<arg|body>>>>

  <assign|padded|<\macro|body>
    <\padded-normal|<value|padding-above>|<value|padding-below>>
      <arg|body>
    </padded-normal>
  </macro>>

  <assign|padded*|<\macro|body>
    <\padded-normal|<value|large-padding-above>|<value|large-padding-below>>
      <arg|body>
    </padded-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Helper macros for underlined/overlined block environments (work also
      for inline content, like page headers).
    </src-comment>
  </active*>

  <assign|overlined-width|1ln>

  <assign|overlined-sep|1sep>

  <assign|underlined-width|1ln>

  <assign|underlined-sep|1sep>

  <assign|wide-bothlined|<macro|top-border|bot-border|top-sep|bot-sep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-lsep|0pt>|<cwith|1|1|1|1|cell-rsep|0pt>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-tborder|<arg|top-border>>|<cwith|1|1|1|1|cell-bborder|<arg|bot-border>>|<cwith|1|1|1|1|cell-tsep|<arg|top-sep>>|<cwith|1|1|1|1|cell-bsep|<arg|bot-sep>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-std-bothlined|<macro|body|<wide-bothlined|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>>>>

  <assign|padded-bothlined|<macro|before|after|top-border|bot-border|top-sep|bot-sep|body|<surround|<vspace*|<arg|before>>|<vspace|<arg|after>>|<wide-bothlined|<arg|top-border>|<arg|bot-border>|<arg|top-sep>|<arg|bot-sep>|<arg|body>>>>>

  <assign|padded-std-bothlined|<\macro|before|after|body>
    <padded-bothlined|<arg|before>|<arg|after>|<value|overlined-width>|<value|underlined-width>|<value|overlined-sep>|<value|underlined-sep>|<arg|body>>
  </macro>>

  <assign|wide-overlined|<macro|tborder|tsep|body|<wide-bothlined|<arg|tborder>|0pt|<arg|tsep>|0pt|<arg|body>>>>

  <assign|wide-std-overlined|<macro|body|<wide-overlined|<value|overlined-width>|<value|overlined-sep>|<arg|body>>>>

  <assign|wide-underlined|<macro|bborder|bsep|body|<wide-bothlined|0pt|<arg|bborder>|0pt|<arg|bsep>|<arg|body>>>>

  <assign|wide-std-underlined|<macro|body|<wide-underlined|<value|underlined-width>|<value|underlined-sep>|<arg|body>>>>

  <assign|overlined|<\macro|body>
    <padded|<wide-std-overlined|<arg|body>>>
  </macro>>

  <assign|underlined|<\macro|body>
    <padded|<wide-std-underlined|<arg|body>>>
  </macro>>

  <assign|bothlined|<\macro|body>
    <padded|<wide-std-bothlined|<arg|body>>>
  </macro>>

  <\active*>
    <\src-comment>
      Helper macros for framed block environments (work also for inline
      content).
    </src-comment>
  </active*>

  <assign|framed-color|>

  <assign|framed-width|1ln>

  <assign|framed-hsep|1spc>

  <assign|framed-vsep|1sep>

  <assign|wide-framed|<macro|border-width|hsep|vsep|body|<surround|<no-indent>||<tabular|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-lborder|<arg|border-width>>|<cwith|1|1|1|1|cell-rborder|<arg|border-width>>|<cwith|1|1|1|1|cell-tborder|<arg|border-width>>|<cwith|1|1|1|1|cell-bborder|<arg|border-width>>|<cwith|1|1|1|1|cell-lsep|<arg|hsep>>|<cwith|1|1|1|1|cell-rsep|<arg|hsep>>|<cwith|1|1|1|1|cell-tsep|<arg|vsep>>|<cwith|1|1|1|1|cell-bsep|<arg|vsep>>|<cwith|1|1|1|1|cell-background|<value|framed-color>>|<table|<row|<cell|<arg|body>>>>>>>>>

  <assign|wide-std-framed|<macro|body|<wide-framed|<value|framed-width>|<value|framed-hsep>|<value|framed-vsep>|<arg|body>>>>

  <assign|wide-framed-colored|<macro|border-color|body-color|border-width|hsep|vsep|body|<with|old-color|<value|color>|color|<arg|border-color>|framed-color|<arg|body-color>|<wide-framed|<arg|border-width>|<arg|hsep>|<arg|vsep>|<with|color|<value|old-color>|<style-with|src-compact|none|<arg|body>>>>>>>

  <assign|wide-std-framed-colored|<macro|border-color|body-color|body|<wide-framed-colored|<arg|border-color>|<arg|body-color>|<value|framed-width>|<value|framed-hsep>|<value|framed-vsep>|<style-with|src-compact|none|<arg|body>>>>>

  <assign|framed|<\macro|body>
    <padded|<wide-std-framed|<arg|body>>>
  </macro>>

  <assign|ornamented|<\macro|body>
    <padded|<\ornament>
      <\surround||<right-flush>>
        <arg|body>
      </surround>
    </ornament>>
  </macro>>

  <\active*>
    <\src-comment>
      Indentation.
    </src-comment>
  </active*>

  <assign|indent-left|<\macro|indentation|body>
    <\with|par-left|<plus|<value|par-left>|<arg|indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-right|<\macro|indentation|body>
    <\with|par-right|<plus|<value|par-right>|<arg|indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|indent-both|<\macro|left-indentation|right-indentation|body>
    <\with|par-left|<plus|<value|par-left>|<arg|left-indentation>>|par-right|<plus|<value|par-right>|<arg|right-indentation>>>
      <arg|body>
    </with>
  </macro>>

  <assign|margin-first-other|<macro|head-indent|tail-indent|body|<with|par-left|<plus|<arg|tail-indent>|0em>|par-first|<minus|<arg|head-indent>|<arg|tail-indent>>|<surround|<yes-indent>||<arg|body>>>>>

  <\active*>
    <\src-comment>
      Indentation.
    </src-comment>
  </active*>

  <assign|paragraph-box|<\macro|width|body>
    <tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-vcorrect|n>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-width|<arg|width>>|<cwith|1|1|1|1|cell-hmode|exact>|<table|<row|<\cell>
      <arg|body>
    </cell>>>>>
  </macro>>

  <\active*>
    <\src-comment>
      Header information.
    </src-comment>
  </active*>

  <assign|page-number|<macro|<quote|<page-the-page>>>>

  <assign|set-this-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|this
  page header>|brown|header><assign|page-this-header|<arg|header>>>>>

  <assign|set-header|<macro|header|<style-with|src-compact|none|<flag|<localize|header>|brown|header><assign|page-odd-header|<arg|header>><assign|page-even-header|<arg|header>>>>>

  <assign|set-odd-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|odd
  page header>|brown|header><assign|page-odd-header|<arg|header>>>>>

  <assign|set-even-page-header|<macro|header|<style-with|src-compact|none|<flag|<localize|even
  page header>|brown|header><assign|page-odd-header|<arg|header>>>>>

  <assign|set-this-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|this
  page footer>|brown|footer><assign|page-this-footer|<arg|footer>>>>>

  <assign|set-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|footer>|brown|footer><assign|page-odd-footer|<arg|footer>><assign|page-even-footer|<arg|footer>>>>>

  <assign|set-odd-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|odd
  page footer>|brown|footer><assign|page-odd-footer|<arg|footer>>>>>

  <assign|set-even-page-footer|<macro|footer|<style-with|src-compact|none|<flag|<localize|even
  page footer>|brown|footer><assign|page-odd-footer|<arg|footer>>>>>

  <assign|set-page-number|<macro|nr|<style-with|src-compact|none|<flag|<localize|renumber
  this page>|brown|nr><assign|page-nr|<arg|nr>>>>>

  <assign|set-page-number-macro|<macro|rendering-macro|<style-with|src-compact|none|<flag|<localize|page
  number text>|brown|rendering-macro><assign|page-the-page|<arg|rendering-macro>>>>>

  <assign|blanc-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|>>>>

  <assign|simple-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<htab|5mm><page-number><htab|5mm>>>>>

  <\active*>
    <\src-comment>
      Other macros.
    </src-comment>
  </active*>

  <assign|localize|<macro|text|<translate|<arg|text>|english|<value|language>>>>

  <assign|map|<macro|fun|args|<extern|ext-map|<arg|fun>|<arg|args>>>>

  <assign|concat-tuple|<macro|args|sep|fin|<extern|ext-concat-tuple|<arg|args>|<arg|sep>|<arg|fin>>>>

  <assign|comma-separated|<xmacro|x|<concat-tuple|<quote-arg|x>|, >>>

  <assign|semicolon-separated|<xmacro|x|<concat-tuple|<quote-arg|x>|; >>>

  <drd-props|comma-separated|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|semicolon-separated|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|font-magnify|<macro|factor|body|<with|font-size|<times|<value|font-size>|<arg|factor>>|<arg|body>>>>

  <assign|hidden-title|<macro|title|>>

  <assign|wiki-link|<macro|body|name|<style-with|src-compact|none|<action|<arg|body>|<merge|(remote-project-load-by-name
  "|<arg|name>|")>>>>>

  <assign|inline-block|<macro|body|<tabular|<tformat|<cwith|1|1|1|1|cell-lsep|0fn>|<twith|table-width|1par>|<cwith|1|1|1|1|cell-rsep|0fn>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
    <arg|body>
  </cell>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>