<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|env-float|1.0>

    <\src-purpose>
      Environments for floating content.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|figure-name|<macro|name|<with|font-series|bold|<arg|name>>>>

  <assign|figure-sep|<macro|. >>

  <assign|footnote-sep|<macro|. >>

  <\active*>
    <\src-comment>
      Figure-like environments.
    </src-comment>
  </active*>

  <assign|caption-detailed|<\macro|long|short>
    <arg|long>
  </macro>>

  <assign|caption-summarized|<\macro|long|short>
    <arg|short>
  </macro>>

  <drd-props|caption-detailed|arity|2|accessible|0|border|no>

  <drd-props|caption-summarized|arity|2|accessible|1|border|no>

  <assign|list-caption|<macro|type|cap|<with|caption-detailed|<value|caption-summarized>|<style-with|src-compact|none|<auto-label><write|<arg|type>|<tuple|normal|<arg|cap>|<pageref|<the-auto>>>>>>>>

  <assign|render-small-figure|<macro|type|name|fig|cap|<tabular*|<tformat|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<twith|table-valign|B>|<cwith|3|3|1|1|cell-hyphen|t>|<twith|table-width|1par>|<twith|table-hmode|min>|<table|<row|<cell|<resize|<arg|fig>|<minus|1l|2fn>||<plus|1r|2fn>|>>>|<row|<cell|>>|<row|<\cell>
    <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
      <arg|cap>
    </surround>>
  </cell>>>>>>>

  <assign|render-big-figure|<\macro|type|name|fig|cap>
    <padded-normal|1fn|1fn|<tabular*|<tformat|<twith|table-width|1par>|<cwith|3|3|1|1|cell-hyphen|t>|<cwith|1|-1|1|-1|cell-lsep|0spc>|<cwith|1|-1|1|-1|cell-rsep|0spc>|<cwith|2|2|1|1|cell-height|0.5fn>|<cwith|3|3|1|1|cell-lsep|1.5fn>|<cwith|3|3|1|1|cell-rsep|1.5fn>|<table|<row|<cell|<arg|fig>>>|<row|<cell|>>|<row|<\cell>
      <small|<\surround|<figure-name|<arg|name><figure-sep>><list-caption|<arg|type>|<arg|cap>>|>
        <arg|cap>
      </surround>>
    </cell>>>>>>
  </macro>>

  <new-figure|figure|Figure>

  <new-figure|table|Table>

  <\active*>
    <\src-comment>
      Footnotes.
    </src-comment>
  </active*>

  <assign|render-footnote-label|<macro|sym|id|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|||<\surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<hard-id|<arg|body>>|<url|<merge|#source-|<arg|id>>>>|<arg|sym>><footnote-sep>|<right-flush>>
      <\tag|<merge|dest-|<arg|id>>>
        <style-with|src-compact|none|<arg|body>>
      </tag>
    </surround>>>>
  </float>>>>

  <assign|render-footnote-ref|<macro|sym|id|body|<tag|<merge|source-|<arg|id>>|<arg|body>><rsup|<locus|<link|hyperlink|<hard-id|<arg|body>>|<url|<merge|#dest-|<arg|id>>>>|<arg|sym>>>>>

  <assign|render-footnote*|<macro|sym|nr|body|<style-with|src-compact|none|<\float|footnote|>
    <smaller|<with|par-mode|justify|par-left|0cm|par-right|0cm|<style-with|src-compact|none|<surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#footnr-|<arg|nr>>>>|<arg|sym>><footnote-sep>|<set-binding|<merge|footnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>

  <assign|render-footnote|<macro|nr|body|<style-with|src-compact|none|<render-footnote*|<arg|nr>|<arg|nr>|<arg|body>>>>>

  <assign|footnote|<macro|body|<style-with|src-compact|none|<next-footnote><render-footnote|<the-footnote>|<arg|body>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<reference|<merge|footnote-|<the-footnote>>>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|zoom-factor|1.35021>
  </collection>
</initial>