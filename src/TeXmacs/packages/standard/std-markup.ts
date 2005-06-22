<TeXmacs|1.0.5.4>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|std-markup|1.0>

    <\src-purpose>
      This package contains several frequently used macros.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      The following macros don't take any arguments.
    </src-comment>
  </active*>

  <assign|TeXmacs|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X<rsub|<space|-0.4spc><move|<resize|M<space|-0.2spc>A<space|-0.4spc>CS||||0.5fn|>|0fn|-0.1fn>>>>>

  <assign|TeXmacs-version|<macro|<extern|texmacs-version>>>

  <assign|TeXmacs-version-release|<macro|type|<extern|texmacs-version-release*|<quote-arg|type>>>>

  <assign|made-by-TeXmacs|<macro|<float|footnote||<with|font-size|0.84|par-mode|justify|par-left|0cm|par-right|0cm|<active*|<move|<postscript|local:$TEXMACS_PATH/misc/images/tm_gnu3.ps||1fn||||>|0fn|-0.2fn>><space|2spc><localize|This
  document has been produced using> GNU <TeXmacs> (<localize|see>
  <with|font-family|tt|http://www.texmacs.org>).<right-flush>>>>>

  <assign|TeX|<macro|<active*|T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|LaTeX|<macro|<active*|L<rsup|<space|-0.8spc><move|A|0fn|-0.1fn>><space|-0.2spc>T<rsub|<space|-0.4spc><move|<resize|<with|math-level|0|E>||||0.5fn|>|0fn|-0.1fn>><space|-0.4spc>X>>>

  <assign|hrule|<macro|<no-indent><tabular|<tformat|<cwith|1|-1|1|-1|cell-tborder|1ln>|<twith|table-width|1par>|<cwith|1|-1|1|-1|cell-vmode|exact>|<cwith|1|-1|1|-1|cell-height|1ln>|<cwith|1|-1|1|-1|cell-lsep|0fn>|<cwith|1|-1|1|-1|cell-rsep|0fn>|<cwith|1|-1|1|-1|cell-bsep|0fn>|<cwith|1|-1|1|-1|cell-tsep|0fn>|<cwith|1|-1|1|-1|cell-vcorrect|n>|<table|<row|<cell|<space|1fn|0ln|1ln>>>>>>>>

  <\active*>
    <\src-comment>
      Size tags (very-small: scriptsize, smaller: footnotesize, larger:
      Large, very-large: LARGE, really-huge: HUGE)
    </src-comment>
  </active*>

  <assign|really-tiny|<macro|x|<with|font-size|0.5|<arg|x>>>>

  <assign|very-tiny|<macro|x|<with|font-size|0.545|<arg|x>>>>

  <assign|tiny|<macro|x|<with|font-size|0.595|<arg|x>>>>

  <assign|really-small|<macro|x|<with|font-size|0.648|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|0.707|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-size|0.771|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|0.841|<arg|x>>>>

  <assign|flat-size|<macro|x|<with|font-size|0.917|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-size|1|<arg|x>>>>

  <assign|sharp-size|<macro|x|<with|font-size|1.091|<arg|x>>>>

  <assign|large|<macro|x|<with|font-size|1.189|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|1.297|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|1.414|<arg|x>>>>

  <assign|really-large|<macro|x|<with|font-size|1.542|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|1.682|<arg|x>>>>

  <assign|very-huge|<macro|x|<with|font-size|1.834|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|2|<arg|x>>>>

  <\active*>
    <\src-comment>
      Below follow some frequently used content tags.
    </src-comment>
  </active*>

  <assign|strong|<macro|x|<with|font-series|bold|math-font-series|bold|<arg|x>>>>

  <assign|em|<macro|x|<with|font-shape|<if|<equal|<value|font-shape>|italic>|right|italic>|<arg|x>>>>

  <assign|tt|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|name|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  <assign|samp|<macro|x|<with|font-family|ss|<arg|x>>>>

  <assign|abbr|<macro|x|<group|<arg|x>>>>

  <assign|math|<macro|x|<with|mode|math|<arg|x>>>>

  <assign|op|<macro|x|<with|math-condensed|true|<arg|x>>>>

  <assign|cite*|<macro|x|<with|font-shape|italic|<arg|x>>>>

  <assign|dfn|<macro|x|<with|font-shape|italic|<arg|x>>>>

  <assign|code*|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|kbd|<macro|x|<with|font-family|tt|<arg|x>>>>

  <assign|var|<macro|x|<with|font-family|tt|font-shape|italic|<arg|x>>>>

  <assign|acronym|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  <assign|person|<macro|x|<with|font-shape|small-caps|<arg|x>>>>

  <\active*>
    <\src-comment>
      Below follow some frequently used content environments.
    </src-comment>
  </active*>

  <assign|verbatim|<macro|body|<with|font-family|tt|language|verbatim|<arg|body>>>>

  <assign|code|<\macro|body>
    <\padded-normal|1fn|1fn>
      <surround||<htab|5mm>|<with|font-family|tt|language|verbatim|par-first|0fn|<arg|body>>>
    </padded-normal>
  </macro>>

  <assign|quote-env|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-both|3fn|3fn>
        <with|par-first|0fn|par-par-sep|0.25fn|<arg|body>>
      </indent-both>
    </padded-normal>
  </macro>>

  <assign|quotation|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-both|3fn|3fn>
        <surround|<yes-indent>||<arg|body>>
      </indent-both>
    </padded-normal>
  </macro>>

  <assign|verse|<\macro|body>
    <\padded-normal|0.5fn|0.5fn>
      <\indent-both|4.5fn|3fn>
        <with|par-first|-1.5fn|par-par-sep|0.fn|<surround|<yes-indent>||<arg|body>>>
      </indent-both>
    </padded-normal>
  </macro>>

  <assign|center|<macro|body|<with|par-mode|center|<arg|body>>>>

  <\active*>
    <\src-comment>
      The following environments complete the most basic tabular
      environments.
    </src-comment>
  </active*>

  <assign|tabular*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>>>>

  <assign|block|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|1|-1|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<arg|x>>>>

  <assign|block*|<macro|x|<tformat|<cwith|1|-1|1|-1|cell-rborder|1ln>|<cwith|1|-1|1|-1|cell-bborder|1ln>|<cwith|1|1|0|0|cell-tborder|1ln>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|1|-1|cell-halign|c>|<arg|x>>>>

  <\active*>
    <\src-comment>
      Other presentation tags.
    </src-comment>
  </active*>

  <assign|phantom|<macro|x|<if*|false|<arg|x>>>>

  <assign|overline|<macro|x|<eval|<quasiquote|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|color>>|<wide|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>>>

  <drd-props|overline|arity|1|accessible|all>

  <assign|underline|<macro|x|<eval|<quasiquote|<style-with|src-compact|none|<datoms|<macro|x|<with|color|<unquote|<value|color>>|<wide*|<arg|x>|\<wide-bar\>>>>|<arg|x>>>>>>>

  <drd-props|underline|arity|1|accessible|all>

  <\active*>
    <\src-comment>
      Tags for dynamic presentations.
    </src-comment>
  </active*>

  <assign|fold|<macro|x|y|<with|par-left|<plus|<value|par-left>|1.5fn>|<style-with|src-compact|none|<surround|<with|par-first|-1.5fn|<yes-indent>><action|<resize|<active*|<with|mode|math|<op|\<circ\>>>>|||r]1.5fn|>|(mouse-unfold)|<arg|x>>|<hflush>|<arg|x>>>>>>

  <assign|unfold|<\macro|x|y>
    <\with|par-left|<plus|<value|par-left>|1.5fn>>
      <style-with|src-compact|none|<surround|<with|par-first|-1.5fn|<yes-indent>><action|<resize|<active*|<with|mode|math|\<bullet\>>>|||r]1.5fn|>|(mouse-fold)|<arg|x>>|<hflush>|<arg|x>>>

      <surround||<right-flush>|<arg|y>>
    </with>
  </macro>>

  <assign|switch|<macro|x|y|<surround||<right-flush>|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>