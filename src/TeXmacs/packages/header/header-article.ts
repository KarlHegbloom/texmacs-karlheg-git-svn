<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|header-article|1.0|header|1.0>

    <\src-purpose>
      Headers for articles.
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

  <use-package|title-generic>

  \;

  <assign|start-page|<macro|s|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<htab|5mm><quote|<page-the-page>><htab|5mm>>>>>

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><with|font-shape|small-caps|<arg|s>><htab|5mm><quote|<page-the-page>>>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<with|font-size|0.84|<style-with|src-compact|none|<no-indent><quote|<page-the-page>><htab|5mm><with|font-shape|small-caps|<arg|s>>>>>>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|<style-with|src-compact|none|<odd-page-text|<arg|name>><even-page-text|<localize|Section>
  <arg|nr>>>>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>