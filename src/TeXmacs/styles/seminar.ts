<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|seminar|1.0>

      <\src-purpose>
        The seminar style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This <TeXmacs> style file falls under the <hlink|GNU general public
        license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
        WHATSOEVER. If you do not have a copy of the license, then write to
        the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
        Boston, MA 02111-1307, USA.
      </src-license>
    </src-title>
  </active*>

  <use-package|common-base|header-seminar|env-us|number-trivial|section-seminar>

  \;

  <assign|magnification|2>

  <assign|par-first|0fn>

  <assign|page-odd|15mm>

  <assign|page-even|15mm>

  <assign|par-width|180mm>

  <assign|page-top|15mm>

  <assign|page-bot|20mm>

  <assign|page-reduce-left|0mm>

  <assign|page-reduce-right|0mm>

  <assign|page-reduce-top|0mm>

  <assign|page-reduce-bot|0mm>

  \;

  <assign|em|<macro|x|<with|color|blue|<arg|x>>>>

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