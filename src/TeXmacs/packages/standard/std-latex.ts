<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|std-latex|1.0>

    <\src-purpose>
      Macros which make it easier to port TeX/LaTeX style files to TeXmacs
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|par-hyphen|professional>

  <active*|<\src-comment>
    Default values of TeX page layout parameters.
  </src-comment>>

  <assign|tex-odd-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-even-side-margin|<macro|<if|<equal|<value|par-columns>|1>|0pt|-30pt>>>

  <assign|tex-text-width|<macro|<if|<equal|<value|par-columns>|1>|25.5cc|17.5cm>>>

  \;

  <assign|tex-top-margin|<macro|-10pt>>

  <assign|tex-head-height|<macro|12pt>>

  <assign|tex-head-sep|<macro|16.74pt>>

  <assign|tex-top-skip|<macro|10pt>>

  <assign|tex-text-height|<macro|<if|<equal|<value|par-columns>|1>|517.5dd|640dd>>>

  <assign|tex-foot-height-heuristic|<macro|1quad>>

  <assign|tex-foot-skip|<macro|30pt>>

  \;

  <assign|tex-footnote-sep|<macro|8pt>>

  <assign|tex-footnote-tm-barlen|<macro|0.4par>>

  <assign|tex-column-sep|<macro|1.5cc>>

  <assign|tex-float-sep|<macro|<tmlen|10pt|12pt|14pt>>>

  <assign|tex-margin-par-width|<macro|48pt>>

  <assign|tex-margin-par-sep|<macro|10pt>>

  <active*|<\src-comment>
    Computing the page layout.
  </src-comment>>

  <assign|page-width-margin|tex>

  <assign|page-odd|<macro|<plus|<tex-odd-side-margin>|1in>>>

  <assign|page-even|<macro|<plus|<tex-even-side-margin>|1in>>>

  <assign|par-width|<macro|<tex-text-width>>>

  \;

  <assign|page-height-margin|tex>

  <assign|page-head-sep|<macro|<tex-head-sep>>>

  <assign|page-top|<style-with|src-compact|none|<macro|<minus|1in|<tex-top-margin>|<tex-head-height>|<page-head-sep>|<tex-top-skip>|0.75quad>>>>

  <assign|page-user-height|<macro|<tex-text-height>>>

  <assign|page-foot-sep|<macro|<minus|<tex-foot-skip>|<tex-foot-height-heuristic>>>>

  \;

  <assign|page-fnote-sep|<macro|<tex-footnote-sep>>>

  <assign|page-fnote-barlen|<macro|<tex-footnote-tm-barlen>>>

  <assign|par-columns-sep|<macro|<tex-column-sep>>>

  <assign|page-float-sep|<style-with|src-compact|all|><macro|<tex-float-sep>>>

  <assign|page-mnote-width|<macro|<tex-margin-par-width>>>

  <assign|page-mnote-sep|<macro|<tex-margin-par-sep>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>