<TeXmacs|1.0.5.3>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-keyboard|1.0>

    <\src-purpose>
      Macros for keyboard shortcuts in the <TeXmacs> documentation.
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|key|<macro|x|<active*|<move|<with|font-size|<times|<value|font-size>|0.92>|<block|<tformat|<cwith|1|1|1|1|cell-lborder|0.5ln>|<cwith|1|1|1|1|cell-rborder|0.5ln>|<cwith|1|1|1|1|cell-bborder|0.5ln>|<cwith|1|1|1|1|cell-tborder|0.5ln>|<cwith|1|1|1|1|cell-background|broken
  white>|<table|<row|<cell|<with|font-family|tt|<inactive*|<arg|x>>>>>>>>>||0.05fn>>>>

  \;

  <assign|kbd-gen|<macro|x|<key|M-<arg|x>>>>

  <assign|kbd-text|<macro|x|<key|A-<arg|x>>>>

  <assign|kbd-math|<macro|x|<key|A-<arg|x>>>>

  <assign|kbd-symb|<macro|x|<key|S-F5 <arg|x>>>>

  <assign|kbd-big|<macro|x|<key|S-F5 <arg|x>>>>

  <assign|kbd-large|<macro|x|<key|A-<arg|x>>>>

  <assign|kbd-ia|<macro|x|<kbd-gen|i <arg|x>>>>

  <assign|kbd-exec|<macro|x|<kbd-gen|e <arg|x>>>>

  <assign|kbd-table|<macro|x|<kbd-gen|t <arg|x>>>>

  \;

  <assign|key-shift|<macro|<localize|shift>>>

  <assign|key-caps-lock|<macro|<localize|caps-lock>>>

  <assign|key-control|<macro|<localize|control>>>

  <assign|key-alternate|<macro|<localize|alternate>>>

  <assign|key-meta|<macro|<localize|meta>>>

  <assign|key-hyper|<macro|<localize|hyper>>>

  <assign|key-windows|<macro|<localize|windows>>>

  <assign|key-escape|<macro|<localize|escape>>>

  <assign|key-variant|<macro|<localize|tab>>>

  <assign|key-tab|<macro|<localize|tab>>>

  <assign|key-return|<macro|<localize|return>>>

  <assign|key-backspace|<macro|<localize|backspace>>>

  <assign|key-delete|<macro|<localize|delete>>>

  <assign|key-left|<macro|<localize|left>>>

  <assign|key-right|<macro|<localize|right>>>

  <assign|key-up|<macro|<localize|up>>>

  <assign|key-down|<macro|<localize|down>>>

  <assign|key-home|<macro|<localize|home>>>

  <assign|key-end|<macro|<localize|end>>>

  <assign|key-pageup|<macro|<localize|pageup>>>

  <assign|key-pagedown|<macro|<localize|pagedown>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>