<TeXmacs|1.0.7.20>

<style|tmdoc>

<\body>
  <tmdoc-title|The standard <TeXmacs> styles>

  The user may select a major style from the <menu|Document|Style> menu. The
  major style usually reflects the kind of document you want to produce (like
  a letter, an article or a book) or a particular layout policy (like
  publishing an article in a given journal).

  The user may further customize the main style, by selecting one or more
  additional style packages. Some of these packages are available in the menu
  <menu|Document|Style|Add package>. Other style packages mainly customize
  specific tags, and they can be selected from the
  <menu|Focus|Preferences|Style options> menu group, whenever available. For
  instance, inside a theorem, you may use <menu|Focus|Preferences|European
  numbering> to enable European style numbering for theorem-like environments
  (that is, theorems, propositions, lemmas, <abbr|etc.> are all numbered
  using their own individual counters).

  In this chapter, we will survey the standard document styles and packages
  provided by <TeXmacs>. Most style files and packages have an abstract
  interface, the <abbr|d.t.d.> (data domain definition), which specifies
  which macros are exported by the style or package, and how to use them.
  Distinct styles or packages (like <tmpackage|header-article> and
  <tmpackage|header-book>) may share the same abstract interface, but differ
  in the way macros are rendered. For this reason, we will mainly be
  concerned with the description of the standard <abbr|d.t.d.>s, except when
  we focus on the rendering. Users may customize standard styles by defining
  new ones which match the same abstract interface (see the chapter on
  <hlink|writing <TeXmacs> style files|../../devel/style/style.en.tm>).

  <\traverse>
    <branch|General organization|style-organize.en.tm>

    <branch|The common base for most styles|std/std-dtd.en.tm>

    <branch|Standard environments|env/env-dtd.en.tm>

    <branch|Headers|header/header-dtd.en.tm>

    <branch|Sections|section/section-base-dtd.en.tm>
  </traverse>

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>