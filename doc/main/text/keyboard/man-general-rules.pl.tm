<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Og�lne zasady prefiksowania>

  Du�a liczba skr�t�w klawiszowych wymaga, aby w dzieli� je na grupy, celem
  �atwiejszego zapami�tania. Podstawow� zasad� jest i� skr�ty z tej samej
  kategorii maj� wsp�lny prefiks. G��wne takie prefiksy to:

  <\description>
    <item*|<key|C-<with|mode|math|x><em|>>>Skr�ty bazuj�ce na klawiszu
    Control najcz��ciej s� u�ywane dla polece� edycyjnych. Zale�� one od
    ustawienia opcji Wygl�d'' w <menu|Edytuj|Ustawienia>. Dla przyk�adu
    je�li u�ywasz wygl�du <name|Emacs> to skr�ty <key|C-<with|mode|math|x>>
    odpowiadaj� tym z <name|Emacs>, zatem <key|C-y> wkleja tekst z bufora.

    <item*|<key|A-<with|mode|math|x>>>Klawisz Alternate jest u�ywany przy
    poleceniach kt�re zale�� od trybu w jakim si� znajdujesz. Na przyk�ad
    <key|A-s> daje <strong|mocny> tekst w trybie tekstowym i pierwiastek
    kwadratowy <no-break><with|mode|math|<sqrt|>> w trybie matematycznym.
    <key|escape escape> jest r�wnowa�ne <key|A->.

    <item*|<key|M-<with|mode|math|x>>>Klawisz meta jest klawiszem dla polece�
    <TeXmacs> og�lnego przeznaczenia, wsp�lnych dla wszystkich tryb�w. Dla
    przyk�adu <key|M-!> tworzy etykiet�. Jest r�wnie� u�ywany dla dodatkowych
    polece� edytorskich, jak <key|M-w> dla kopiowania tekstu, je�li ustawiony
    jest wygl�d <name|Emacs>. <key|escape> jest r�wnowa�ny <key|M->.

    <item*|<key|H-<with|mode|math|x>>>Zdefiniowany przez u�ytkownika klawisz
    modyfikuj�cy jest u�ywany do tworzenia symboli specjalnych jak litery
    greckie w trybie matematycznym. Mo�na skonfigurowa� klawiatur� tak aby
    caps-lock pe�ni� funkcj� klawisza hyper. <key|F5> jest r�wnowa�ne
    <key|H->.
  </description>

  Konkretne klawisze kt�re mog� by� u�yte do otrzymania prefiks�w <key|M-> i
  <key|H-> mo�na <hyper-link|ustawi�|../../config/man-config-kbd-modkeys.pl.tm>
  poprzez <menu|Edit|Preferences>.

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
    <associate|language|polish>
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
    <associate|sfactor|4>
  </collection>
</initial>