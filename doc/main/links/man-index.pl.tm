<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Tworzenie indeksu>

  Dla stworzenia indeksu nale�y najpierw wstawi� has�a w dokument u�ywaj�c
  <menu|Insert|Link|Index entry>. Po wybraniu miejsca gdzie indeks ma zosta�
  umieszczony nale�y u�y� <menu|Text|Automatic|Index>. Indeks jest tworzony w
  podobny spos�b jak spis tre�ci.

  W menu <menu|Insert|Link|Index entry> jest kilka rodzaj�w pozycji indeksu.
  Najprostsze to g��wne'', podrz�dne'', pod-podrz�dne'' kt�re s� makrami z
  jednym, dwoma lub trzema argumentami odpowiednio. Wpisy typu podrz�dnego''
  i pod-podrz�dnego'' s� u�ywane aby ustawi� hierarchi� pozycji indeksu.

  Z�o�one has�o indeksu przyjmuje cztery argumenty. Pierwszy to klucz pod
  jakim jest przechowywane i musi to by� tuple'' (tworzone przez <key|M-i
  \<less\>>) kt�rego pierwszym sk�adnikiem jest g��wna kategoria, drugim
  podkategoria itd. Drugi argument z�o�onego has�a jest albo pusty lub
  strong'', w przypadku kt�rego numer strony zostanie napisany t�ust�
  czcionk�. Trzeci argument jest zwykle pusty, ale po stworzeniu dw�ch hase�
  z tym samym nie pustym trzecim argumentem powstanie has�o z zakresem''
  stron. Czwartym argumentem, kt�rym znowu jest tuple'', jest has�o.

  Jest r�wnie� mo�liwe stworzenie has�a indeksu bez numeru strony przy u�yciu
  W miejsce'' w <menu|Insert|Link|Index entry>. Pierwszym argumentem jest
  klucz do sortowania indeksu. Drugi argument zawiera w�a�ciwy tekst. Ta
  konstrukcja mo�e by� przydatna do tworzenia r�nych sekcji A'', B'' itp.
  w indeksie.

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