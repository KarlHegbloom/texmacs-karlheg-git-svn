<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Formu�y matematyczne>

  Aby wpisywa� formu�y matematyczne, potrzeba wej�� w tryb matematyczny''.
  To specjalna w�a�ciwo�� tekstu wstawiana poprzez menu
  <menu|Wstaw|Matematyka>

  <\description>
    <item*|Wz�r <key|$>>�rodowisko u�ywane przy ma�ych matematycznych
    wstawkach wewn�trz normalnego akapitu.

    Wzory s� domy�lnie uk�adane tak aby zajmowa�y jak najmniejsz� wysoko��.
    Na przyk�ad granice s� wy�wietlane po lewej. To mo�na zmieni� ustawiaj�c
    <menu|Format|Display style|W��cz>.\ 

    <item*|R�wnanie <key|A-$>>�rodowisko dla wi�kszych wyra�e� matematycznych
    kt�re s� umieszczane w samodzielnych paragrafach.

    <item*|R�wnania <key|A-&>>Tworzy <markup|eqnarray*>, tr�jkolumnowe
    �rodowisko tablicopodobne (zobacz <hyper-link|tworzenie
    tablic|../table/man-create-table.pl.tm>).

    To �rodowisko jest wygodne do pokazywania przekszta�ce� r�wnania.
    Pierwsza kolumna zawiera lew� stron� relacji, druga znak relacji, trzecia
    praw� stron�.\ 
  </description>

  W trybie matematycznym, dost�pne s� specyficzne komendy i skr�ty klawiszowe
  do wpisywania matematycznych symboli i wzor�w. Na przyk�ad, prefiks
  <key|H-> mo�e by� u�yty do wprowadzania symboli greckich (zauwa� i�
  <key|H-> jest r�wnowa�ne <key|F5>, <key|escape escape escape> lub
  <key|A-C>).

  Odgadywanie przez program znaczenia b�dzie rozwijane w przysz�ych wersjach.
  Obecnie na przyk�ad wskazane jest zaznaczenie mno�enia <key|*> pomi�dzy
  symbolami <with|mode|math|a> i <with|mode|math|b>. Domy�lnie wpisanie
  <key|a b> wy�wietli <with|mode|math|ab>, a nie <with|mode|math|a*b>.

  <\traverse>
    <branch|Podstawowe konstrukcje matematyczne|keyboard/man-main.pl.tm>

    <branch|Symbole matematyczne|keyboard/man-symbols.pl.tm>

    <branch|Du�e operatory|keyboard/man-big.pl.tm>

    <branch|Du�e nawiasy|keyboard/man-large.pl.tm>

    <branch|Szerokie akcenty|keyboard/man-wide.pl.tm>
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