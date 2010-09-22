<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Ustawienia klawiszy modyfikuj�cych>

  <TeXmacs> korzysta z pi�ciu klawiszy modyfikuj�cych: <key|S->,
  <key|C->, <key|A->, <key|M-> i
  <key|H->, skracanych do <key|S->, <key|C->, <key|A->, <key|M-> i
  <key|H->. Klawisze <key|shift> i <key|control> s� obecne na praktycznie
  ka�dej klawiaturze, a <key|alternate> jest na prawie wszystkich. Wi�kszo��
  wsp�czesnych klawiatur posiada klawisz <key|windows>, kt�ry jest zwykle
  r�wnowa�ny klawiszowi <key|meta> dla <TeXmacs>.

  Przed zmian� ustawie� klawiatury nale�y najpierw sprawdzi� czy jest to
  rzeczywi�cie potrzebne. Je�li dost�pne s� klawisze w wygodny spos�b
  pasuj�ce do <key|S->, <key|C->, <key|A-> i
  <key|M->, to nie trzeba nic robi�. Prawdopodobny wyj�tek to
  sytuacja gdy potrzeba prostego klawisza jak <key|caps-lock> do wpisywania
  symboli matematycznych. W takim przypadku nale�y odwzorowa� <key|caps-lock>
  na <key|hyper>.

  Niestety w X Windows konfiguracja obejmuje od razu ca�y system. Zatem je�li
  nast�pi przedefiniowanie klawisza <key|<key-caps-lock>> wewn�trz <TeXmacs>
  to jego nowe zachowanie b�dzie r�wnie� w innych aplikacjach. Czyli powinno
  si� przestawia� tylko klawisze dla kt�re nie s� u�ywane przez inne
  programy. Dla przyk�adu klawisz <key|<key-windows>> jest u�ywany przez
  niewiele apliakcji, zatem jego przedefiniowanie nie powinno wyrz�dzi�
  szkody. Wygodniej mo�e by� okre�li� odpowiednio konfiguracj� ca�ego
  systemu. To mo�na zrobi� przy u�yciu polecenia <verbatim|xmodmap>;
  dok�adniejsze informacje w jego dokumentacji.

  W niekt�rych wypadkach, na klawiaturze s� klawisze odpowiadaj�ce
  <key|A->, <key|M-> i <key|H->, jednak
  ustawione inaczej ni� pasuje u�ytkownikowi. Mo�na zmieni� przypisanie
  prefiks�w <key|A->, <key|M->, i <key|H-> do modyfikator�w poprzez menu
  <menu|Edit|Preferences|Keyboard>.

  Na przyk�ad, aby zachowa� kompatybilno�� z Emacsem mo�na spermutowa�
  klawisz <key|M-> lub <key|<key-windows>> z <key|A->
  bez zmian dla innych cz��ci systemu. Nale�y znale�� kt�re modyfikatory s�
  powi�zane z tymi klawiszami; zwykle b�dzie to <key|Mod1> dla
  <key|A-> i <key|Mod4> dla <key|M-> lub
  <key|<key-windows>>. Nast�pnie zadan� permutacj� ustawia si� wybieraj�c
  <menu|A modifier|R�wnowa�ny Modulo4> i <menu|M modifier|R�wnowa�ny Modulo1>
  w menu <menu|Edit|Preferences|Keyboard>.

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