<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Konfiguration der Tastatur-Modifier >

  <TeXmacs> benutzt f�nf Tastatur-Modifier: \ <key|<key-shift>>,
  \ \ <key|strg>, \ <key|alt>, \ <key|meta> und \ <key|hyper> die als
  \ <key|S->, \ <key|C->, <key|A->, <key|M-> und \ <key|H-> abgek�rzt werden.
  Die Tasten \ <key|<key-shift>> und \ <key|strg> sind normalerweise \ auf
  praktisch allen Tastaturen vorhanden, die \ <key|alt> Taste auf den
  Meisten. Neuere Tastaturen f�r PC's haben �blicherweise noch die
  \ <key|windows> taste, diese ist in <TeXmacs> \ �quivalent zu der Taste
  <key|meta>.

  Bevor Sie die Konfiguration Ihrer Tastatur ver�ndern, sollten Sie pr�fen ob
  das auch wirklich n�tig ist. Wenn Sie Tasten entsprechend zu \ <key|shift>,
  \ <key|strg>, \ <key|alt> und \ <key|meta> auf Ihrer Tastatur haben,
  sollten Sie nichts �ndern. M�chten Sie jedoch einen einfachen Tastendruck
  wie <key|feststellen> verwenden um beispielsweise mathematische Symbole zu
  schreiben, k�nnen Sie es nat�rlich trotzdem tun. In diesem Fall sollten Sie
  <key|hyper> der Taste <key|feststellen> zuweisen.

  Um die Konfiguration der Tastatur zu ver�ndern, w�hlen Sie einfach einen
  der Tastatur-Modifier �ber <specific|texmacs|><menu|Edit|Preferences|Keyboard>,
  den Sie einer vorhandenen Taste zuweisen m�chten. Wenn Sie beispielsweise
  <menu|Windows key|Legen auf M Modifier> w�hlen, wird die <key|windows>
  Taste dem <key|meta> Modifier zugewiesen. Ebenso wird <key|feststellen> dem
  <key|hyper> Modifier zugewiesen, wenn Sie <menu|Caps-lock key|Legen auf H
  Modifier> aus dem Men� w�hlen.

  Ungl�cklicherweise erlaubt X-Window nur eine systemweite Konfiguration.
  Deshalb hat es Auswirkungen auf alle anderen Anwendungen wenn Sie in
  <TeXmacs> die Konfiguration der <key|feststellen> Taste ver�ndern da
  <key|feststellen> dann dort ebenfalls die neue Funktion hat. Aus diesem
  Grund ist es wichtig, nur die Tasten zu �ndern, die nicht f�r etwas Anderes
  in den �brigen Anwendungen ben�tigt werden. Die <key|windows> Taste
  beispielsweise wird nicht von vielen anderen Applikationen benutzt, deshalb
  ist es normalerweise ungef�hrlich die Konfiguration f�r diese Taste zu
  ver�ndern. M�glicherweise bevorzugen Sie es eine entsprechende systemweite
  Rekonfiguration vorzunehmen. Dies k�nnen Sie mit dem Kommando <kbd|xmodmap>
  erreichen; lesen Sie hierzu die zugeh�rige Manual-Page f�r weitere
  Informationen.

  In manchen F�llen werden Sie schon die entsprechenden Tasten zu <key|alt>,
  <key|meta> und <key|hyper> auf Ihrer Tastatur haben, was aber eventuell
  nicht Ihren Vorstellungen entsprechen k�nnte. Um dies zu �ndern k�nnen Sie
  die <key|A->, <key|M-> und <key|H-> Pr�fixe �ber die erste Gruppe von
  Untermen�s in <menu|Edit|Preferences|Keyboard> anderen Modifiern zuweisen.

  F�r die Kompatibilit�t zu Emacs m�chten Sie vielleicht die <key|meta> oder
  <key|windows> Tasten mit <key|alt> vertauschen ohne systemweite �nderungen
  hervorzurufen. Um dies zu erreichen, m�ssen Sie herausfinden welche
  Modifier diesen Tasten entsprechen, normalerweise ist das <key|Mod1> f�r
  <key|alt> und <key|Mod4> f�r <key|meta> oder <key|windows>. Das Vertauschen
  wird schlie�lich �ber <menu|Edit|Preferences|Keyboard>, durch ausw�hlen von
  <menu|A modifier|�quivalent f�r Mod4> und <menu|M Modifier|�quivalent f�r
  Mod1> durchgef�hrt.

  <tmdoc-copyright|1998-2003|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|german>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>