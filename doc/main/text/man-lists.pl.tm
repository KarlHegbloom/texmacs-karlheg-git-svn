<TeXmacs|1.0.3.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Listy>

  U�ywaj�c <menu|Tekst|Wyszczeg�lnienie> mo�na stworzy� nienumerowan� list�.
  Mo�esz albo wybra� konkretny znacznik jak <with|mode|math|\<bullet\>>
  (kropki), <with|mode|math|<op|->> (kreski) lub
  <with|mode|math|<op|\<rightarrow\>>> (strza�ki) do wyr�niania pozycji w
  li�cie, lub domy�lny. Listy mog� by� zagnie�d�one jak na przyk�adzie:

  <\itemize>
    <item>Pierwszy element.

    <item>A teraz podlista:

    <\itemize>
      <item>Podelement.

      <item>Jaki� kolejny.
    </itemize>

    <item>Ko�cowy element.
  </itemize>

  Domy�lny znacznik jest wy�wietlany r�nie, zale�nie od poziomu
  zagnie�d�enia. Na najbardziej zewn�trznym poziomie to
  <with|mode|math|\<bullet\>>, drugi poziom <with|mode|math|<op|\<circ\>>>, i
  tak dalej. Wewn�trz listy, wci�ni�cie <key|enter> automatycznie startuje
  nowy element. Je�li potrzeba element�w kt�re zajmuj� par� paragraf�w to
  mo�na zawsze u�y� <key|S-enter> do zacz�cia nowego paragrafu.

  Wyliczane �rodowisko, tworzone przez <menu|Tekst|Wyliczenie>, zachowuje si�
  podobnie jak punktowane, tylko �e elementy s� numerowane. Poni�ej przyk�ad
  na wyliczenie wywo�ane przez <menu|><menu|Tekst|Wyliczenie|I, II, III, ...>

  <\enumerate-Roman>
    <item>Pierwszy element.

    <item>Drugi.

    <item>Ostatni.
  </enumerate-Roman>

  Oczywi�cie mo�liwe jest zagnie�d�anie list.

  <\enumerate>
    <item>Podpunkty: <key|\\ e n u m e r a t e>

    <\enumerate>
      <item>podpunkt
    </enumerate>

    <item>Podpunkty: <key|\\ e n u m e r a t e *>

    <\enumerate*>
      <item>podpunkt
    </enumerate*>
  </enumerate>

  Ostatnim rodzajem jest lista opisowa. Wywo�ywana przez <menu|Tekst|Opis>.
  Tutaj wyr�nik jest indywidualny dla elementu.

  <\description>
    <item*|Pingwin>Mi�e stworzenie, cho� lubi ryby.

    <item*|Stru�>Wiecznie uciekaj�cy posi�ek Kojota.
  </description>

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