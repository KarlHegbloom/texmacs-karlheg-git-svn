<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Allgemeine Regeln f�r Tastatur-Kurzbefehle>

  Weil es eine gro�e Anzahl von Tastatur-Kurzbefehlen in <TeXmacs> gibt,
  existieren allgemeine Klassifizierungs-Regeln, die das Erlernen und
  Behalten dieser Befehle erleichtern sollen. Generell beginnen
  Tastaturbefehle, die zu einer bestimmten Kategorie geh�ren, mit der
  gleichen Modifiziertaste. Die wichtigsten dieser Tasten sind die folgenden:\ 

  <\description>
    <item*|<key|C-<with|mode|math|x>>>Dies ist die Steuerungstaste
    <key|<with|mode|math|Strg>> auf manchen (englischen) Tastaturen auch mit
    <key|<with|mode|math|Ctrl>> beschriftet. Kurzbefehle auf Basis der
    Steuerungstaste werden f�r h�ufig benutzte Editorbefehle benutzt. Es
    h�ngt von den Einstellungen im Men� <menu|Edit|Preferences|Look and feel>
    ab, welche Kurzbefehle in einzelnen zur Verf�gung stehen. Wenn Sie
    beispielsweise <em|Emacs> gew�hlt haben, dann entsprechen Kurzbefehle der
    Form <key|C-<with|mode|math|x>> <name|Emacs>-Kurzbefehlen, z.B. <key|C-y>
    dem Befehl <em|Text einf�gen>.

    <item*|<key|A-<with|mode|math|x>>>Die <key|Alt>-Taste wird f�r Befehle
    verwendet, die davon abh�ngen, in welchem Modus Sie sich gerade befinden.
    Beispielsweise erzeugt <kbd-text|s> <strong|fetten> Text im Textmodus
    aber das Quadratwurzel-Zeichen im Mathematik-Modus. Beachten Sie bitte,
    dass zweimaliges Tippen der <key|Esc>-Taste, also <key|<key-escape>
    <key-escape>> die gleiche Funktion wie <key|A-> hat.

    <item*|<key|M-<with|mode|math|x>>>Die Meta-Taste wird f�r allgemeine
    <TeXmacs>-Befehle genutzt, die in allen m�glichen Moden verwendet werden
    k�nnen. Beispielsweise erzeugt <kbd-gen|!> ein Kennzeichen (label). Diese
    Taste wird auch f�r zus�tzliche Editierbefehle genutzt, wenn die
    Einstellungen in <menu|Edit|Preferences|Look and feel> dies vorsehen.
    Beachten Sie bitte, dass einmaliges Tippen der <key|Esc>-Taste, also
    <key|<key-escape>>, \ gleichbedeutend ist mit <key|M->.

    <item*|<key|H-<with|mode|math|x>>>Diese Modifiziertaste wird zur
    Erzeugung spezieller Symbole, wie z.B. griechischer Buchstaben, benutzt.
    Sie k�nnen Ihre Tastatur so konfigurieren, dass die Hochstelltaste die
    Rolle der Hypertaste spielt (<menu|Edit|Preferences|Keyboard>). Die Taste
    <key|F5> ist der Taste \ <key|H-> �quivalent.
  </description>

  Es sei daran erinnert, dass die Modifiziertasten, die zur Erzeugung von
  <key|M-> und <key|H-> benutzt werden, im Men�
  <menu|Edit|Preferences|Keyboard> <hyper-link|konfiguriert|../../config/man-config-kbd-modkeys.en.tm>
  werden k�nnen.

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
    <associate|language|german>
    <associate|preamble|false>
  </collection>
</initial>