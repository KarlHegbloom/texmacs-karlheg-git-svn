<TeXmacs|1.0.3.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Neue Dateiformate und Konverter hinzuf�gen>

  Mit der <name|Guile/Scheme> Erweiterungssprache ist es m�glich, neue
  Dateiformate und Konverter in modularer Weise zu <TeXmacs> hinzuzuf�gen.
  �blicherweise sind die zus�tzlichen Formate und Konverter in Ihrer Datei
  <verbatim|~/.TeXmacs/progs/my-init-texmacs.scm> oder einem dedizierten
  Plug-In deklariert. Einige Beispiele sind unter
  <verbatim|$TEXMACS_PATH/progs/convert> zu finden, wie beispielsweise
  <hlink|init-html.scm|$TEXMACS_PATH/progs/convert/html/init-html.scm>.

  <paragraph|Deklaration neuer Formate>

  Ein neues Format wird mit folgendem Kommando deklariert

  <\scheme-fragment>
    (define-format <em|format>

    \ \ (:name <em|format-name>)

    \ \ <em|options>)
  </scheme-fragment>

  Hier ist <kbd|<em|format>> ein Symbol das f�r das Format steht und
  <em|<kbd|format-name>> ein Zeichenkette die in den Men�s benutzt werden
  kann. Tats�chlich besteht ein Dateiformat aus verschiedenen Varianten: ein
  Format <kbd|<em|format>-file> f�r Dateien, ein Format
  <kbd|<em|format>-document> f�r ganze Dokumente, ein Format
  <kbd|<em|format>-snipped> f�r Ausschnitte wie Selektionen und ein
  <kbd|<em|format>-object> f�r die bevorzugte Interne Repr�sentation des
  Schemas f�r �bersetzungen (d.h. die geparste Variante des Formats).
  Konverter von <kbd|<em|format>-file> zu <kbd|<em|format>-document> und
  umgekehrt werden automatisch zu Verf�gung gestellt.

  Sie k�nnen zus�tzliche Optionen f�r die automatische Erkennung von Formaten
  durch ihre Dateiendung und ihren Inhalt definieren. Die m�glichen Endungen
  f�r ein Format, mit der Standardendung zuerst aufgelistet, k�nnen wie folgt
  spezifiziert werden

  <\scheme-fragment>
    (:suffix <em|default-suffix> <em|other-suffix-1> ... <em|other-suffix-n>)
  </scheme-fragment>

  Eine (heuristische) Routine, die ermittelt ob das vorliegende Dokument dem
  Format entspricht, wird durch eine der Folgenden Anweisungen spezifiziert

  <\scheme-fragment>
    (:recognize <em|predicate>)

    (:must-recognize <em|predicate>)
  </scheme-fragment>

  Im ersten Fall bekommt die Erkennung �ber die Dateiendung Vorrang vor der
  Dokumenterkennung und im zweiten Fall erfolgt die heuristische Erkennung
  ausschlie�lich durch die Ermittlung des Inhalts.

  <paragraph|Deklaration neuer Konverter>

  Neue Konverter werden wie folgt deklariert

  <\scheme-fragment>
    (converter <em|from> <em|to>

    \ \ <em|options>)
  </scheme-fragment>

  Der tats�chliche Konverter wird �ber eine der folgenden Optionen
  spezifiziert

  <\scheme-fragment>
    (:function <em|converter>)

    (:function-with-options <em|converter-with-options>)

    (:shell <em|prog> <em|prog-pre-args> from <em|progs-infix-args> to
    <em|prog-post-args>)
  </scheme-fragment>

  Im ersten Fall ist der <kbd|<em|converter>> eine Routine, die ein Objekt
  aus dem <em|<kbd|from>> Format nimmt und im <kbd|<em|to>> Format zur�ck
  gibt. Im zweiten Fall benutzt der <em|<kbd|converter>> eine zus�tzliche
  Verbindungsliste und sein zweites Argument mit Optionen f�r den Konverter.
  Im letzten Fall wird ein Shell-Kommando spezifiziert um zwischen zwei
  Dateiformaten zu konvertieren. Der Konverter wird nur dann aktiviert, wenn
  <em|<kbd|prog>> tats�chlich in dem Verzeichnis gefunden wird. Hilfsdateien
  werden ggf. automatisch erstellt und gel�scht.

  Wenn Sie einen Konverter von <em|x> nach <em|y> und von <em|y> nach <em|z>
  haben, bekommen Sie von <TeXmacs>automatisch einen weiteren Konverter von
  <em|x> nach <em|z>. Eine Distanz zwischen zwei Formaten �ber einen
  Konverter`` kann durch

  <\scheme-fragment>
    (:penalty <em|floating-point-distance>)
  </scheme-fragment>

  spezifiziert werden. Weitere Optionen sind:

  <\scheme-fragment>
    (:require <em|cond>)

    (:option <em|option> <em|default-value>)
  </scheme-fragment>

  Die erste Option spezifiziert eine Bedingung die erf�llt werden muss damit
  der Konverter benutzt wird. Diese Option sollte als erste oder zweite
  Option und st�ndig hinter der <kbd|:penalty> Option angegeben werden. Die
  <kbd|:option> Option spezifiziert eine Option f�r den Konverter durch seine
  default value. Diese Option wird automatisch eine Benutzereinstellung und
  wird an alle Konverter mit Optionen weitergegeben.

  <tmdoc-copyright|1998-2004|Joris van der Hoeven, Christoph Strobel>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|page-type|a4>
    <associate|page-top|30mm>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|german>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>