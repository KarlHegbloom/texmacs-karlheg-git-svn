<TeXmacs|1.0.4.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Mutatoren>

  Ein Mutator hat die Form <explain-macro|mutator|body|cmd>, worin
  <src-arg|body> der sichtbare Rumpf des Mutators ist und <src-arg|cmd> ein
  sicheres <value|scheme>-Skript, das periodisch aufgerufen wird und das
  <src-arg|body> darf (im �brigen auch jeden anderen Teil des Dokuments).
  W�hrend Ausf�hrung von <src-arg|cmd> gibt <verbatim|mutator-path> die
  Position von <src-arg|body> im Baum des Dokuments an.

  Mutatoren sind eine besondere Eigenschaft von <TeXmacs>. Mit ihnen kann man
  \ interaktive Dokumente erzeugen. Beispielsweise wird innerhalb von
  Sitzungen mit anderen Anwendungen die Ausgabe der Anwendung aus einem
  Mutator entnommen, der automatisch am Ende der Ausgabe entfernt wird. In
  Zukunft k�nnten Mutatoren z.B. f�r Schnittstellen zu Rechtschreibpr�fungen
  eingesetzt werden.

  <\remark>
    Mutatoren arbeiten nur dann richtig, wenn sie explizit im Dokument
    auftreten. Sie funktionieren nicht, wenn sie als Teil eines Makros
    auftreten, selbst dann, wenn der Rumpf des Mutators erreichbar ist.

    Die derzeitige Implementierung in <TeXmacs> �berpr�ft in allen geladenen
    Dokumenten, wenn kurzzeitig keine Aktivit�t vorhanden ist, ob Mutatoren
    in ihnen enthalten sind. Dokument, die bekannterma�en keine Mutatoren
    enthalten, werden ignoriert. Diese Implementierung ist zugleich effizient
    und inkompatibel mit dem Makrosystem. Da gibt es Spielraum f�r
    Verbesserungen.
  </remark>

  <\remark>
    Aus Gr�nden der Effizienz sollten, au�er in ganz besonderen F�llen,
    Mutatoren nur eingesetzt werden, um ihren eigenen Rumpf zu ver�ndern und
    nicht um andere Teile eines Dokuments zu �ndern.
  </remark>

  <paragraph| Das <verbatim|mutator> plugin>

  Ein einfaches Beispiel mit zwei verschiedenen Arten von Mutatoren ist das
  <verbatim|mutator> Plugin. Es erzeugt zwei Kurzbefehle <key|C-F11> und
  <key|C-F12>, die die aktuelle Zeit bzw. einen blinkenden Text in das
  Dokument einf�gen. Es besteht aus der Datei

  <\verbatim>
    \ \ \ \ <example-plugin-link|mutator/progs/init-mutator.scm>
  </verbatim>

  Der Kurzbefehl <key|C-F11> f�gt die Zeichenkette
  <inactive*|<mutator|text|(mutate-date)>> in den Haupt-Text:

  <\cpp-fragment>
    (kbd-map ("C-F11" (insert '(mutator "" "(mutate-date)"))))
  </cpp-fragment>

  Der \Rsichere'' <value|scheme>-Code f�r <verbatim|mutate-date> ist der
  folgende:\ 

  <\scheme-fragment>
    (tm-define (mutate-date)

    \ \ (:secure #t)

    \ \ (let* ((p (the-mutator-path))

    \ \ \ \ \ \ \ \ \ (date (var-eval-system "date +\\"%H:%M:%S\\"")))

    \ \ \ \ (tm-assign-diff p date)))
  </scheme-fragment>

  Der Befehl <verbatim|tm-assign-diff> ist besonders geeignet, da er nur dann
  �nderungen durchf�hrt, wenn sich etwas ge�ndert hat.

  Die Einf�gung von blinkendem Text ist etwas komplexer, denn es muss den
  aktuellen Inhalt des Mutator-Befehl ber�cksichtigen. Der <key|C-F12> f�gt
  <inactive*|<mutator|text|(mutate-blink)>> in den Haupt-Text ein und
  versetzt den Cursor hinter den Text in den Rumpf des Mutators:\ 

  <\cpp-fragment>
    (kbd-map ("C-F12" (insert-go-to '(mutator "text" "(mutate-blink)")
    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '(0
    4))))
  </cpp-fragment>

  Dabei ist die \ \Rsichere'' <value|scheme>-Routine <verbatim|mutate-blink>
  so definiert:

  <\scheme-fragment>
    (tm-define (mutate-blink)

    \ \ (:secure #t)

    \ \ (let* ((mod (lambda (x y) (* y (- (/ x y) (floor (/ x y))))))

    \ \ \ \ \ \ \ \ \ (p (the-mutator-path))

    \ \ \ \ \ \ \ \ \ (t (tm-subtree p))

    \ \ \ \ \ \ \ \ \ (s (string-\<gtr\>number (var-eval-system "date
    +\\"%S\\"")))

    \ \ \ \ \ \ \ \ \ (e (mod s 4)))

    \ \ \ \ (if (and (\<less\>= e 1) (not (match? t '(strong :1))))

    \ \ \ \ \ \ \ \ (tm-ins-unary p 'strong))

    \ \ \ \ (if (and (\<gtr\>= e 2) (match? t '(strong :1)))

    \ \ \ \ \ \ \ \ (tm-rem-unary p))))
  </scheme-fragment>

  <\remark>
    Beachten Sie das die obigen Beispiele nur zur Veranschaulichung gedacht
    sind. Man sollte Daten,und blinkenden Text nicht mit Mutatoren
    programmieren, da jedes mal das Dokument ge�ndert wird. Das f�hrt zu
    Problemen, denken Sie nur an die Schwierigkeiten, die sich f�r die
    Wiederherstellung nach �nderungen ergeben. Wir \ planen die Entwicklung
    von Konstrukten f�r automatische Animationen, Filme und blinkendem Text,
    in denen der Inhalt gleich bleibt und nur die Darstellung ge�ndert wird.
  </remark>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

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
  </collection>
</initial>