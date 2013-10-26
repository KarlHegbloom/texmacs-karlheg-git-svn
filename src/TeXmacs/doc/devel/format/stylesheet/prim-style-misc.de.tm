<TeXmacs|1.0.4.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Sonstige Stil-Konstrukte>

  <\explain>
    <explain-macro|extern|scheme-foo|arg-1|<with|mode|math|\<cdots\>>|arg-n><explain-synopsis|<value|scheme>-Makros
    verwenden>
  <|explain>
    Dieses Konstrukt dient zur Implementierung von Makros, die in
    <value|scheme> geschrieben sind. Die <value|scheme>-Funktion
    <src-arg|scheme-foo> bzw. das <value|scheme>-Makro <src-arg|scheme-foo>
    wird auf die Argumente <src-arg|arg-1> bis <src-arg|arg-n> angewandt. Der
    Code <inactive*|<extern|(lambda (name) `(concat "Hallo " ,name))|Emil>>
    liefert beispielsweise \R<extern|(lambda (name) `(concat "Hallo "
    ,name))|Emil>''.

    Die Argumente <src-arg|arg-1> bis <src-arg|arg-n> werden evaluiert und
    dann als B�ume an <src-arg|scheme-foo> �bergeben. Wenn man ein Makro
    schreibt, dass externen <value|scheme>-Code benutzt, sollte man also die
    Argumente unter Benutzung des <markup|quote-arg> Konstrukts �bergeben:

    <\tm-fragment>
      <inactive*|<assign|inc-div|<macro|x|y|<style-with|src-compact|none|<extern|(lambda
      (x y) `(frac ,x (concat "1+" ,y)))|<quote-arg|x>|<quote-arg|y>>>>>>
    </tm-fragment>

    Es war an sich vorgesehen, dass die Erreichbarkeit von Makro-Argumenten
    auch hier erhalten bleibt. Da aber <TeXmacs> <value|scheme>-Code nicht
    heuristisch untersucht, muss man die <abbr|D.R.D.>-Eigenschaften mit
    <markup|drd-props> von Hand setzen.

    Man beachte ferner, dass die <value|scheme>-Funktion <src-arg|scheme-foo>
    nur sichere <value|scheme>-Funktionen verwendet werden und nicht etwa
    solche, die Ihre Festplatte l�schen. <value|scheme>-Funktionen in
    Plugins, die ein Anwender implementiert hat, k�nnen mit der Option
    <verbatim|:secure> als sicher definiert werden. Es wird dann
    <inactive|>auf eine R�ckfrage verzichtet. Alternativ kann man nat�rlich
    auch alle <value|scheme>-Funktionen im Men�
    <menu|Edit|Preferences|Security|Accept all scripts> als sicher
    akzeptieren.
  </explain>

  <\explain>
    <explain-macro|write|aux|content><explain-synopsis|Zus�tzlich
    Informationen zu Quellcode>
  <|explain>
    Diese verborgenen Zusatzinformationen werden nur im Quell-Modus
    dargestellt.
  </explain>

  <\explain>
    <explain-macro|flag|content|color>

    <explain-macro|flag|content|color|var><explain-synopsis|informatorische
    Flags>
  <|explain>
    Diese Konstrukte dienen dazu, dem Anwender sichtbare Informationen zu
    geben, die nicht ausgedruckt werden sollen. <TeXmacs> zeigt solche
    informatorischen Flags f�r Label, Formatierbefehle, wie Seitenumbr�che
    usw.. Im Men� <menu|Document|Informative flags> kann der Anwender
    einstellen, wie solche Flags dargestellt werden sollen.

    Die Variante mit zwei Argumenten gibt ein informatives Flag mit einem
    spezifischen Inhalt <src-arg|content> und Farbe <src-arg|color>. Der
    <src-arg|content> wird nur gezeigt, wenn die Darstellungsweise von
    informativen Flags auf <translate|detailed|english|german> eingestellt
    ist, z.B. im Men� <menu|Document|Informative flags|Detailed>.
    Beispielsweise wird <inactive*|<flag|warning|red>> im Text in der
    Voreinstellung als <flag|warning|red> \ 

    und mit Einstellung \R<translate|detailed|english|german>`` so
    <with|info-flag|detailed|<flag|warning|red>> dargestellt. Das optionale
    Argument <src-arg|var> dient zur Steuerung der Sichtbarkeit. Wenn
    <src-arg|var> zu einem erreichbaren Dokumentteil geh�rt, dann wird es
    dargestellt, sonst nicht. <TeXmacs> generiert automatisch
    Abschnitts-Labels, damit sie in das Inhaltsverzeichnis aufgenommen werden
    k�nnen. Es ist aber unsch�n, informatorische Flags in diesen F�llen zu
    zeigen.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

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