<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Portar <TeXmacs> a otras plataformas>

  Teniendo s�lo acceso a sistemas PC/Linux y SUN, estoy interesado en gente
  que quiera portar <apply|TeXmacs> a otros sistemas Unix con X Window y
  mantener las correspondientes distribuciones. Si usted quiere hacer esto,
  debe tomar un vistaso a los archivos

  <\verbatim>
    \ \ \ \ configure.in<format|next line> \ \ \ src/Basic/fast_alloc.cpp
  </verbatim>

  Especialistas en <verbatim|autoconf>, redhat y paquetes rpm son tambi�n
  bienvenidos a comunicar sus sugerencias, parches, etc.

  Adem�s de portar a <apply|TeXmacs> a otros sistemas basados en Unix, ser�a
  agradable portar <apply|TeXmacs> a Windows (y Mac OC). Por favor �nase a la
  lista de correo <verbatim|texmacs-dev@gnu.org> si usted quiere ayudar. Han
  habido discusiones sobre como hacer el puerto y en particular sobre cual
  interface gr�fica de usuario portable (como Gtk, Qt, Wxwindows o GNUstep)
  debemos usar. Nuestra estrat�gia ser� primer poner todo el c�digo
  dependiente de la GUI en un limpiamente espefcicada API TMGUI y entonces
  hacer el puerto actual. De hecho, esto nos permitir� soportar m�ltiples
  toolkits gr�ficos. M�s detalles pueden ser encontrados en los archivos de
  la lista de correo <verbatim|texmacs-dev@gnu.org>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir
  Luna C�rdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versi�n 1.1 o cualquier versi�n posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia est� incluida en la
  secci�n titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|spanish>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>
