<TeXmacs|1.0.1.10>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Port de <TeXmacs> sur d'autres plateformes>

  Comme je n'ai acc�s qu'aux environnements PC/Linux et SUN, j'aimerais
  trouver des personnes qui pourraient porter <apply|TeXmacs> sur d'autres
  syst�mes Unix avec X Window et maintenir les distributions correspondantes.
  Si vous �tes int�ress�(e), voyez les fichiers :

  <\verbatim>
    \ \ \ \ configure.in<format|next line> \ \ \ src/Basic/fast_alloc.cpp
  </verbatim>

  Les sp�cialistes des progiciels <verbatim|autoconf>, redhat et rpm peuvent
  soumettre leurs suggestions, patches, etc.

  En plus de porter <apply|TeXmacs> sur d'autres syst�mes Unix, il serait
  int�ressant de porter <apply|TeXmacs> sur Windows (et Mac OS). Joignez-vous
  � la liste de diffusion <verbatim|texmacs-dev@gnu.org> si vous voulez nous
  aider. Des discussions ont eu lieu sur la fa�on d'effectuer le portage et,
  en particulier, sur quelle interface graphique utilisateur portable (telles
  Gtk, Qt, Wxwindows ou GNUstep) nous devions nous baser. Notre strat�gie
  est, dans une premi�re �tape, d'isoler le code d�pendant d'une GUI dans une
  API TMGUI, puis d'effectuer le portage. Ceci nous permettra de g�rer
  plusieurs bo�tes � outils graphiques. Vous trouverez de plus amples d�tails
  dans les archives de la liste de diffusion <verbatim|texmacs-dev@gnu.org>.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Mich�le Garoche>

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
    <associate|language|french>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
    <associate|toc-5|<tuple|<uninit>|?>>
    <associate|toc-6|<tuple|<uninit>|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>
