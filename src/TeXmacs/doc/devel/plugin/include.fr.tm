<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Int�gration de votre syst�me dans <apply|TeXmacs>>

  Supposons que vous avez r�ussi � �crire votre premi�re interface avec
  <apply|TeXmacs> � l'aide des explications de la section pr�c�dente. Il est
  temps maintenant d'inclure la gestion de votre syst�me dans la distribution
  standard de <apply|TeXmacs> distribution, apr�s quoi vous pourrez
  l'am�liorer.

  Depuis la sortie de la version 1.0.1.5, il est devenu tr�s facile d'adapter
  une interface de fa�on � ce qu'elle puisse �tre directement int�gr�e dans
  <TeXmacs>. Il suffit de cr�er un r�pertoire :

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim>

  o� <verbatim|myplugin> est le nom de votre plugin. Nous vous rappelons que
  <verbatim|$TEXMACS_HOME_PATH> est assimil� � <verbatim|~/.TeXmacs> par
  d�faut. Vous trouverez dans le r�pertoire <verbatim|$TEXMACS_PATH/plugins>
  tous les plugins standards qui sont livr�s avec <TeXmacs>. Servez-vous en
  de base pour construire les v�tres.

  Le r�pertoire <verbatim|myplugin> devra contenir une structure de
  r�pertoire similaire � la structure du r�pertoire <verbatim|$TEXMACS_PATH>,
  quoique vous puissiez omettre les sous-r�pertoires dont vous ne vous servez
  pas. N�anmoins, il vous faudra cr�er un fichier
  <verbatim|progs/init-myplugin.scm> qui d�crira l'initialisation de votre
  plugin. En g�n�ral, ce fichier contient juste une instruction <name|Scheme>
  de la forme suivante :

  <\verbatim>
    \ \ \ \ (plugin-configure myplugin<format|next line> \ \ \ \ \ (:require
    (file-in-path "myplugin"))<format|next line> \ \ \ \ \ (:launch
    "shell-cmd")<format|next line> \ \ \ \ \ (:format "input-format"
    "output-format")<format|next line> \ \ \ \ \ (:session "Myplugin"))
  </verbatim>

  La premi�re instruction est un pr�dicat qui teste si votre plugin peut �tre
  utilis� sur un syst�me donn�. En g�n�ral, il v�rifie qu'un programme donn�
  est accessible via votre PATH. Les autres instructions ne sont ex�cut�es
  que si ce premier point est v�rifi�. L'instruction <verbatim|:launch>
  sp�cifie que votre plugin sera lanc� avec <verbatim|shell-cmd>. La commande
  <verbatim|shell-cmd> est g�n�ralement de la forme <verbatim|myplugin
  --texmacs>. L'instruction <verbatim|:format> sp�cifie les formats d'entr�e
  et de sortie � utiliser. En g�n�ral, <verbatim|input-format> correspond �
  <verbatim|verbatim> et <verbatim|output-format> � <verbatim|generic>. Les
  autres formats possibles sont : <verbatim|scheme>, <verbatim|latex>,
  <verbatim|html> et <verbatim|ps>. L'instruction <verbatim|:session> rend
  les sessions shell disponible pour votre plugin � partir du menu
  <apply|menu|Insert|Session|Myplugin>.

  Si tout fonctionne correctement et que vous souhaitez faire profiter les
  autres de votre syst�me dans la version officielle de <apply|TeXmacs>
  distribution, contactez-moi � <verbatim|vdhoeven@texmacs.org>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Ins�rer>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Myplugin>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
