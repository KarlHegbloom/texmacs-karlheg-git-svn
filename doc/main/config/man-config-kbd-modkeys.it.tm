<TeXmacs|1.0.1.18>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Configurazione dei tasti modificatori>

  <apply|TeXmacs> utilizza cinque principali modificatori da tastiera:
  <key|<expand|key-shift>>, <key|<expand|key-control>>,
  <key|<expand|key-alternate>>, <key|<expand|key-meta>> e
  <key|<expand|key-hyper>>, che vengono abbreviati rispettivamente con
  <key|S->, <key|C->, <key|A->, <key|M-> e <key|H->. I tasti
  <key|<expand|key-shift>> e \ <key|<expand|key-control>> si trovano su tutte
  le tastiere; il tasto \ <key|<expand|key-alternate>> su quasi tutte. Molte
  tastiere moderne sono dotate anche del tasto <key|<expand|key-windows>>
  che, in <TeXmacs>, � equivalente a <key|<expand|key-meta>>.

  Prima di riconfigurare la tastiera � consigliabile verificare se ci� sia
  veramente necessario. Se sulla vostra tastiera ci sono i tasti
  corrispondenti a <key|<expand|key-shift>>, <key|<expand|key-control>>,
  <key|<expand|key-alternate>> e <key|<expand|key-meta>>, probabilmente non
  saranno necessarie modifiche di configurazione. Una possibile eccezione �
  che desideriate utilizzare il tasto <key|<expand|key-caps-lock>> per
  scrivere simboli matematici. In questo caso dovrete far corrispondere il
  tasto <key|<expand|key-caps-lock>> al tasto <key|<expand|key-hyper>>.

  Per riconfigurare la tastiera � sufficiente selezionare, nel menu
  <apply|menu|Edit|Preferences|Keyboard>, il modificatore logico che
  desiderate far corrispondere ad un dato tasto fisico. Ad esempio,
  selezionando <apply|menu|Windows key|Map to M modifier>, il tasto
  <key|<expand|key-windows>> verr� fatto corrispondere al modificatore
  <key|<expand|key-meta>>. In modo del tutto analogo, selezionando
  <apply|menu|Caps-lock key|Map to H modifier>, il tasto
  <key|<expand|key-caps-lock>> verr� fatto corrispondere al modificatore
  <key|<expand|key-hyper>>.

  Sfortunatamente il sistema X Window permette di effettuare solo
  riconfigurazioni globali. Per questo, se in <TeXmacs> viene riconfigurato
  il tasto <key|<expand|key-caps-lock>>, il nuovo comportamento di questo
  tasto interesser� anche tutte le altre applicazioni in cui questo stesso
  tasto viene utilizzato. Perci� � consigliabile riconfigurare solo i tasti
  che non vengono utilizzati per scopi diversi in altre applicazioni. Ad
  esempio, il tasto <key|<expand|key-windows>> non viene solitamente
  utilizzato in molte altre applicazioni, percui la sua riconfigurazione non
  ha ripercussioni. Alcuni utilizzatori potrebbero decidere di effettuare una
  opportuna riconfigurazione globale della tastiera. Ci� pu� essere fatto
  ricorrendo al comando <verbatim|xmodmap> per informazioni sul quale si
  rimanda alle corrispondenti pagine del manuale.

  In alcuni casi pu� accadere che i tasti corrispondenti a
  <key|<expand|key-alternate>>, <key|<expand|key-meta>> e
  <key|<expand|key-hyper>>, siano presenti sulla tastiera ma non funzionino
  nel modo desiderato. Per farli funzionare adeguatamente � sufficiente
  rimappare i prefissi <key|A->, <key|M-> e <key|H-> in altri modificatori
  logici appartenenti al primo gruppo di sottomenu in
  <apply|menu|Edit|Preferences|Keyboard>.

  Ad esempio, per avere compatibilit� con Emacs, potreste decidere di
  scambiare il tasto <key|<expand|key-meta>> o <key|<expand|key-windows>> con
  il tasto <key|<expand|key-alternate>> senza tuttavia eseguire una modifica
  globale. Ci� pu� essere fatto cercando i modificatori corrispondenti a
  questo tasto; tipicamente avremo <key|Mod1> al posto di
  <key|<expand|key-alternate>> e <key|Mod4> al posto di
  <key|<expand|key-meta>> o <key|<expand|key-windows>>. Quindi eseguiremo le
  dovute permutazioni nel menu <apply|menu|Edit|Preferences|Keyboard>,
  selezionando <apply|menu|A modifier|Equivalent for Mod4> e <apply|menu|M
  modifier|Equivalent for Mod1>.

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven, Andrea Centomo>

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
    <associate|language|italian>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Tasto Window>|<with|font
      family|<quote|ss>|Map to M modifier>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Caps-lock key>|<with|font
      family|<quote|ss>|Map to H modifier>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Modifica>|<with|font
      family|<quote|ss>|Preferenze>|<with|font
      family|<quote|ss>|Tastiera>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|A modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod4>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|ss>|M modifier>|<with|font
      family|<quote|ss>|Equivalent for Mod1>>|<pageref|idx-7>>
    </associate>
  </collection>
</auxiliary>
