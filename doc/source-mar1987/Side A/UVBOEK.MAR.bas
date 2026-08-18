1 ON ERROR GOTO 20000
10 REM UVBOEK.MAR=UITDRUKKEN VERKOOPBOEK DD versie 040487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 LPRINT CHR$(27);"C";CHR$(72);
90 LPRINT CHR$(27);"N";CHR$(1);
100 PRINT cls$:PRINT FNat$(12,12,"FAKTUREN/CREDITNOTA'S  (F/C) ?");
110 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$=""THEN 110
120 IF m$="F"THEN r=11 ELSE r=13
130 IF m$="F"THEN r2=12 ELSE r2=14
140 BUFFERS 40
150 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
160 OPEN"R",1,drive$+"99.rnd",30
170 FIELD 1,6 AS rec.nummer$
180 GET 1,r:anr=VAL(rec.nummer$):GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,r2:unr=VAL(rec.nummer$):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 790:CLOSE 1
190 IF anr=unr THEN PRINT"VERKOOPBOEK IS REEDS UITGEDRUKT !!!!!"ELSE 210
200 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
210 unr=unr+1
220 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
230 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
240 OPEN"K",3,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
250 FIELD 3,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.dat1$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
260 z$=STRING$(132,"-")
270 IF m$="F"THEN tp$="UITDRUKKEN VERKOOPBOEK FAKTUREN"ELSE tp$="UITDRUKKEN VERKOOPBOEK CREDITNOTA'S"
280 PRINT cls$:PRINT hel$;:PRINT FNat$(0,0,tp$);zcht$;:PRINT FNat$(1,0,STRING$(80,172))
290 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
300 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
310 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 310
320 IF at$="J" THEN LPRINT CHR$(15); ELSE LPRINT CHR$(18);
330 PRINT FNat$(19,12,"PRINTER KLAAR ?  DRUK <V> VOOR VERVOLG - <M> VOOR MENU");
340 antw$="MV":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 340
350 IF at$="M"THEN 780
360 nr=anr+1
370 t=0:pteller=0
380 REM uitdrukken hoofdding
390 pteller=pteller+1:lteller=0
400 LPRINT
410 LPRINT
420 IF r=11 THEN tp$="VERKOOPFAKTUREN"ELSE tp$="CREDITNOTA'S OP VERKOOPFAKTUREN"
430 IF r=11 THEN tp2$="BTW VAK 11"ELSE tp2$="BTW VK 31/32"
440 LPRINT CHR$(14);"VERKOOPBOEK ";bedrijf$;TAB(52);"PAGINA : ";USING"##";pteller:LPRINT tp$;TAB(113);"DATUM : ";dat$:LPRINT
450 LPRINT z$:LPRINT"LIJN";TAB(6);"DATUM";TAB(15);"NAAM KLANT";TAB(40);"FAKT.";TAB(48);"KLANTEN";TAB(58);tp2$;TAB(73);"VERKOOP VAK 01";TAB(88);"VERKOOP VAK 02";TAB(103);"VERKOOP VAK 03";TAB(118);"VERKOOP VAK 04"
460 LPRINT TAB(73);"TAKSEN VAK 12";TAB(88);"VERKOOP 0 %";TAB(103);"MEDEKTR VAK 06";TAB(118);"CR.NOTA VAK 09":LPRINT z$:LPRINT
470 REM afdrukken faktuurgegevens
480 IF r=11 THEN s$="VF"ELSE s$="CV"
490 sleutel$=MID$(d$,7,2)+s$+STR$(unr):kontrole=SEEKKEY(3,0,1,sleutel$):GET 3
500 kontrole=SEEKKEY(1,0,0,rec.nrk$):GET 1
510 t=t+1:tot=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.ttb$)+VAL(rec.nttb$)+VAL(rec.reserve$)+VAL(rec.taks$)+VAL(rec.medektr$)
520 IF r=13 THEN ctb=VAL(rec.bt6$)+VAL(rec.bt19$)+VAL(rec.bt25$)+VAL(rec.bt33$)+VAL(rec.nttb$)+VAL(rec.reserve$)+VAL(rec.medektr$)ELSE ctb=0
530 IF r<>13 THEN 550
540 LSET rec.bt6$="":LSET rec.bt19$="":LSET rec.bt25$="":LSET rec.bt33$="":LSET rec.nttb$="":LSET rec.medektr$="":LSET rec.reserve$=""
550 LPRINT t;TAB(6);rec.dat$;TAB(15);MID$(rec.naam$,1,20);TAB(40);rec.boek$;TAB(48);USING"#########";tot;:LPRINT TAB(58);USING"#########";VAL(rec.ttb$);
560 LPRINT TAB(73);USING"#########";VAL(rec.bt6$);:LPRINT TAB(88);USING"#########";VAL(rec.bt19$);:LPRINT TAB(103);USING"#########";VAL(rec.bt25$);:LPRINT TAB(118);USING"#########";VAL(rec.bt33$)
570 LPRINT TAB(73);USING"#########";VAL(rec.taks$);:LPRINT TAB(88);USING"#########";VAL(rec.nttb$)+VAL(rec.reserve$);:LPRINT TAB(103);USING"#########";VAL(rec.medektr$);:LPRINT TAB(118);USING"#########";ctb
580 ttot=ttot+tot:IF r<>11 THEN 600
590 bt6=bt6+VAL(rec.bt6$):bt19=bt19+VAL(rec.bt19$):bt25=bt25+VAL(rec.bt25$):bt33=bt33+VAL(rec.bt33$):mktr=mktr+VAL(rec.medektr$)
600 ttb=ttb+VAL(rec.ttb$):nttb=nttb+VAL(rec.nttb$)+VAL(rec.reserve$):tctb=tctb+ctb:taks=taks+VAL(rec.taks$)
610 IF unr=anr THEN 660 ELSE unr=unr+1
620 lteller=lteller+1
630 IF lteller<25 THEN 650
640 LPRINT CHR$(12);:GOTO 390
650 GOTO 480
660 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(48);USING"#########";ttot;:LPRINT TAB(58);USING"#########";ttb;:LPRINT TAB(73);USING"#########";bt6;:LPRINT TAB(88);USING"#########";bt19;
670 LPRINT TAB(103);USING"#########";bt25;:LPRINT TAB(118);USING"#########";bt33:LPRINT TAB(73);USING"#########";taks;:LPRINT TAB(88);USING"#########";nttb;:LPRINT TAB(103);USING"#########";mktr;:LPRINT TAB(118);USING"#########";tctb
680 LPRINT:LPRINT
690 IF r=11 THEN LPRINT"TOTAAL DEBET : ";ttot ELSE LPRINT"TOTAAL DEBET : ";ttb;" + ";tctb;" + ";taks;" = ";ttb+tctb+taks
700 IF r=11 THEN LPRINT"TOTAAL CREDIT : ";ttb;" + ";bt6;" + ";bt19;" + ";bt25;" + ";bt33;" + ";nttb;" + ";taks;" + ";mktr;" = ";ttb+bt6+bt19+bt25+bt33+nttb+taks+mktr ELSE LPRINT"TOTAAL CREDIT : ";ttot
710 LPRINT CHR$(12);
720 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
730 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 730
740 CLOSE 1:CLOSE 3
750 ttot=0:bt6=0:bt19=0:bt25=0:bt33=0:nttb=0:tctb=0:ttb=0
760 IF at$="H"THEN 100
770 OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$:LSET rec.nummer$=STR$(unr):PUT 1,r2:CLOSE 1
780 RUN"menu.mar"
790 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
800 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 820 ELSE 810
810 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
820 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 2004