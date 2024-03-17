1 ON ERROR GOTO 20000
10 REM BL.MAR=UITDRUKKEN BTW LISTING versie 030887
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$
40 eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
50 REM
60 REM
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:GET 1,47:straat$=rec.bedrijf$:GET 1,48:stad$=rec.bedrijf$:GET 1,49:tele$=rec.bedrijf$
160 GET 1,50:hr$=rec.bedrijf$:GET 1,51:nrbtw$=rec.bedrijf$:GET 1,52:brek$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 710:CLOSE 1
165 IF bl=72 THEN lnbl=22 ELSE lnbl=19
166 LPRINT CHR$(27);"C";CHR$(bl);
167 LPRINT CHR$(27);"N";CHR$(1);
170 IF FIND$(drk$+"klanten.rnd")=""THEN PRINT"KLANTENBESTAND BESTAAT NOG NIET !!!"ELSE 190
180 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
190 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
200 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
210 tp$="UITDRUKKEN BTW LISTING"
220 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
230 PRINT FNat$(12,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(12,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
260 kode=0
270 PRINT FNat$(16,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
280 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 280
290 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
300 kontrole=SEEKRANK(1,2,kode)
310 PRINT FNat$(16,5,"PRINTER EN PAPIER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU : ");
320 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 320
330 IF at$="M"THEN 700
340 t=0:pteller=0
350 FOR t=1 TO 20:LPRINT:NEXT t
360 LPRINT CHR$(14);TAB(24);"BTW LISTING PER ";dat$:LPRINT CHR$(14);TAB(24);STRING$(24,"="):LPRINT:LPRINT
370 LPRINT:LPRINT CHR$(14);TAB(21);STRING$(32,"*"):LPRINT:LPRINT CHR$(14);TAB(21);"* "+bedrijf$+" *":LPRINT
380 LPRINT CHR$(14);TAB(21);"* "+straat$+" *":LPRINT:LPRINT CHR$(14);TAB(21);"* "+stad$+" *":LPRINT:LPRINT CHR$(14);TAB(21);"* "+tele$+" *":LPRINT:LPRINT CHR$(14);TAB(21);"* "+hr$+" *":LPRINT
390 LPRINT CHR$(14);TAB(21);"* "+nrbtw$+" *":LPRINT:LPRINT CHR$(14);TAB(21);"* "+brek$+" *":LPRINT:LPRINT CHR$(14);TAB(21);STRING$(32,"*")
400 LPRINT CHR$(12);:t=0:REM uitdrukken hoofdding
410 pteller=pteller+1:lteller=0
420 LPRINT
430 LPRINT
440 tp$="BTW-LISTING"
450 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT:LPRINT:LPRINT
460 LPRINT z$:LPRINT"NUM.";TAB(8);"NAAM";TAB(36);"STRAAT EN NR";TAB(66);"P/C";TAB(71);"STAD";TAB(101);"BTW NUMMER";TAB(113);"GOEDEREN";TAB(123);"BEDRAG BTW"
470 LPRINT z$:LPRINT:LPRINT
475 IF t<>0 THEN LPRINT TAB(96);"OVER GEDRAGEN :";:LPRINT TAB(113);USING"#########";tj;:LPRINT TAB(123);USING"#########";ts:LPRINT:LPRINT ELSE 480
480 IF t<>0 THEN RETURN
490 REM afdrukken journaalposten
500 a$=FETCHKEY$(1):kontrole=SEEKKEY(1,2,kode,a$):IF kontrole<>0 THEN STOP ELSE GET 1
510 IF VAL(rec.jaaromzet$)-VAL(rec.btwbedrag$)<5000 THEN 580
520 IF INSTR(rec.btwnr$,"-")=0 THEN 580 ELSE 530
530 tj=tj+VAL(rec.jaaromzet$)-VAL(rec.btwbedrag$):ts=ts+VAL(rec.btwbedrag$)
540 lteller=lteller+1
550 IF LEN(a$)=6 THEN num$=a$ELSE num$=""
560 t=t+1:LPRINT rec.numkl$;TAB(8);MID$(rec.naam$,1,28);TAB(36);rec.straat$;TAB(66);rec.postkode$;TAB(71);rec.stad$;TAB(101);rec.btwnr$;TAB(113);USING"#########";VAL(rec.jaaromzet$)-VAL(rec.btwbedrag$);:LPRINT TAB(123);USING"#########";VAL(rec.btwbedrag$)
570 LPRINT
580 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 620
590 IF lteller<lnbl THEN 610
600 LPRINT TAB(96);"OVER TE DRAGEN :";:LPRINT TAB(113);USING"#########";tj;:LPRINT TAB(123);USING"#########";ts:LPRINT CHR$(12);:GOSUB 410
610 IF kontrole=0 THEN 510 ELSE 500
620 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL KLANTEN :";USING"####";t;:LPRINT TAB(113);USING"#########";tj;:LPRINT TAB(123);USING"#########";ts
630 LPRINT CHR$(12);
640 ts=0:tj=0
650 PRINT FNat$(18,5,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
660 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 660
670 CLOSE 1:CLOSE 2
680 ts=0:tj=0
690 IF at$="H"THEN 110
700 RUN"menu.mar"
710 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
720 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 740 ELSE 730
730 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
740 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20