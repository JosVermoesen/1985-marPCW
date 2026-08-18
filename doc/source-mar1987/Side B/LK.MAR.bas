1 ON ERROR GOTO 20000
10 REM LK.MAR=UITDRUKKEN LIJST KLANTEN versie 250687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 670:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 IF FIND$(drk$+"klanten.rnd")=""THEN PRINT"KLANTENBESTAND BESTAAT NOG NIET !!!"ELSE 180
170 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
180 OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
190 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
200 tp$="UITDRUKKEN KLANTENLIJST"
210 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
220 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
230 PRINT FNat$(10,12,"GESORTEERD <N>UMMERISCH - <A>LFABETISCH - <P>OSTKODE");
240 antw$="NAP":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 240
250 IF a$="N"THEN kode=0 ELSE IF a$="A"THEN kode=1 ELSE IF a$="P"THEN kode=2
260 PRINT FNat$(14,12,"MIN SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(14,32,"");:INPUT"",mini$:PRINT zcht$;:IF LEN(mini$)>6 THEN 260
270 PRINT FNat$(15,12,"MAX SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(15,32,"");:INPUT"",maxi$:PRINT zcht$;:IF LEN(maxi$)>6 THEN 270
280 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
290 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 290
300 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
310 kontrole=SEEKRANK(1,2,kode)
320 IF mini$=""THEN 330 ELSE kontrole=SEEKKEY(1,2,kode,mini$)
330 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU : ");
340 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 340
350 IF at$="M"THEN 660
360 t=0:pteller=0
370 REM uitdrukken hoofdding
380 pteller=pteller+1:lteller=0
390 LPRINT
400 LPRINT
410 tp$="KLANTENLIJST"
420 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
430 LPRINT z$:LPRINT"NUMMER";TAB(8);"NAAM";TAB(36);"STRAAT EN NR";TAB(66);"P/C";TAB(71);"STAD";TAB(101);"TELEFOON";TAB(113);"JAAROMZET";TAB(123);"    SALDO"
440 LPRINT z$:LPRINT
450 IF t<>0 THEN RETURN
460 REM afdrukken journaalposten
470 a$=FETCHKEY$(1)
480 IF a$>maxi$THEN 580
490 kontrole=SEEKKEY(1,2,kode,a$):IF kontrole<>0 THEN STOP ELSE GET 1
500 tj=tj+VAL(rec.jaaromzet$):ts=ts+VAL(rec.saldo$)
510 lteller=lteller+1
520 IF LEN(a$)=6 THEN num$=a$ELSE num$=""
530 t=t+1:LPRINT rec.numkl$;TAB(8);MID$(rec.naam$,1,28);TAB(36);rec.straat$;TAB(66);rec.postkode$;TAB(71);rec.stad$;TAB(101);rec.telefoon$;TAB(113);USING"#########";VAL(rec.jaaromzet$);:LPRINT TAB(123);USING"#########";VAL(rec.saldo$)
540 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 580
550 IF lteller<lnbl THEN 570
560 LPRINT CHR$(12);:GOSUB 380
570 IF kontrole=0 THEN 500 ELSE 460
580 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL KLANTEN :";USING"####";t;:LPRINT TAB(113);USING"#########";tj;:LPRINT TAB(123);USING"#########";ts
590 LPRINT CHR$(12);
600 ts=0:tj=0
610 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
620 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 620
630 CLOSE 1:CLOSE 2
640 ts=0:tj=0
650 IF at$="H"THEN 110
660 RUN"menu.mar"
670 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
680 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 700 ELSE 690
690 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
700 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"TH