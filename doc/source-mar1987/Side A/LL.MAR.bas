1 ON ERROR GOTO 20000
10 REM LL.MAR=UITDRUKKEN LIJST LEVERANCIERS versie 250687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 660:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 IF FIND$(drl$+"leveranc.rnd")=""THEN PRINT"LEVERANCIERSBESTAND BESTAAT NOG NIET !!!"ELSE 180
170 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
180 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2:FIELD 1,6 AS rec.levnum$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
190 tp$="UITDRUKKEN LIJST LEVERANCIERS"
200 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
210 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
220 PRINT FNat$(10,12,"GESORTEERD <N>UMMERISCH - <A>LFABETISCH");
230 antw$="NA":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 230
240 PRINT FNat$(14,12,"MIN SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(14,32,"");:INPUT"",mini$:PRINT zcht$;:IF LEN(mini$)>6 THEN 240
250 PRINT FNat$(15,12,"MAX SLEUTELNUMMER : ......");eol$;hel$;:PRINT FNat$(15,32,"");:INPUT"",maxi$:PRINT zcht$;:IF LEN(maxi$)>6 THEN 250
260 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
270 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 270
280 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
290 IF a$="N"THEN kode=0 ELSE IF a$="A"THEN kode=1
300 kontrole=SEEKRANK(1,0,kode)
310 IF mini$=""THEN 320 ELSE kontrole=SEEKKEY(1,0,kode,mini$)
320 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU : ");
330 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 330
340 IF at$="M"THEN 650
350 t=0:pteller=0
360 REM uitdrukken hoofdding
370 pteller=pteller+1:lteller=0
380 LPRINT
390 LPRINT
400 tp$="LIJST LEVERANCIERS"
410 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
420 LPRINT z$:LPRINT"NUMMER";TAB(9);"NAAM";TAB(40);"STRAAT EN NR";TAB(71);"WOONPLAATS";TAB(101);"REK. NUMMER";TAB(116);"OMZET";TAB(126);"SALDO"
430 LPRINT z$:LPRINT
440 IF t<>0 THEN RETURN
450 REM afdrukken journaalposten
460 a$=FETCHKEY$(1)
470 IF a$>maxi$THEN 570
480 kontrole=SEEKKEY(1,0,kode,a$):IF kontrole<>0 THEN STOP ELSE GET 1
490 tj=tj+VAL(rec.jaaromzet$):ts=ts+VAL(rec.saldo$)
500 lteller=lteller+1
510 IF LEN(a$)=4 THEN num$=a$ELSE num$=""
520 t=t+1:LPRINT rec.levnum$;TAB(9);rec.naam$;TAB(40);rec.straat$;TAB(71);rec.stad$;TAB(101);rec.rekening$;TAB(115);USING"#########";VAL(rec.jaaromzet$);:LPRINT TAB(124);USING"#########";VAL(rec.saldo$)
530 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 570
540 IF lteller<lnbl THEN 560
550 LPRINT CHR$(12);:GOSUB 370
560 IF kontrole=0 THEN 490 ELSE 450
570 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL LEVERANCIERS :";USING"####";t;:LPRINT TAB(115);USING"#########";tj;:LPRINT TAB(124);USING"#########";ts
580 LPRINT CHR$(12);
590 ts=0:tj=0
600 PRINT FNat$(19,12,eol$);:PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
610 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 610
620 CLOSE 1:CLOSE 2
630 ts=0:tj=0
640 IF at$="H"THEN 110
650 RUN"menu.mar"
660 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
670 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 690 ELSE 680
680 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
690 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
ER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at