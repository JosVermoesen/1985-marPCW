1 ON ERROR GOTO 20000
10 REM UHISTOR.MAR=UITDRUKKEN HISTORIEK REKENINGEN DD versie 240687
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 670:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 IF FIND$(drj$+"journaal.rnd")=""THEN PRINT"HISTORIEK REKENINGEN IS REEDS UITGEDRUKT EN HET BESTAND IS REEDS VERNIETIGD !!!"ELSE 180
170 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
180 OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2:FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
190 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2:FIELD 2,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
200 kontrole=SEEKRANK(1,2,1)
210 tp$="UITDRUKKEN HISTORIEK ALGEMENE REKENINGEN"
220 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
230 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
240 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
250 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 250
260 IF at$="J" THEN LPRINT CHR$(15); ELSE LPRINT CHR$(18);
270 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
280 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 280
290 IF at$="M"THEN 660
300 t=0:pteller=0
310 REM uitdrukken hoofdding
320 lteller=0
330 LPRINT
340 LPRINT
350 tp$="HISTORIEK REKENINGEN"
360 IF t<>0 THEN 390
370 a$=FETCHKEY$(1):kontrole=SEEKKEY(1,0,1,a$):IF kontrole<>0 THEN STOP ELSE GET 1
380 kontrole=SEEKKEY(2,0,0,a$):IF kontrole<>0 THEN STOP ELSE GET 2
390 IF t=0 THEN pteller=VAL(rec.telhistor$)+1 ELSE pteller=pteller+1
400 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
410 LPRINT CHR$(14);rec.num$;" ";rec.om$:LPRINT z$:LPRINT"LIJN DATUM    BETREFT                   VORIG SALDO     DEBET    CREDIT  NIEUW SALDO"
420 LPRINT z$:LPRINT
430 IF t<>0 THEN RETURN
440 stand1=VAL(rec.jaarsaldo$):IF stand1<0 THEN LPRINT TAB(41);"CS:";USING"#########";ABS(VAL(rec.jaarsaldo$))ELSE LPRINT TAB(41);"DS:";USING"#########";VAL(rec.jaarsaldo$)
450 REM afdrukken journaalposten
460 t=t+1
470 IF VAL(rec.bedrg$)<0 THEN c=ABS(VAL(rec.bedrg$))ELSE d=VAL(rec.bedrg$)
480 IF VAL(rec.bedrg$)<0 THEN tc=tc+VAL(rec.bedrg$)ELSE td=td+VAL(rec.bedrg$)
490 LPRINT t;TAB(6);rec.datum$;TAB(15);rec.omschr$;TAB(52);USING"#########";d;:LPRINT TAB(62);USING"#########";c
500 d=0:c=0
510 kontrole=SEEKNEXT(1,1):IF kontrole=0 THEN GET 1 ELSE IF kontrole=101 THEN 560 ELSE 560
520 lteller=lteller+1
530 IF lteller<lnbl THEN 550
540 LPRINT CHR$(12);:GOSUB 320
550 GOTO 460
560 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(52);USING"#########";td;:LPRINT TAB(62);USING"#########";ABS(tc);
570 ns=VAL(rec.jaarsaldo$)+td+tc:IF ns<0 THEN LPRINT TAB(74);"CS";USING"#########";ABS(ns)ELSE LPRINT TAB(74);"DS";USING"#########";ns
580 LPRINT:LPRINT
590 LPRINT CHR$(12);
600 IF td+tc<>VAL(rec.maandsaldo$)THEN LPRINT"JOURNAALPOSTEN NIET CONFORM MET REKENINGSTELSEL 1) DEBET=CREDIT ? 2) MANUELE WIJZIGINGEN AANGEBRACHT ? KONTAKTEER VSOFT !!!"ELSE ns=VAL(rec.jaarsaldo$)+VAL(rec.maandsaldo$)
610 LSET rec.telhistor$=STR$(pteller):LSET rec.jaarsaldo$=STR$(ns):LSET rec.maandsaldo$=STR$(0):PUT 2
620 d=0:c=0:td=0:tc=0:t=0:IF kontrole=101 THEN 300
630 CLOSE 1:CLOSE 2
640 d=0:c=0:td=0:tc=0:d=0:t=0
650 KILL drj$+"journaal.rnd":KILL drj$+"journaal.key"
660 RUN"menu.mar"
670 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
680 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 700 ELSE 690
690 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
700 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RU