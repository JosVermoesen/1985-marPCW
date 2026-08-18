1 ON ERROR GOTO 20000
10 REM UAJOUR.MAR=UITDRUKKEN ALGEMEEN JOURNAAL versie 240687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$
40 eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
50 OPTION RUN
60 REM
70 REM OPTION RUN
100 BUFFERS 40
110 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
120 OPEN"R",1,drive$+"99.rnd",30
130 FIELD 1,6 AS rec.nummer$
140 GET 1,6:ajour=VAL(rec.nummer$):GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 620:CLOSE 1
145 IF bl=72 THEN lnbl=55 ELSE lnbl=49
146 LPRINT CHR$(27);"C";CHR$(bl);
147 LPRINT CHR$(27);"N";CHR$(1);
150 IF FIND$(drj$+"journaal.rnd")=""THEN PRINT"ALGEMEEN JOURNAAL IS REEDS UITGEDRUKT EN VERNIETIGD !!!"ELSE 170
160 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
170 OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2:FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
180 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2:FIELD 2,30 AS rec.om$
190 nr=0:nr$=STR$(nr):kontrole=SEEKKEY(1,0,0,nr$):IF kontrole<>0 THEN STOP ELSE GET 1
200 n=VAL(rec.datum$)
210 z$=STRING$(132,"-")
220 tp$="UITDRUKKEN ALGEMEEN JOURNAAL"
230 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
240 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
250 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
260 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 260
270 IF at$="J" THEN LPRINT CHR$(15); ELSE LPRINT CHR$(18);
280 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
290 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 290
300 IF at$="M"THEN 610
310 t=0:pteller=0
320 REM uitdrukken hoofdding
330 pteller=ajour+1:ajour=ajour+1:lteller=0
340 LPRINT
350 LPRINT
360 tp$="ALGEMEEN JOURNAAL"
370 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
380 LPRINT z$:LPRINT"LIJN REK. NUMMER DATUM    NAAM REKENING                  BETREFT                 DEBET    CREDIT"
390 LPRINT z$:LPRINT
400 IF t>1 THEN RETURN
410 REM afdrukken journaalposten
420 FOR t=1 TO n
430 t$=STR$(t):kontrole=SEEKKEY(1,0,0,t$):IF kontrole<>0 THEN STOP ELSE GET 1
440 kontrole=SEEKKEY(2,0,0,rec.num$):IF kontrole<>0 THEN STOP ELSE GET 2
450 IF VAL(rec.bedrg$)>0 THEN td#=td#+VAL(rec.bedrg$)ELSE tc#=tc#+ABS(VAL(rec.bedrg$))
460 IF VAL(rec.bedrg$)>0 THEN d=VAL(rec.bedrg$)ELSE c=ABS(VAL(rec.bedrg$))
470 LPRINT t;TAB(8);rec.num$;TAB(18);rec.datum$;TAB(27);rec.om$;TAB(58);rec.omschr$;TAB(78);USING"#########";d;:LPRINT TAB(88);USING"#########";c:d=0:c=0
480 lteller=lteller+1
490 IF lteller<lnbl THEN 510
500 LPRINT CHR$(12);:GOSUB 330
510 NEXT t
520 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(78);USING"#########";td#;:LPRINT TAB(88);USING"#########";tc#
530 LPRINT:LPRINT
540 LPRINT CHR$(12);
550 PRINT flash$;hel$;:PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR DRUKKEN HISTORIEK ");
560 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 560
570 PRINT zcht$;:CLOSE 1:CLOSE 2
580 d=0:c=0:td#=0:tc#=0:d=0:t=0
590 IF at$="H"THEN 600 ELSE 610
600 OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$:LSET rec.nummer$=STR$(ajour):PUT 1,6:CLOSE 1:RUN"uhistor.mar"
610 RUN"menu.mar"
620 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
630 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 650 ELSE 640
640 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
650 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 