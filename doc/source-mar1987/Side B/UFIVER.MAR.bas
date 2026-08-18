1 ON ERROR GOTO 20000
10 REM UFIVER.MAR=UITDRUKKEN FINANCIELE VERRICHTINGEN versie 240687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
100 BUFFERS 40
110 pteller=0:td1=0:td2=0:tc1=0:tc2=0:tdk=0:tck=0
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 810:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2:FIELD 2,30 AS rec.om$
170 z$=STRING$(132,"-")
180 tp$="UITDRUKKEN FINANCIELE BOEKEN"
190 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
200 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
210 PRINT FNat$(10,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
220 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 220
230 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
240 PRINT FNat$(14,12,"REKENING : ......  ");hel$;:PRINT FNat$(14,23,"");:INPUT"",nummer$:PRINT zcht$;:IF nummer$=""THEN 800
250 kontrole=SEEKKEY(2,0,0,nummer$):IF kontrole<>0 THEN 240 ELSE GET 2
260 r$=rec.om$
270 bestand$=MID$(rec.om$,1,2):bestand1$=drf$+bestand$+".rnd":bestand2$=drf$++bestand$+".key"
280 IF FIND$(bestand1$)=""THEN 240
290 PRINT hel$;:PRINT FNat$(16,23,rec.om$);zcht$;" OK (J/N) ?";
300 antw$="JN":p$=UPPER$(INKEY$):IF INSTR(antw$,p$)=0 OR p$=""THEN 300
310 IF p$="N"THEN 240
320 OPEN"K",1,bestand1$,bestand2$,2
330 FIELD 1,1 AS rec.m1$,1 AS rec.m2$,6 AS rec.nrr$,9 AS rec.btld$,20 AS rec.omsch$,9 AS rec.btkrt$,5 AS rec.dok$,8 AS rec.d$
340 nr$=STR$(0):kontrole=SEEKKEY(1,0,0,nr$):IF kontrole<>0 THEN STOP ELSE GET 1
350 n=VAL(rec.nrr$)
360 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
370 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 370
380 IF at$="M"THEN 800
390 t=0:pteller=0
400 REM uitdrukken hoofdding
410 pteller=ajour+1:ajour=ajour+1:lteller=0
420 LPRINT
430 LPRINT
440 tp$="FIN. BOEK "+r$
450 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
460 LPRINT z$:LPRINT"LIJN";TAB(6);"REK. NUMMER";TAB(18);"DATUM";TAB(27);"NAAM REKENING";TAB(58);"BETREFT";TAB(84);"FAKTUREN";TAB(102);"DIVERSEN";TAB(116);"BETALINGSKORTING":LPRINT TAB(81);"DEBET";TAB(89);"CREDIT";TAB(99);"DEBET";TAB(107);"CREDIT";
470 LPRINT TAB(117);"DEBET";TAB(125);"CREDIT"
480 LPRINT z$:LPRINT
490 IF t>1 THEN RETURN
500 REM afdrukken journaalposten
510 FOR t=1 TO n
520 t$=STR$(t):kontrole=SEEKKEY(1,0,0,t$):IF kontrole<>0 THEN STOP ELSE GET 1
530 kontrole=SEEKKEY(2,0,0,rec.nrr$):IF kontrole<>0 THEN STOP ELSE GET 2
540 IF rec.m1$="U"OR rec.m1$="u"THEN 600
550 IF rec.m2$="N"OR rec.m2$="n"THEN c2=ABS(VAL(rec.btld$))ELSE c1=ABS(VAL(rec.btld$))
560 tc2=tc2+c2:tc1=tc1+c1
570 IF rec.m2$="N"OR rec.m2$="n"THEN dk=VAL(rec.btkrt$)ELSE ck=ABS(VAL(rec.btkrt$))
580 tdk=tdk+dk:tck=tck+ck
590 GOTO 640
600 IF rec.m2$="N"OR rec.m2$="n"THEN d2=VAL(rec.btld$)ELSE d1=VAL(rec.btld$)
610 td2=td2+d2:td1=td1+d1
620 IF rec.m1$="U"OR rec.m1$="u"THEN ck=ABS(VAL(rec.btkrt$))ELSE dk=VAL(rec.btkrt$)
630 tck=tck+ck:tdk=tdk+dk
640 LPRINT t;TAB(6);rec.nrr$;TAB(18);rec.d$;TAB(27);rec.om$;TAB(58);rec.omsch$;TAB(78);USING"########";d1;:LPRINT TAB(87);USING"########";c1;:LPRINT TAB(96);USING"########";d2;:LPRINT TAB(105);USING"########";c2;
650 LPRINT TAB(114);USING"########";dk;:LPRINT TAB(123);USING"########";ck:d1=0:c1=0:d2=0:c2=0:dk=0:ck=0
660 lteller=lteller+1
670 IF lteller<lnbl THEN 690
680 LPRINT CHR$(12);:GOSUB 410
690 NEXT t
700 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(78);USING"########";td1;:LPRINT TAB(87);USING"########";tc1;:LPRINT TAB(96);USING"########";td2;:LPRINT TAB(105);USING"########";tc2;:LPRINT TAB(114);USING"########";tdk;
710 LPRINT TAB(123);USING"########";tck
720 LPRINT:LPRINT
730 LPRINT CHR$(12);
740 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");eol$;
750 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 750
760 CLOSE 1:CLOSE 2
770 d=0:c=0:td=0:tc=0:d=0:t=0
780 IF at$="H"THEN 100
790 CLOSE 1:CLOSE 2:CLOSE 3:KILL bestand1$:KILL bestand2$
800 RUN"menu.mar"
810 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
820 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 840 ELSE 830
830 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
840 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
2