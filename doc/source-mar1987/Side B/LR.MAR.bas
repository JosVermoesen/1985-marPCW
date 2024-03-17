1 ON ERROR GOTO 20000
10 REM LR.MAR=UITDRUKKEN LIJST ALGEMENE REKENINGEN versie 250687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 620:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 IF FIND$(drr$+"rekening.rnd")=""THEN PRINT"REKENINGENBESTAND BESTAAT NOG NIET !!!"ELSE 180
170 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
180 OPEN"K",1,drr$+"rekening.rnd",drr$+"rekening.key",2:FIELD 1,30 AS rec.naam$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
190 tp$="UITDRUKKEN LIJST ALGEMENE REKENINGEN"
200 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
210 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
220 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
230 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 230
240 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
250 kontrole=SEEKRANK(1,0,kode)
260 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU : ");
270 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 270
280 IF at$="M"THEN 610
290 t=0:pteller=0
300 REM uitdrukken hoofdding
310 pteller=pteller+1:lteller=0
320 LPRINT
330 LPRINT
340 tp$="LIJST ALGEMENE REKENINGEN"
350 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
360 LPRINT z$:LPRINT"NUMMER";TAB(9);"NAAM";TAB(42);"J A A R S A L D O";TAB(64);"P E R I O D I E K   S A L D O"
370 LPRINT TAB(46);"DEBET";TAB(56);"CREDIT";TAB(71);"DEBET";TAB(81);"CREDIT"
380 LPRINT z$:LPRINT
390 IF t<>0 THEN RETURN
400 REM afdrukken journaalposten
410 a$=FETCHKEY$(1):kontrole=SEEKKEY(1,0,kode,a$):IF kontrole<>0 THEN STOP ELSE GET 1
420 IF VAL(rec.jaarsaldo$)<0 THEN jc=ABS(VAL(rec.jaarsaldo$))ELSE jd=VAL(rec.jaarsaldo$)
430 IF VAL(rec.maandsaldo$)<0 THEN mc=ABS(VAL(rec.maandsaldo$))ELSE mds=VAL(rec.maandsaldo$)
440 jtd=jtd+jd:jtc=jtc+jc:mtd=mtd+mds:mtc=mtc+mc
450 lteller=lteller+1
460 IF LEN(a$)=6 THEN num$=a$ELSE num$=""
470 t=t+1:LPRINT num$;TAB(9);rec.naam$;TAB(42);USING"#########";jd;:LPRINT TAB(52);USING"#########";jc;:LPRINT TAB(67);USING"#########";mds;:LPRINT TAB(77);USING"#########";mc
480 jd=0:jc=0:mds=0:mc=0
490 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 530
500 IF lteller<lnbl THEN 520
510 LPRINT CHR$(12);:GOSUB 310
520 IF kontrole=0 THEN 420 ELSE 410
530 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL REKENINGEN :";USING"####";t;:LPRINT TAB(42);USING"#########";jtd;:LPRINT TAB(52);USING"#########";jtc;:LPRINT TAB(67);USING"#########";mtd;:LPRINT TAB(77);USING"#########";mtc
540 LPRINT CHR$(12);
550 ts=0:tj=0
560 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
570 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 570
580 CLOSE 1:CLOSE 2
590 ts=0:tj=0:pteller=0:jtd=0:jtc=0:mtd=0:mtc=0
600 IF at$="H"THEN 110
610 RUN"menu.mar"
620 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
630 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 650 ELSE 640
640 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
650 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 