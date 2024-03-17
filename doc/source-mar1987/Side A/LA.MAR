1 ON ERROR GOTO 20000
10 REM LA.MAR=UITDRUKKEN LIJST ARTIKELEN versie 250687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
80 z$=STRING$(132,"-")
110 BUFFERS 40
120 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
130 OPEN"R",1,drive$+"99.rnd",30
140 FIELD 1,6 AS rec.nummer$
150 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 650:CLOSE 1
155 IF bl=72 THEN lnbl=55 ELSE lnbl=49
156 LPRINT CHR$(27);"C";CHR$(bl);
157 LPRINT CHR$(27);"N";CHR$(1);
160 IF FIND$(dra$+"artikels.rnd")=""THEN PRINT"ARTIKELBESTAND BESTAAT NOG NIET !!!"ELSE 180
170 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
180 OPEN"K",1,dra$+"artikels.rnd",dra$+"artikels.key",2:FIELD 1,25 AS rec.naam$,9 AS rec.kostprijs$,2 AS rec.btw$,9 AS rec.jaaromzet$,9 AS rec.maandomzet$,9 AS rec.stock$,9 AS rec.minstock$
190 recleng=4:OPEN"R",2,drb$+"btw.rnd",4:FIELD 2,2 AS rec.btwpct$
200 tp$="UITDRUKKEN LIJST ARTIKELEN STOCK"
210 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172))
220 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
230 kontrole=SEEKRANK(1,0,kode)
240 PRINT FNat$(14,12,"MIN SLEUTELNUMMER : ..........");eol$;hel$;:PRINT FNat$(14,32,"");:INPUT"",mini$:PRINT zcht$;:IF LEN(mini$)>10 THEN 240
250 PRINT FNat$(15,12,"MAX SLEUTELNUMMER : ..........");eol$;hel$;:PRINT FNat$(15,32,"");:INPUT"",maxi$:PRINT zcht$;:IF LEN(maxi$)>10 THEN 250
260 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
270 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 270
280 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
290 PRINT zcht$;:PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU");
300 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 300
310 IF mini$=""THEN 320 ELSE kontrole=SEEKKEY(1,0,kode,mini$)
320 IF at$="M"THEN 640
330 t=0:pteller=0
340 REM uitdrukken hoofdding
350 pteller=pteller+1:lteller=0
360 LPRINT
370 LPRINT
380 tp$="LIJST ARTIKELEN STOCK"
390 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
400 LPRINT z$:LPRINT"NUMMER";TAB(13);"NAAM";TAB(41);"KOSTPRIJS";TAB(51);"BTW KODE";TAB(60);"JAAROMZET";TAB(70);"MAANDOMZET";TAB(81);"STOCK";TAB(91);"MIN.STOCK";TAB(101);"TE BESTELLEN";TAB(116);"BEDRAG STOCK"
410 LPRINT z$:LPRINT
420 IF t<>0 THEN RETURN
430 REM afdrukken journaalposten
440 a$=FETCHKEY$(1)
450 IF a$>maxi$THEN 560
460 kontrole=SEEKKEY(1,0,kode,a$):IF kontrole<>0 THEN STOP ELSE GET 1
470 tj=tj+VAL(rec.jaaromzet$):ts=ts+VAL(rec.maandomzet$):tbst=VAL(rec.minstock$)-VAL(rec.stock$):IF tbst<0 THEN tbst=0
480 lteller=lteller+1
490 GET 2,VAL(rec.btw$):btw=VAL(rec.btwpct$):kp=VAL(rec.kostprijs$):totbedrg=ROUND(kp*VAL(rec.stock$),0):ttotbdrg#=ttotbdrg#+totbedrg
500 t=t+1:LPRINT a$;TAB(13);rec.naam$;TAB(41);USING"######.##";VAL(rec.kostprijs$);:LPRINT TAB(51);USING"##";VAL(rec.btw$);:LPRINT TAB(60);USING"#########";VAL(rec.jaaromzet$);:LPRINT TAB(70);USING"#########";VAL(rec.maandomzet$);
510 LPRINT TAB(81);USING"#########";VAL(rec.stock$);:LPRINT TAB(91);USING"#########";VAL(rec.minstock$);:LPRINT TAB(101);USING"#########";tbst;:LPRINT TAB(116);USING"#########";totbedrg
520 kontrole=SEEKNEXT(1,kode):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 560
530 IF lteller<lnbl THEN 550
540 LPRINT CHR$(12);:GOSUB 350
550 IF kontrole=0 THEN 470 ELSE 430
560 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL ARTIKELS :";USING"####";t;:LPRINT TAB(60);USING"#########";tj;:LPRINT TAB(70);USING"#########";ts;:LPRINT TAB(116);USING"#########";ttotbdrg#
570 LPRINT CHR$(12);
580 ts=0:tj=0:ttotbdrg#=0
590 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
600 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 600
610 CLOSE 1:CLOSE 2
620 ts=0:tj=0
630 IF at$="H"THEN 110
640 RUN"menu.mar
650 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
660 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 680 ELSE 670
670 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
680 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 2004