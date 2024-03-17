10 ON ERROR GOTO 750
15 REM UHISTEEN.MAR=UITDRUKKEN HISTORIEK VAN EEN REKENING versie 270687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$
40 eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
50 OPTION RUN
60 REM
80 'OPTION RUN
90 z$=STRING$(132,"-")
100 BUFFERS 40
110 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
120 OPEN"R",1,drive$+"99.rnd",30
130 FIELD 1,6 AS rec.nummer$
140 GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 710:CLOSE 1
150 IF bl=72 THEN lnbl=55 ELSE lnbl=49
160 LPRINT CHR$(27);"C";CHR$(bl);
170 LPRINT CHR$(27);"N";CHR$(1);
180 IF FIND$(drj$+"journaal.rnd")=""THEN PRINT"DE PERIODE IS REEDS AFGESLOTEN EN HET BESTAND VERNIETIGD !!!"ELSE 200
190 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
200 OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2:FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
210 OPEN"K",2,drr$+"rekening.rnd",drr$+"rekening.key",2:FIELD 2,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
220 kontrole=SEEKRANK(1,2,1)
230 tp$="UITDRUKKEN HISTORIEK VAN EEN REKENING"
240 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,172)):PRINT FNat$(22,0,STRING$(80,172)):PRINT FNat$(23,0,"DRUK <RETURN> VOOR HOOFDMENU")
250 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
260 PRINT FNat$(10,12,"NR REKENING   : ......");eol$;hel$;:PRINT FNat$(10,28,"");:INPUT"",a$:PRINT zcht$;:IF a$=""THEN 700
270 kontrole=SEEKKEY(2,0,0,a$):IF kontrole<>0 THEN 260 ELSE GET 2
280 PRINT hel$;:PRINT FNat$(12,28,rec.om$);zcht$;" OK (J/N) ?";
290 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 290
300 IF at$="N"THEN 260
310 IF SEEKKEY(1,0,1,a$)<>0 THEN PRINT FNat$(23,0,"GEEN VERRICHTINGEN OP DEZE REKENING !!!");eol$;:GOTO 260
320 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
330 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 330
340 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
350 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
360 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 360
370 IF at$="M"THEN 700
380 t=0:pteller=0
390 REM uitdrukken hoofdding
400 lteller=0
410 LPRINT
420 LPRINT
430 tp$="HISTORIEK VAN EEN REKENING"
440 IF t<>0 THEN 470
450 kontrole=SEEKKEY(1,0,1,a$):IF kontrole<>0 THEN STOP ELSE GET 1
460 kontrole=SEEKKEY(2,0,0,a$):IF kontrole<>0 THEN STOP ELSE GET 2
470 IF t=0 THEN pteller=VAL(rec.telhistor$)+1 ELSE pteller=pteller+1
480 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";dat$:LPRINT
490 LPRINT CHR$(14);rec.num$;" ";rec.om$:LPRINT z$:LPRINT"LIJN DATUM    BETREFT                   VORIG SALDO     DEBET    CREDIT  NIEUW SALDO"
500 LPRINT z$:LPRINT
510 IF t<>0 THEN RETURN
520 stand1=VAL(rec.jaarsaldo$):IF stand1<0 THEN LPRINT TAB(41);"CS:";USING"#########";ABS(VAL(rec.jaarsaldo$))ELSE LPRINT TAB(41);"DS:";USING"#########";VAL(rec.jaarsaldo$)
530 REM afdrukken journaalposten
540 t=t+1
550 IF VAL(rec.bedrg$)<0 THEN c=ABS(VAL(rec.bedrg$))ELSE d=VAL(rec.bedrg$)
560 IF VAL(rec.bedrg$)<0 THEN tc=tc+VAL(rec.bedrg$)ELSE td=td+VAL(rec.bedrg$)
570 LPRINT t;TAB(6);rec.datum$;TAB(15);rec.omschr$;TAB(52);USING"#########";d;:LPRINT TAB(62);USING"#########";c
580 d=0:c=0
590 kontrole=SEEKNEXT(1,1):IF kontrole=0 THEN GET 1 ELSE IF kontrole=101 THEN 640 ELSE 640
600 lteller=lteller+1
610 IF lteller<lnbl THEN 630
620 LPRINT CHR$(12);:GOSUB 400
630 GOTO 540
640 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(52);USING"#########";td;:LPRINT TAB(62);USING"#########";ABS(tc);
650 ns=VAL(rec.jaarsaldo$)+td+tc:IF ns<0 THEN LPRINT TAB(74);"CS";USING"#########";ABS(ns)ELSE LPRINT TAB(74);"DS";USING"#########";ns
660 LPRINT:LPRINT
670 IF td+tc<>VAL(rec.maandsaldo$)THEN LPRINT"JOURNAALPOSTEN NIET CONFORM MET REKENINGSTELSEL 1) DEBET=CREDIT ? 2) MANUELE WIJZIGINGEN AANGEBRACHT ? KONTAKTEER VSOFT !!!"
680 LPRINT CHR$(12);
690 d=0:c=0:td=0:tc=0:d=0:t=0:GOTO 240
700 RUN"menu.mar"
710 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
720 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 740 ELSE 730
730 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
740 GET 1,72:bl=VAL(rec.nummer$):RETURN
750 PRINT FNat$(23,0,"foutkode :");ERR
760 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
770 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 770
780 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 790 ELSE RESUME NEXT
790 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
$="" THEN 770
780 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 790 ELSE RESUME NEXT
790