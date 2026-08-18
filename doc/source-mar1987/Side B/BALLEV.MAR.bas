1 ON ERROR GOTO 20000
10 REM BALLEV.MAR=UITDRUKKEN BALANS LEVERANCIERS DD versie 230687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
60 OPTION RUN
70 DIM m1$(30),btkrt(30),btld(30),oms$(30),m2$(30),nrr$(50)
80 BUFFERS 20
90 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
130 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
140 FIELD 1,28 AS rec.nummer$:GOSUB 650:CLOSE 1
145 IF bl=72 THEN lnbl=50 ELSE lnbl=44
150 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
160 FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
170 IF FIND$(draa$+"afactuur.rnd")=""THEN RUN"menu.mar"ELSE OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2
180 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
190 PRINT cls$:PRINT FNat$(0,0,"UITDRUKKEN BALANS LEVERANCIERS"):PRINT FNat$(1,0,STRING$(80,172))
200 PRINT FNat$(12,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(12,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
210 PRINT FNat$(14,12,"MIN LEVERANCIERNUMMER : ......");eol$;hel$;:PRINT FNat$(14,36,"");:INPUT"",mini$:PRINT zcht$;:IF LEN(mini$)<>6 THEN 210
220 PRINT FNat$(15,12,"MAX LEVERANCIERNUMMER : ......");eol$;hel$;:PRINT FNat$(15,36,"");:INPUT"",maxi$:PRINT zcht$;:IF LEN(maxi$)<>6 THEN 220
230 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
240 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 240
250 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
260 PRINT FNat$(19,5,"PRINTER EN PAPIER KLAAR ? DRUK <V> VOOR VERVOLG <M> VOOR MENU");
270 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 270
280 IF at$="M"THEN 640
290 LPRINT CHR$(27);"C";CHR$(bl);
300 LPRINT CHR$(27);"N";CHR$(1);
310 tp$="BALANS LEVERANCIERS"
320 pteller=pteller+1
330 LPRINT CHR$(14);tp$;" ";firm$:LPRINT TAB(120);"PAGINA : ";USING"##";pteller:LPRINT TAB(116);"DATUM : ";d$:LPRINT
340 z$=STRING$(132,"-")
350 LPRINT:LPRINT z$:LPRINT TAB(1);"NUMMER";TAB(8);"NAAM LEVERANCIER";TAB(33);"FAKT.";TAB(40);"DATUM";TAB(49);"TOTAAL";TAB(59);"REEDS";TAB(69);"BETALING";TAB(79);"NOG TE";TAB(89);"VERVALDAG";TAB(99);"CUMUL"
360 LPRINT TAB(33);"NR.";TAB(49);"BEDRAG";TAB(59);"BETAALD";TAB(69);"STUK";TAB(79);"BETALEN"
370 LPRINT z$:LPRINT
380 IF p<>0 THEN RETURN
390 kontrole=SEEKRANK(3,2,0)
400 IF mini$=""THEN 410 ELSE kontrole=SEEKKEY(3,2,0,mini$)
410 tot=0:ttot=0:nt=0:tnt=0:trb=0
420 a$=FETCHKEY$(3)
430 IF a$>maxi$THEN 630
440 p=1
450 kontrole=SEEKKEY(3,2,0,a$):IF kontrole<>0 THEN 630 ELSE GET 3
460 kontrole=SEEKKEY(1,2,0,rec.nrl$):IF kontrole<>0 THEN STOP ELSE GET 1
470 t=t+1:LPRINT:LPRINT TAB(1);a$;TAB(8);MID$(rec.naam$,1,20);
480 tot=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+VAL(rec.invvhf$)+VAL(rec.sptaks$)-VAL(rec.btwmedektr$)
490 IF MID$(rec.boek$,1,2)="CA"THEN tot=-tot
500 ttot=ttot+tot:nt=tot-VAL(rec.rbet$):tnt=tnt+nt:trb=trb+VAL(rec.rbet$):vtot=vtot+tot-VAL(rec.rbet$)
510 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
520 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
530 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
540 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
550 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
560 IF tot=VAL(rec.rbet$)THEN dat2$=""
570 LPRINT TAB(33);rec.boek$;TAB(40);rec.dat$;TAB(49);USING"#########";tot;:LPRINT TAB(59);USING"#########";VAL(rec.rbet$);:LPRINT TAB(69);rec.bdok$;TAB(79);USING"#########";tot-VAL(rec.rbet$);:LPRINT TAB(89);dat2$;:LPRINT TAB(99);USING"#########";tnt
580 t=t+1
590 IF t>lnbl THEN t=0 ELSE 610
600 LPRINT CHR$(12);:GOSUB 320
610 kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 410 ELSE GET 3
620 GOTO 480
630 LPRINT z$:LPRINT"N O G   T E   B E T A L E N : ";TAB(99);USING"#########";vtot:LPRINT CHR$(12);
640 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
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
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE