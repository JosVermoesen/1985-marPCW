1 ON ERROR GOTO 20000
10 REM OVRLEV.MAR=OVERSCHRIJVINGEN LEVERANCIERS DD versie 04/04/87
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 DIM bedrag(100),naam$(100),straat$(100),stad$(100),rekening$(100),oms$(100)
80 BUFFERS 20
90 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
100 OPEN"R",1,drive$+"99.rnd",30
110 FIELD 1,6 AS rec.nummer$
120 GET 1,20:d9$=rec.nummer$:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2)
130 FIELD 1,28 AS rec.m$:GET 1,46:firm$=rec.m$:GET 1,47:strt$=rec.m$
140 FIELD 1,28 AS rec.nummer$:GOSUB 820:CLOSE 1
150 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
160 FIELD 1,6 AS rec.numlev$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
170 IF FIND$(draa$+"afactuur.rnd")=""THEN RUN"menu.mar"ELSE OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2
180 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
190 PRINT cls$:PRINT FNat$(0,0,"OVERSCHRIJVINGEN LEVERANCIERS MET KONTROLELIJST"):PRINT FNat$(1,0,STRING$(80,172))
200 PRINT FNat$(3,5,"MIN LEVERANCIERNUMMER : ......");eol$;hel$;:PRINT FNat$(3,29,"");:INPUT "",mini$:PRINT zcht$;:IF LEN(mini$)<>6 THEN 200
210 PRINT FNat$(4,5,"MAX LEVERANCIERNUMMER : ......");eol$;hel$;:PRINT FNat$(4,29,"");:INPUT "",maxi$:PRINT zcht$;:IF LEN(maxi$)<>6 THEN 210
220 PRINT FNat$(6,5,"LEVERANCIER :")
230 PRINT FNat$(7,0,"");:PRINT TAB(1);"FAKT.";TAB(8);"DATUM";TAB(19);"TOTAAL";TAB(28);"REEDS";TAB(37);"BETALING";TAB(46);"NOG TE";TAB(54);"VERVALDAG";TAB(64);"OMSCHRIJVING"
240 PRINT TAB(1);"NR.";TAB(19);"BEDRAG";TAB(28);"BETAALD";TAB(37);"STUK";TAB(46);"BETALEN":PRINT STRING$(80,"-"):PRINT:PRINT STRING$(80,"-"):PRINT FNat$(22,0,STRING$(79,172))
250 PRINT FNat$(14,2,"1. BEDRAG       :"):PRINT FNat$(15,2,"2. REKENING NR  :"):PRINT FNat$(16,2,"3. OMSCHRIJVING :")
260 kontrole=SEEKRANK(3,2,0)
270 IF mini$="" THEN 280 ELSE kontrole=SEEKKEY(3,2,0,mini$)
280 tot=0:ttot=0:nt=0:tnt=0:trb=0
290 a$=FETCHKEY$(3)
300 IF a$>maxi$ THEN 710
310 p=1
320 kontrole=SEEKKEY(3,2,0,a$):IF kontrole<>0 THEN 710 ELSE GET 3
330 kontrole=SEEKKEY(1,2,0,rec.nrl$):IF kontrole<>0 THEN STOP ELSE GET 1
340 tot=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+VAL(rec.invvhf$)+VAL(rec.sptaks$)-VAL(rec.btwmedektr$)
350 IF MID$(rec.boek$,1,2)="CA"THEN 690 ELSE IF tot=VAL(rec.rbet$) THEN 690
360 tnt=tot-VAL(rec.rbet$)
370 dd$=STR$((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))MOD 30)
380 mm$=STR$(VAL(MID$(rec.dat$,4,2))+INT((VAL(MID$(rec.dat$,1,2))+VAL(rec.vervaldag$))/30))
390 IF VAL(dd$)<10 THEN dd$="0"+MID$(dd$,2,1)ELSE dd$=MID$(dd$,2,2)
400 IF VAL(mm$)<10 THEN mm$="0"+MID$(mm$,2,1)ELSE mm$=MID$(mm$,2,2)
410 dat2$=dd$+"/"+mm$+"/"+MID$(rec.dat$,7,2)
420 PRINT hel$;:PRINT FNat$(10,0,"");TAB(1);rec.boek$;TAB(8);rec.dat$;TAB(17);USING"########";tot;:PRINT TAB(27);USING"########";VAL(rec.rbet$);:PRINT TAB(36);rec.bdok$;TAB(45);USING"########";tnt;:PRINT TAB(54);dat2$;TAB(64);MID$(rec.oms$,1,17)
430 PRINT FNat$(6,19,rec.naam$);zcht$;" OK (J/N) ? ";
440 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 440
450 IF at$="N" THEN 690
460 t=0
470 FOR pipo=14 TO 16:PRINT FNat$(pipo,20,eol$);:NEXT pipo
480 PRINT hel$;:PRINT FNat$(14,20,eol$);tnt:PRINT FNat$(14,50,"");zcht$;" OK (J/N) ? ";
490 GOSUB 800
500 IF at$="J" THEN bedrag(ar+1)=tnt:GOTO 520
510 PRINT hel$;:PRINT FNat$(14,20,eol$);:INPUT "",bedrag$:bedrag(ar+1)=VAL(bedrag$):PRINT zcht$;
520 IF t=1 THEN RETURN
530 PRINT FNat$(15,20,eol$);hel$;:PRINT FNat$(15,20,rec.rekening$):PRINT FNat$(15,50,"");zcht$;" OK (J/N) ? ";
540 GOSUB 800
550 IF at$="J" THEN rekening$(ar+1)=rec.rekening$:GOTO 570
560 PRINT FNat$(15,20,"...-.......-..");eol$;hel$;:PRINT FNat$(15,20,"");:INPUT "",rekening$(ar+1):PRINT zcht$;
570 IF t=1 THEN RETURN
580 PRINT FNat$(16,20,eol$);hel$;:PRINT FNat$(16,20,rec.oms$):PRINT FNat$(16,50,"");zcht$;" OK (J/N) ? ";
590 GOSUB 800
600 IF at$="J" THEN oms$(ar+1)=rec.oms$:GOTO 620
610 PRINT hel$;:PRINT FNat$(16,20,eol$);:INPUT "",oms$(ar+1):PRINT zcht$;:IF LEN(oms$(ar+1))>20 THEN 610
620 t=1
630 PRINT FNat$(23,0,"DRUK NR OM TE WIJZIGEN - <V>ERVOLG - <A>NNULEREN ");eol$;
640 antw$="123AV":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 640
650 IF at$="V" THEN 680 ELSE IF at$="A" THEN 690
660 ON VAL(at$) GOSUB 480,530,580
670 FOR pipo=14 TO 16:PRINT FNat$(pipo,50,eol$):NEXT pipo:GOTO 630
680 ar=ar+1:naam$(ar)=rec.naam$:stad$(ar)=rec.stad$:straat$(ar)=rec.straat$
690 kontrole=SEEKNEXT(3,0):IF kontrole<>0 THEN 280 ELSE GET 3
700 GOTO 340
710 PRINT FNat$(23,0,"EINDE BESTAND : <M>ENU - <K>ONTROLELIJST DRUKKEN ? DRUK DE JUISTE TOETS");eol$;
720 antw$="MK":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 720
730 IF at$="M" THEN 790
740 GOSUB 860
750 PRINT FNat$(23,0,"<M>ENU - <O>VERSCHRIJVINGSFORMULIEREN DRUKKEN ? ");eol$;
760 antw$="MO":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 760 
770 IF at$="M" THEN 790
780 GOSUB 1150
790 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar"
800 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 800
810 RETURN
820 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
830 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 850 ELSE 840
840 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
850 RETURN
860 REM
870 z$=STRING$(132,"-")
880 LPRINT CHR$(27);"C";CHR$(72);
890 LPRINT CHR$(27);"N";CHR$(1);
900 PRINT FNat$(23,0,"GECONDENSEERD AFDRUKKEN (J/N) ? ");eol$;
910 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 910
920 IF at$="J" THEN LPRINT CHR$(15); ELSE LPRINT CHR$(18);
930 t=0:pteller=0
940 REM uitdrukken hoofdding
950 pteller=pteller+1:lteller=0
960 LPRINT
970 LPRINT
980 tp$="KONTROLELIJST OVERSCHRIJVINGEN LEVERANCIERS"
990 LPRINT CHR$(14);tp$;" ";bedrijf$:LPRINT TAB(120);"PAGINA : ";USING"###";pteller:LPRINT TAB(116);"DATUM : ";d$:LPRINT
1000 LPRINT z$:LPRINT"NAAM";TAB(31);"STRAAT EN NR";TAB(61);"WOONPLAATS";TAB(91);"REK. NUMMER";TAB(106);"BEDRAG";TAB(116);"OMSCHRIJVING"
1010 LPRINT z$:LPRINT
1020 IF t<>0 THEN RETURN
1030 REM afdrukken journaalposten
1040 FOR pipo=1 TO ar
1050 altot=altot+bedrag(pipo)
1060 lteller=lteller+1
1070 t=t+1:LPRINT naam$(pipo);TAB(31);straat$(pipo);TAB(61);stad$(pipo);TAB(91);rekening$(pipo);TAB(106);USING"#########";bedrag(pipo);:LPRINT TAB(116);MID$(oms$(pipo),1,16)
1080 IF lteller<55 THEN 1100
1090 LPRINT CHR$(12);:GOSUB 950
1100 NEXT pipo
1110 LPRINT z$:LPRINT TAB(5);"TOTAAL AANTAL BETALINGEN :";USING"####";t;:LPRINT TAB(106);USING"#########";altot
1120 LPRINT CHR$(12);
1130 ts=0:tj=0
1140 RETURN
1150 LPRINT CHR$(27);"C";CHR$(24);
1160 LPRINT CHR$(27);"N";CHR$(1);
1170 LPRINT CHR$(18);
1180 FOR pipo=1 TO ar
1190 LPRINT:LPRINT:LPRINT:LPRINT:LPRINT:LPRINT:LPRINT TAB(26);rekening$(pipo);TAB(48);bedrag(pipo)
1200 LPRINT:LPRINT:LPRINT TAB(31);MID$(naam$(pipo),1,26)
1210 LPRINT:LPRINT TAB(31);MID$(straat$(pipo),1,26)
1220 LPRINT:LPRINT TAB(31);MID$(stad$(pipo),1,26)
1230 LPRINT:LPRINT TAB(4);oms$(pipo)
1240 LPRINT CHR$(12);
1250 NEXT pipo
1260 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF 