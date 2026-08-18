1 ON ERROR GOTO 20000
10 REM KK.MAR=KREATIE KLANTEN DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN:DIM btnum$(100)
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
80 OPEN"r",1,drive$+"99.rnd",30
90 FIELD 1,28 AS rec.nummer$
100 GOSUB 1380:CLOSE 1
110 IF FIND$(drk$+"klanten.rnd")=""THEN GOSUB 1340
120 BUFFERS 20
130 recleng=162:OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2,recleng
140 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
150 PRINT cls$
160 PRINT FNat$(0,0,"KREATIE KLANTEN ====>"):PRINT FNat$(0,39,"<N>ieuwe,<O>pvragen,<W>ijzigen,<E>inde:O")
170 PRINT STRING$(80,CHR$(172))
180 PRINT FNat$(3,5,"   NUMMER KLANT  :"):PRINT:PRINT SPACE$(5);"1. NAAM          :":PRINT SPACE$(5);"2. STRAAT EN NR  :":PRINT SPACE$(5);"3. POSTKODE      :"
190 PRINT SPACE$(5);"4. STAD          :":PRINT:PRINT SPACE$(5);"5. TELEFOON      :":PRINT:PRINT SPACE$(5);"6. B.T.W. NR.    :"
200 PRINT:PRINT SPACE$(5);"7. B.T.W. BEDRAG :":PRINT:PRINT SPACE$(5);"8. JAAROMZET     :":PRINT:PRINT SPACE$(5);"9. SALDO         :":PRINT:PRINT SPACE$(5);"0. VERVALDAG     :":PRINT FNat$(22,0,STRING$(80,CHR$(172)))
210 PRINT hel$;:PRINT FNat$(0,78,"");:INPUT"",p$:p$=UPPER$(p$):PRINT zcht$
220 IF p$="N"THEN keuze=1 ELSE IF p$="O"OR p$=""THEN keuze=2 ELSE IF p$="W"THEN keuze=3 ELSE IF p$="A"THEN keuze=4 ELSE IF p$="E"THEN 1330 ELSE 210
230 ON keuze GOSUB 250,370,480,680
240 GOTO 150
250 REM invoer nieuwe klant
260 GOSUB 840:REM invoer klantnummer
270 IF nummer$="" THEN RETURN
280 kontrole=SEEKKEY(1,2,0,nummer$)
290 IF kontrole=0 THEN 260
300 GOSUB 960:REM intijpen gegevens
310 GOSUB 1240:REM lset routine
320 kontrole=ADDREC(1,2,0,nummer$):a=FETCHREC(1):kontrole=ADDKEY(1,2,1,MID$(rec.naam$,1,4),a):kontrole=ADDKEY(1,2,2,rec.postkode$,a)
330 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
340 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 340
350 IF a$="V"THEN GOSUB 1310 ELSE RETURN
360 GOTO 260
370 REM opvragen klant
380 GOSUB 840:REM invoer klantnummer
390 IF nummer$="" THEN RETURN
400 kontrole=SEEKKEY(1,2,0,nummer$)
410 IF kontrole<>0 THEN 380 ELSE GET 1
420 GOSUB 1280:REM variabele routine
430 GOSUB 890:REM klantgegevens op scherm
440 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
450 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 450
460 IF a$="V"THEN GOSUB 1310 ELSE RETURN
470 GOTO 380
480 REM wijzigen klant
490 GOSUB 840:REM invoer klantnummer
500 IF nummer$="" THEN RETURN
510 kontrole=SEEKKEY(1,2,0,nummer$)
520 IF kontrole<>0 THEN 490 ELSE GET 1
530 GOSUB 1280:REM variabele routine
540 naam2$=MID$(rec.naam$,1,4):nrpc2$=rec.postkode$
550 GOSUB 890:REM gegevens op scherm
560 GOSUB 1170:REM wijzigen gegevens
570 GOSUB 1240:REM lset routine
580 PUT 1
590 IF naam2$=MID$(rec.naam$,1,4) AND nrpc2$=rec.postkode$ THEN 630
600 record=FETCHREC(1):key$=naam2$:file%=1:rank%=1:GOSUB 1430:GOSUB 1450
610 key$=nrpc2$:rank%=2:GOSUB 1430:GOSUB 1450
620 key$=MID$(rec.naam$,1,4):rank%=1:GOSUB 1480:key$=rec.postkode$:rank%=2:GOSUB 1480
630 kontrole=CONSOLIDATE(1)
640 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
650 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 650
660 IF a$="V"THEN GOSUB 1310 ELSE RETURN
670 GOTO 490
680 REM annuleren klant
690 GOSUB 840:REM invoer klantnummer
700 IF nummer$="" THEN RETURN
710 kontrole=SEEKKEY(1,2,0,nummer$)
720 IF kontrole<>0 THEN 690 ELSE GET 1
730 GOSUB 1280:REM variabele routine
740 GOSUB 890:REM klantgegevens op scherm
750 PRINT FNat$(23,0,"ANNULEREN (J/N) ?");eol$;
760 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 760
770 IF a$="N"THEN 800
780 record=FETCHREC(1):kontrole=DELKEY(1,2):rank%=1:file%=1:key$=MID$(rec.naam$,1,4):GOSUB 1420:GOSUB 1450
790 rank%=2:key$=rec.postkode$:GOSUB 1420:GOSUB 1450
800 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
810 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 810
820 IF a$="V"THEN GOSUB 1310 ELSE RETURN
830 GOTO 690
840 REM invoer klantnummer
850 PRINT FNat$(3,24,"400......      "):PRINT hel$;:PRINT FNat$(3,27,"");:INPUT"",nummer$:PRINT zcht$;:IF nummer$="" THEN RETURN ELSE IF LEN(nummer$)<>6 THEN 850
860 IF nummer$="//////" AND keuze<>1 THEN GOSUB 1510 ELSE 870
870 IF LEN(nummer$)>6 OR nummer$="//////" THEN 850 ELSE RETURN
880 REM
890 REM klantgegevens op scherm
900 PRINT hel$;:PRINT FNat$(3,27,nummer$):PRINT FNat$(5,24,naam$):PRINT FNat$(6,24,straat$):PRINT FNat$(7,24,postkode$):PRINT FNat$(8,24,stad$)
910 PRINT FNat$(10,24,telefoon$)
920 PRINT FNat$(12,24,btwnr$):PRINT FNat$(14,24,"");USING"#########";VAL(btwbedrag$):PRINT FNat$(16,24,"");USING"#########";VAL(jaaromzet$)
930 PRINT FNat$(18,24,"");USING"#########";VAL(saldo$):PRINT FNat$(20,24,"");USING"###";VAL(vervaldag$)
940 PRINT zcht$;:RETURN
950 REM intijpen gegevens
960 t=0
970 PRINT FNat$(5,24,"..............................       ");hel$;:PRINT FNat$(5,24,"");:INPUT"",naam$:PRINT zcht$;:IF LEN(naam$)>30 THEN 970
980 IF t=1 THEN RETURN
990 PRINT FNat$(6,24,"..............................     ");hel$;:PRINT FNat$(6,24,"");:INPUT"",straat$:PRINT zcht$;:IF LEN(straat$)>30 THEN 990
1000 IF t=1 THEN RETURN
1010 PRINT FNat$(7,24,".....    ");hel$;:PRINT FNat$(7,24,"");:INPUT"",postkode$:PRINT zcht$;:IF LEN(postkode$)>5 THEN 1010
1020 IF t=1 THEN RETURN
1030 PRINT FNat$(8,24,".............................     ");hel$;:PRINT FNat$(8,24,"");:INPUT"",stad$:PRINT zcht$;:IF LEN(stad$)>30 THEN 1030
1040 IF t=1 THEN RETURN
1050 PRINT FNat$(10,24,"............   "):PRINT FNat$(10,24,"");hel$;:INPUT"",telefoon$:PRINT zcht$;:IF LEN(telefoon$)>12 THEN 1050
1060 IF t=1 THEN RETURN
1070 PRINT FNat$(12,24,"...-...-...    ");hel$;:PRINT FNat$(12,24,"");:INPUT"",btwnr$:PRINT zcht$;:q=q+1:btnum$(q)=btwnr$:IF LEN(btwnr$)>11 THEN 1070 ELSE IF LEN(btwnr$)<2 THEN 1090
1080 IF btnum$(q-1)=btwnr$THEN 1090 ELSE IF CINT(97-(VAL(MID$(btwnr$,1,3)+MID$(btwnr$,5,3)+MID$(btwnr$,9,1))/97-INT(VAL(MID$(btwnr$,1,3)+MID$(btwnr$,5,3)+MID$(btwnr$,9,1))/97))*97)<>VAL(MID$(btwnr$,10,2))THEN 1070
1090 IF t=1 THEN RETURN
1100 PRINT FNat$(14,24,"");hel$;:INPUT"",btwbedrag$:PRINT zcht$;:btwbedrag=VAL(btwbedrag$):IF LEN(btwbedrag$)>9 THEN 1100
1110 IF t=1 THEN RETURN
1120 PRINT FNat$(16,24,"");hel$;:INPUT"",jaaromzet$:PRINT zcht$;:jaaromzet=VAL(jaaromzet$):IF LEN(jaaromzet$)>9 THEN 1120
1130 IF t=1 THEN RETURN
1140 PRINT FNat$(18,24,"");hel$;:INPUT"",saldo$:PRINT zcht$;:saldo=VAL(saldo$):IF LEN(saldo$)>9 THEN 1140
1150 IF t=1 THEN RETURN
1160 PRINT FNat$(20,24,"");hel$;:INPUT"",vervaldag$:PRINT zcht$;:antw$="8306090120":IF INSTR(antw$,vervaldag$)=0 OR LEN(vervaldag$)>3 THEN 1160
1170 t=1
1180 PRINT FNat$(23,0,"GEEF <1><2><3><4><5><6><7><8><9><0> OM TE WIJZIGEN - <E> VOOR EINDE");eol$;
1190 antw$="1234567890E":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 1190
1200 IF a$="E"THEN RETURN
1210 IF VAL(a$)=0 THEN 1160
1220 PRINT hel$;:ON VAL(a$)GOSUB 970,990,1010,1030,1050,1070,1100,1120,1140
1230 GOTO 1180
1240 REM routine variabele=>rec
1250 LSET rec.naam$=naam$:LSET rec.straat$=straat$:LSET rec.postkode$=postkode$:LSET rec.stad$=stad$:LSET rec.telefoon$=telefoon$:LSET rec.btwnr$=btwnr$:LSET rec.btwbedrag$=btwbedrag$:LSET rec.jaaromzet$=jaaromzet$:LSET rec.saldo$=saldo$
1260 LSET rec.numkl$=nummer$:LSET rec.vervaldag$=vervaldag$
1270 RETURN
1280 REM routine rec=>variabele
1290 naam$=rec.naam$:straat$=rec.straat$:postkode$=rec.postkode$:stad$=rec.stad$:telefoon$=rec.telefoon$:btwnr$=rec.btwnr$:btwbedrag$=rec.btwbedrag$:jaaromzet$=rec.jaaromzet$:saldo$=rec.saldo$:vervaldag$=rec.vervaldag$
1300 nummer$=rec.numkl$:RETURN
1310 PRINT FNat$(3,27,eol$):FOR t=5 TO 20:PRINT FNat$(t,24,eol$):NEXT t
1320 RETURN
1330 PRINT zcht$;:CLOSE 1:RUN"menu.mar"
1340 BUFFERS 20
1350 CREATE 1,drk$+"klanten.rnd",drk$+"klanten.key",2,162
1360 CLOSE 1
1370 RETURN
1380 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1390 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1410 ELSE 1400
1400 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1410 RETURN
1420 REM subroutine SEEKKEY
1430 kontrole=SEEKKEY(file%,2,rank%,key$):IF kontrole<>0 THEN  PRINT CHR$(7); 
1440 RETURN
1450 REM subroutine DELKEY
1460 kontrole=DELKEY(file%,2,rank%,key$,record):IF kontrole>103 THEN PRINT CHR$(7);
1470 RETURN
1480 REM subroutine ADDKEY
1490 kontrole=ADDKEY(file%,2,rank%,key$,record)
1500 RETURN
1510 PRINT FNat$(23,0,"GEEF DE EERSTE 4 LETTERS VAN DE TE ZOEKEN NAAM");eol$:PRINT FNat$(5,24,"");hel$;:INPUT "",alfa$:PRINT zcht$;:IF alfa$="" THEN nummer$="10000000":RETURN ELSE 1520
1520 kontrole=SEEKKEY(1,2,1,alfa$):IF kontrole=0 THEN 1540
1530 PRINT FNat$(23,0,"GEEN KLANTEN MET DEZE BEGINLETTERS !!");eol$;:GOSUB 1310:FOR t=1 TO 1000:NEXT t:GOTO 1510 ELSE 1540
1540 GET 1:GOSUB 1280:GOSUB 890
1550 PRINT FNat$(23,0,"<O>K ? - <V>ERDER ZOEKEN ? : DRUK DE JUISTE TOETS");eol$;
1560 antw$="OoVv":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$="" THEN 1560
1570 IF a$="O" THEN RETURN
1580 kontrole=SEEKNEXT (1,1):IF kontrole<>0 THEN PRINT FNat$(23,0,"GEEN KLANTEN MEER MET DEZE BEGINLETTERS !!");eol$:GOSUB 1310:FOR t=1 TO 1000:NEXT t:GOTO 1510 ELSE 1540
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2: