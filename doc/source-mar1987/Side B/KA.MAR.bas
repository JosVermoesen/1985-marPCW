1 ON ERROR GOTO 20000
10 REM KA.MAR=KREATIE ARTIKELS DD
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
80 OPEN"r",1,drive$+"99.rnd",30
90 FIELD 1,28 AS rec.nummer$
100 GOSUB 1200:CLOSE 1
110 IF FIND$(dra$+"artikels.rnd")=""THEN GOSUB 1160
120 IF FIND$(drb$+"btw.rnd")=""THEN 130 ELSE 150
130 PRINT"EERST BTW KODES INBRENGEN A.U.B. !!!"
140 FOR T=1 TO 1000:NEXT t:GOTO 1150
150 BUFFERS 6
160 recleng=74:OPEN"K",1,dra$+"artikels.rnd",dra$+"artikels.key",2:recleng=4:OPEN"R",2,drb$+"btw.rnd",recleng
170 FIELD 1,25 AS rec.om$,9 AS rec.kostprijs$,2 AS rec.btw$,9 AS rec.jaaromzet$,9 AS rec.maandomzet$,9 AS rec.stock$,9 AS rec.minstock$:FIELD 2,2 AS rec.btwpct$
180 PRINT cls$
190 PRINT FNat$(0,0,"KREATIE ARTIKELS ====>"):PRINT FNat$(0,39,"<N>ieuwe,<O>pvragen,<W>ijzigen,<E>inde:O")
200 PRINT STRING$(80,CHR$(172))
210 PRINT FNat$(3,5,"1. ARTIKELNUMMER :"):PRINT FNat$(5,5,"2. OMSCHRIJVING  :"):PRINT FNat$(7,5,"3. KOSTPR./EENH. :")
220 PRINT FNat$(9,5,"4. B.T.W. KODE   :"):PRINT FNat$(11,5,"5. JAAROMZET     :"):PRINT FNat$(13,5,"6. MAANDOMZET    :")
230 PRINT FNat$(15,5,"7. STOCK         :"):PRINT FNat$(17,5,"8. MININUM STOCK :"):PRINT FNat$(22,0,STRING$(80,CHR$(172)))
240 PRINT hel$;:PRINT FNat$(0,78,"");:INPUT"",p$:p$=UPPER$(p$):PRINT zcht$
250 IF p$="N"THEN keuze=1 ELSE IF p$="O"OR p$=""THEN keuze=2 ELSE IF p$="W"THEN keuze=3 ELSE IF p$="A"THEN keuze=4 ELSE IF p$="E"THEN 1150 ELSE 240
260 ON keuze GOSUB 280,390,490,630
270 GOTO 180
280 REM invoer nieuw artikel
290 GOSUB 770:REM invoer artikelnummer
300 kontrole=SEEKKEY(1,2,0,nummer$)
310 IF kontrole=0 THEN 290
320 GOSUB 850:REM intijpen gegevens
330 GOSUB 1070:REM lset routine
340 kontrole=ADDREC(1,2,0,nummer$):a=FETCHREC(1):kontrole=ADDKEY(1,2,1,rec.naam$,a)
350 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
360 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 360
370 IF a$="V"THEN GOSUB 1130 ELSE 1150
380 GOTO 290
390 REM opvragen artikel
400 GOSUB 770:REM invoer artikelnummer
410 kontrole=SEEKKEY(1,2,0,nummer$)
420 IF kontrole<>0 THEN 400 ELSE GET 1
430 GOSUB 1100:REM variabele routine
440 GOSUB 800:REM artikelgegevens op scherm
450 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
460 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 460
470 IF a$="V"THEN GOSUB 1130 ELSE 1150
480 GOTO 400
490 REM wijzigen artikel
500 GOSUB 770:REM invoer artikelnummer
510 kontrole=SEEKKEY(1,2,0,nummer$)
520 IF kontrole<>0 THEN 500 ELSE GET 1
530 GOSUB 1100:REM variabele routine
540 naam2$=MID$(rec.om$,1,4)
550 GOSUB 800:REM gegevens op scherm
560 GOSUB 1010:REM wijzigen gegevens
570 GOSUB 1070:REM lset routine
580 PUT 1
590 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
600 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 600
610 IF a$="V"THEN GOSUB 1130 ELSE 1150
620 GOTO 500
630 REM annuleren artikel
640 GOSUB 770:REM invoer artikelnummer
650 kontrole=SEEKKEY(1,2,0,nummer$)
660 IF kontrole<>0 THEN 640 ELSE GET 1
670 GOSUB 1100:REM variabele routine
680 GOSUB 800:REM artikelgegevens op scherm
690 PRINT FNat$(23,0,"ANNULEREN (J/N) ?");eol$
700 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 700
710 IF a$="N"THEN 730
720 a=FETCHREC(1):kontrole=DELKEY(1,2):kontrole=SEEKKEY(1,2,1,rec.om$):kontrole=DELKEY(1,2,1,rec.om$,a):IF kontrole>103 THEN PRINT CHR$(7);
730 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
740 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 740
750 IF a$="V"THEN GOSUB 1130 ELSE 1150
760 GOTO 640
770 REM invoer artikelnummer
780 PRINT FNat$(3,24,"..........      "):PRINT hel$;:PRINT FNat$(3,24,"");:INPUT"",nummer$:PRINT zcht$;:IF nummer$=""THEN 1150
790 nummer=VAL(nummer$):IF LEN(nummer$)>10 THEN 780 ELSE RETURN
800 REM artikelgegevens op scherm
810 PRINT hel$;:PRINT FNat$(5,24,om$):PRINT FNat$(7,24,"");USING"######.##";VAL(kostprijs$):PRINT FNat$(9,24,"");USING"##";VAL(btw$):PRINT FNat$(11,24,"");USING"#########";VAL(jaaromzet$)
820 PRINT FNat$(13,24,"");USING"#########";VAL(maandomzet$):PRINT FNat$(15,24,"");USING"#########";VAL(stock$):PRINT FNat$(17,24,"");USING"#########";VAL(minstock$)
830 PRINT zcht$;:RETURN
840 REM intijpen gegevens
850 t=0
860 PRINT FNat$(5,24,".........................       "):PRINT hel$;::PRINT FNat$(5,24,"");:INPUT"",om$:PRINT zcht$;::IF LEN(om$)>25 THEN 860
870 IF t=1 THEN RETURN
880 PRINT hel$;:PRINT FNat$(7,24,"");:INPUT"",kostprijs$:PRINT zcht$;::kostprijs=VAL(kostprijs$)
890 IF t=1 THEN RETURN
900 PRINT hel$;:PRINT FNat$(9,24,"");:INPUT"",btw$:PRINT zcht$;::antw$="1234567":IF INSTR(antw$,btw$)=0 OR btw$=""THEN 900
910 GET 2,VAL(btw$):PRINT FNat$(9,30,rec.btwpct$);" %"
920 IF t=1 THEN RETURN
930 PRINT hel$;:PRINT FNat$(11,24,"");:INPUT"",jaaromzet$:PRINT zcht$;::jaaromzet=VAL(jaaromzet$):IF LEN(jaaromzet$)>9 THEN 930
940 IF t=1 THEN RETURN
950 PRINT hel$;:PRINT FNat$(13,24,"");:INPUT"",maandomzet$:PRINT zcht$;::maandomzet=VAL(maandomzet$):IF LEN(maandomzet$)>9 THEN 950
960 IF t=1 THEN RETURN
970 PRINT hel$;:PRINT FNat$(15,24,"");:INPUT"",stock$:PRINT zcht$;:stock=VAL(stock$):IF LEN(stock$)>9 THEN 970
980 IF t=1 THEN RETURN
990 PRINT hel$;:PRINT FNat$(17,24,"");:INPUT"",minstock$:PRINT zcht$;:minstock=VAL(minstock$):IF LEN(minstock$)>9 THEN 990
1000 IF t=1 THEN RETURN
1010 t=1
1020 PRINT FNat$(23,0,"GEEF <2><3><4><5><6><7><8> OM TE WIJZIGEN - <E> VOOR EINDE");eol$
1030 antw$="2345678E":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 1030
1040 IF a$="E"THEN RETURN
1050 ON VAL(a$)-1 GOSUB 860,880,900,930,950,970,990
1060 GOTO 1020
1070 REM routine variabele=>rec
1080 LSET rec.om$=om$:LSET rec.kostprijs$=kostprijs$:LSET rec.btw$=btw$:LSET rec.jaaromzet$=jaaromzet$:LSET rec.maandomzet$=maandomzet$:LSET rec.stock$=stock$:LSET rec.minstock$=minstock$
1090 RETURN
1100 REM routine rec=>variabele
1110 om$=rec.om$:kostprijs$=rec.kostprijs$:btw$=rec.btw$:jaaromzet$=rec.jaaromzet$:maandomzet$=rec.maandomzet$:stock$=rec.stock$:minstock$=rec.minstock$
1120 RETURN
1130 FOR t=3 TO 17 STEP 2:PRINT FNat$(t,24,eol$):NEXT t
1140 RETURN
1150 CLOSE 1:CLOSE 2:RUN"menu.mar"
1160 BUFFERS 6
1170 CREATE 1,dra$+"artikels.rnd",dra$+"artikels.key",2,74
1180 CLOSE 1
1190 RETURN
1200 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1210 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1230 ELSE 1220
1220 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1230 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN