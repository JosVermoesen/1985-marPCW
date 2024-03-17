1 ON ERROR GOTO 20000
10 REM KR.MAR=KREATIE ALGEMENE REKENINGEN
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN"r",1,drive$+"99.rnd",30
100 FIELD 1,28 AS rec.nummer$:GOSUB 1090:CLOSE 1:OPEN"r",2,drt$+"99.rnd",30:FIELD 2,6 AS rec.nummer$
110 IF FIND$(drr$+"rekening.rnd")=""THEN GOSUB 1050
120 BUFFERS 6
130 OPEN"K",1,drr$+"rekening.rnd",drr$+"rekening.key",2
140 FIELD 1,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
150 PRINT cls$
160 PRINT FNat$(0,0,"KREATIE REKENINGEN ====>"):PRINT FNat$(0,39,"<N>ieuwe,<O>pvragen,<W>ijzigen,<E>inde:O")
170 PRINT STRING$(80,CHR$(172))
180 PRINT FNat$(5,5,"   REKENING NUMMER :"):PRINT FNat$(7,5,"2. OMSCHRIJVING    :"):PRINT FNat$(9,5,"3. JAARSALDO       : ")
190 PRINT FNat$(11,5,"4. PERIODIEK SALDO : "):PRINT FNat$(22,0,STRING$(80,CHR$(172)))
200 PRINT hel$;:PRINT FNat$(0,78,"");:INPUT"",p$:p$=UPPER$(p$):PRINT zcht$;
210 IF p$="N"THEN keuze=1 ELSE IF p$="O"OR p$=""THEN keuze=2 ELSE IF p$="W"THEN keuze=3 ELSE IF p$="A"THEN keuze=4 ELSE IF p$="E"THEN 1040 ELSE 200
220 ON keuze GOSUB 240,350,450,580
230 GOTO 150
240 REM invoer nieuwe rekening
250 GOSUB 770:REM invoer rekeningnummer
260 kontrole=SEEKKEY(1,2,0,nummer$)
270 IF kontrole=0 THEN 250
280 GOSUB 840:REM intijpen gegevens
290 GOSUB 960:REM lset routine
300 kontrole=ADDREC(1,2,0,nummer$)
310 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
320 antw$="EV":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 320
330 IF a$="V"THEN GOSUB 1020 ELSE 1040
340 GOTO 250
350 REM opvragen rekening
360 GOSUB 770:REM invoer rekeningnummer
370 kontrole=SEEKKEY(1,2,0,nummer$)
380 IF kontrole<>0 THEN 360 ELSE GET 1
390 GOSUB 990:REM variabele routine
400 GOSUB 800:REM rekeninggegevens op scherm
410 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
420 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 420
430 IF a$="V"THEN GOSUB 1020 ELSE 1040
440 GOTO 360
450 REM wijzigen rekening
460 GOSUB 770:REM invoer rekeningnummer
470 kontrole=SEEKKEY(1,2,0,nummer$)
480 IF kontrole<>0 THEN 460 ELSE GET 1
490 GOSUB 990:REM variabele routine
500 GOSUB 800:REM gegevens op scherm
510 GOSUB 900:REM wijzigen gegevens
520 GOSUB 960:REM lset routine
530 PUT 1
540 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
550 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 550
560 IF a$="V"THEN GOSUB 1020 ELSE 1040
570 GOTO 460
580 REM annuleren rekening
590 GOSUB 770:REM invoer rekeningnummer
600 kontrole=SEEKKEY(1,2,0,nummer$)
610 IF kontrole<>0 THEN 590 ELSE GET 1
620 GOSUB 990:REM variabele routine
630 GOSUB 800:REM rekeninggegevens op scherm
640 PRINT FNat$(23,0,"ANNULEREN (J/N) ?");eol$
650 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 650
660 IF a$="N"THEN 730
670 flag=0
680 FOR zoek=7 TO 45:GET 2,zoek:IF rec.nummer$=nummer$THEN flag=1
690 NEXT zoek
700 IF flag<>1 THEN 720 ELSE PRINT hel$;:PRINT FNat$(23,0,"REKENING KAN VIA DEZE ROUTINE NIET VERNIETIGD WORDEN!!!");eol$:PRINT zcht$;
710 FOR t=1 TO 5000:NEXT t:RETURN
720 kontrole=DELKEY(1,2)
730 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
740 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 740
750 IF a$="V"THEN GOSUB 1020 ELSE 1040
760 GOTO 590
770 REM invoer rekeningnummer
780 PRINT FNat$(5,26,"......      ");hel$;:PRINT FNat$(5,26,"");:INPUT"",nummer$:PRINT zcht$;:IF nummer$=""THEN 1040 ELSE IF LEN(nummer$)<>6 THEN 780
790 nummer=VAL(nummer$):IF nummer>999999! OR nummer<100000! THEN 780 ELSE RETURN
800 REM rekeninggegevens op scherm
810 PRINT hel$;:PRINT FNat$(7,26,om$):PRINT FNat$(9,26,"");USING"#########";VAL(jaarsaldo$):PRINT FNat$(11,26,"");USING"#########";VAL(maandsaldo$)
820 PRINT zcht$;:RETURN
830 REM intijpen gegevens
840 t=0
850 PRINT FNat$(7,26,"..............................       ");hel$;:PRINT FNat$(7,26,"");:INPUT"",om$:IF INSTR(MID$(om$,1,2),".")<>0 OR LEN(om$)>30 THEN 850 ELSE PRINT zcht$;
860 IF t=1 THEN RETURN
870 PRINT FNat$(9,40,"");hel$;:INPUT"",jaarsaldo:PRINT zcht$;:jaarsaldo$=STR$(jaarsaldo):IF LEN(jaarsaldo$)>9 THEN 870
880 IF t=1 THEN RETURN
890 PRINT FNat$(11,40,"");hel$;:INPUT"",maandsaldo:PRINT zcht$;:maandsaldo$=STR$(maandsaldo):IF LEN(maandsaldo$)>9 THEN 890
900 t=1
910 PRINT FNat$(23,0,"GEEF <2><3><4> OM TE WIJZIGEN - <E> VOOR EINDE");eol$
920 antw$="234E":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 920
930 IF a$="E"THEN RETURN
940 ON VAL(a$)-1 GOSUB 850,870,890
950 GOTO 910
960 REM routine variabele=>rec
970 LSET rec.om$=om$:LSET rec.jaarsaldo$=jaarsaldo$:LSET rec.maandsaldo$=maandsaldo$:LSET rec.telhistor$=telhistor$
980 RETURN
990 REM routine rec=>variabele
1000 om$=rec.om$:jaarsaldo$=rec.jaarsaldo$:maandsaldo$=rec.maandsaldo$:telhistor$=rec.telhistor$
1010 RETURN
1020 FOR t=5 TO 11 STEP 2:PRINT FNat$(t,26,eol$):NEXT t
1030 RETURN
1040 CLOSE 1:CLOSE 2:RUN"menu.mar"
1050 BUFFERS 6
1060 CREATE 1,drr$+"rekening.rnd",drr$+"rekening.key",2
1070 CLOSE 1
1080 RETURN
1090 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1100 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1120 ELSE 1110
1110 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1120 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" 