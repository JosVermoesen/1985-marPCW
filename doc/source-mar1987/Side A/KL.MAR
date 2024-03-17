1 ON ERROR GOTO 20000
10 REM KL.MAR=KREATIE LEVERANCIERS DD versie 13/07/87
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
70 OPTION RUN
80 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN"r",1,drive$+"99.rnd",30
100 FIELD 1,28 AS rec.nummer$:GOSUB 1310:CLOSE 1
110 DIM reknum$(100)
120 IF FIND$(drl$+"leveranc.rnd")=""THEN GOSUB 1270
130 BUFFERS 20
140 recleng=152
150 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2,recleng
160 FIELD 1,6 AS rec.levnum$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
170 PRINT cls$
180 PRINT FNat$(0,0,"KREATIE LEVERANCIERS ====>"):PRINT FNat$(0,39,"<N>ieuwe,<O>pvragen,<W>ijzigen,<E>inde:O")
190 PRINT STRING$(80,CHR$(172))
200 PRINT FNat$(3,5,"1. NUMMER        :"):PRINT FNat$(5,5,"2. NAAM          :"):PRINT FNat$(6,5,"3. STRAAT EN NR  :")
210 PRINT FNat$(7,5,"4. STAD          :"):PRINT FNat$(9,5,"5. TELEFOON      :"):PRINT FNat$(11,5,"6. JAAROMZET     :")
220 PRINT FNat$(13,5,"7. SALDO         :"):PRINT FNat$(15,5,"8. BANKREKENING  :"):PRINT FNat$(17,5,"9. VERVALDAG     :"):PRINT FNat$(22,0,STRING$(80,CHR$(172)))
230 PRINT hel$;:PRINT FNat$(0,78,"");:INPUT"",p$:p$=UPPER$(p$):PRINT zcht$;
240 IF p$="N"THEN keuze=1 ELSE IF p$="O"OR p$=""THEN keuze=2 ELSE IF p$="W"THEN keuze=3 ELSE IF p$="A"THEN keuze=4 ELSE IF p$="E"THEN 1260 ELSE 230
250 ON keuze GOSUB 270,390,500,690
260 GOTO 170
270 REM invoer nieuwe leverancier
280 GOSUB 850:REM invoer leveranciernummer
290 IF nummer$=""THEN RETURN
300 kontrole=SEEKKEY(1,2,0,nummer$)
310 IF kontrole=0 THEN 280
320 GOSUB 950:REM intijpen gegevens
330 GOSUB 1180:REM lset routine
340 kontrole=ADDREC(1,2,0,nummer$):record=FETCHREC(1):kontrole=ADDKEY(1,2,1,MID$(rec.naam$,1,4),record)
350 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
360 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 360
370 IF a$="V"THEN GOSUB 1240 ELSE RETURN
380 GOTO 280
390 REM opvragen leverancier
400 GOSUB 850:REM invoer leveranciernummer
410 IF nummer$=""THEN RETURN
420 kontrole=SEEKKEY(1,2,0,nummer$)
430 IF kontrole<>0 THEN 400 ELSE GET 1
440 GOSUB 1210:REM variabele routine
450 GOSUB 900:REM leveranciergegevens op scherm
460 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
470 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 470
480 IF a$="V"THEN GOSUB 1240 ELSE RETURN
490 GOTO 400
500 REM wijzigen leverancier
510 GOSUB 850:REM invoer leveranciernummer
520 IF nummer$=""THEN RETURN
530 kontrole=SEEKKEY(1,2,0,nummer$)
540 IF kontrole<>0 THEN 510 ELSE GET 1
550 GOSUB 1210:REM variabele routine
560 naam2$=MID$(rec.naam$,1,4)
570 GOSUB 900:REM gegevens op scherm
580 GOSUB 1120:REM wijzigen gegevens
590 GOSUB 1180:REM lset routine
600 PUT 1
610 IF naam2$=MID$(rec.naam$,1,4)THEN 640
620 record=FETCHREC(1):key$=naam2$:file%=1:rank%=1:GOSUB 1530:GOSUB 1550
630 key$=MID$(rec.naam$,1,4):rank%=1:GOSUB 1580
640 kontrole=CONSOLIDATE(1)
650 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
660 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 660
670 IF a$="V"THEN GOSUB 1240 ELSE RETURN
680 GOTO 510
690 REM annuleren leverancier
700 GOSUB 850:REM invoer leveranciernummer
710 IF nummer$=""THEN RETURN
720 kontrole=SEEKKEY(1,2,0,nummer$)
730 IF kontrole<>0 THEN 700 ELSE GET 1
740 GOSUB 1210:REM variabele routine
750 GOSUB 900:REM leveranciergegevens op scherm
760 PRINT FNat$(23,0,"ANNULEREN (J/N) ?");eol$;
770 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 770
780 IF a$="N"THEN 810
790 record=FETCHREC(1):kontrole=DELKEY(1,2):rank%=1:file%=1:key$=MID$(rec.naam$,1,4):GOSUB 1520:GOSUB 1550
810 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$;
820 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 820
830 IF a$="V"THEN GOSUB 1240 ELSE RETURN
840 GOTO 700
850 REM invoer leveranciernummer
860 PRINT FNat$(3,24,"440......      ");hel$;:PRINT FNat$(3,27,"");:INPUT"",nummer$:PRINT zcht$;:IF nummer$=""THEN RETURN ELSE IF LEN(nummer$)<>6 THEN 860
870 IF nummer$="//////"AND keuze<>1 THEN GOSUB 1350 ELSE 880
880 IF LEN(nummer$)>6 OR nummer$="//////"THEN 860 ELSE RETURN
890 REM
900 REM leveranciergegevens op scherm
910 PRINT hel$;:PRINT FNat$(3,27,nummer$):PRINT FNat$(5,24,naam$):PRINT FNat$(6,24,straat$):PRINT FNat$(7,24,stad$):PRINT FNat$(9,24,telefoon$)
920 PRINT FNat$(11,24,"");USING"#########";VAL(jaaromzet$):PRINT FNat$(13,24,"");USING"#########";VAL(saldo$):PRINT FNat$(15,24,rekening$):PRINT FNat$(17,24,"");VAL(vervaldag$)
930 PRINT zcht$;:RETURN
940 REM intijpen gegevens
950 t=0
960 PRINT FNat$(5,24,"..............................       ");hel$;:PRINT FNat$(5,24,"");:INPUT"",naam$:PRINT zcht$;:IF LEN(naam$)>30 THEN 960
970 IF t=1 THEN RETURN
980 PRINT FNat$(6,24,"..............................     ");hel$;:PRINT FNat$(6,24,"");:INPUT"",straat$:PRINT zcht$;:IF LEN(straat$)>30 THEN 980
990 IF t=1 THEN RETURN
1000 PRINT FNat$(7,24,"..............................     ");hel$;:PRINT FNat$(7,24,"");:INPUT"",stad$:PRINT zcht$;:IF LEN(stad$)>30 THEN 1000
1010 IF t=1 THEN RETURN
1020 PRINT FNat$(9,24,"............   ");hel$;:PRINT FNat$(9,24,"");:INPUT"",telefoon$:PRINT zcht$;:IF LEN(telefoon$)>12 THEN 1020
1030 IF t=1 THEN RETURN
1040 PRINT FNat$(11,24,"");hel$;:INPUT"",jaaromzet$:PRINT zcht$;:jaaromzet=VAL(jaaromzet$):IF LEN(jaaromzet$)>9 THEN 1040
1050 IF t=1 THEN RETURN
1060 PRINT FNat$(13,24,"");hel$;:INPUT"",saldo$:PRINT zcht$;:saldo=VAL(saldo$):IF LEN(saldo$)>9 THEN 1060
1070 IF t=1 THEN RETURN
1080 PRINT FNat$(15,24,"...-.......-..    ");hel$;:PRINT FNat$(15,24,"");:INPUT"",rekening$:PRINT zcht$;:q=q+1:reknum$(q)=rekening$:IF LEN(rekening$)>14 THEN 1080
1090 IF reknum$(q-1)=rekening$THEN 1100 ELSE IF INT((VAL(MID$(rekening$,1,3)+MID$(rekening$,5,7))/97-INT(VAL(MID$(rekening$,1,3)+MID$(rekening$,5,7))/97))*97)<>VAL(MID$(rekening$,13,2))THEN 1080
1100 IF t=1 THEN RETURN
1110 PRINT FNat$(17,24,"");hel$;:INPUT"",vervaldag$:PRINT zcht$;:antw$="08306090120":IF INSTR(antw$,vervaldag$)=0 THEN 1110
1120 t=1
1130 PRINT FNat$(23,0,"GEEF <2><3><4><5><6><7><8><9> OM TE WIJZIGEN - <E> VOOR EINDE");eol$;
1140 antw$="23456789E":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 1140
1150 IF a$="E"THEN RETURN
1160 ON VAL(a$)-1 GOSUB 960,980,1000,1020,1040,1060,1080,1110
1170 GOTO 1130
1180 REM routine variabele=>rec
1190 LSET rec.levnum$=nummer$:LSET rec.naam$=naam$:LSET rec.straat$=straat$:LSET rec.stad$=stad$:LSET rec.telefoon$=telefoon$:LSET rec.jaaromzet$=jaaromzet$:LSET rec.saldo$=saldo$:LSET rec.rekening$=rekening$:LSET rec.vervaldag$=vervaldag$
1200 RETURN
1210 REM routine rec=>variabele
1220 naam$=rec.naam$:straat$=rec.straat$:stad$=rec.stad$:telefoon$=rec.telefoon$:jaaromzet$=rec.jaaromzet$:saldo$=rec.saldo$:rekening$=rec.rekening$:vervaldag$=rec.vervaldag$
1230 nummer$=rec.levnum$:RETURN
1240 PRINT FNat$(3,27,eol$):FOR t=5 TO 17:PRINT FNat$(t,24,eol$):NEXT t
1250 RETURN
1260 CLOSE 1:RUN"menu.mar"
1270 BUFFERS 20
1280 CREATE 1,drl$+"leveranc.rnd",drl$+"leveranc.key",2,152
1290 CLOSE 1
1300 RETURN
1310 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
1320 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1340 ELSE 1330
1330 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1340 RETURN
1350 PRINT FNat$(23,0,"GEEF DE EERSTE 4 LETTERS VAN DE TE ZOEKEN NAAM");eol$:PRINT FNat$(5,24,"");hel$;:INPUT"",alfa$:PRINT zcht$;:IF alfa$=""THEN nummer$="10000000":RETURN ELSE 1360
1360 kontrole=SEEKKEY(1,2,1,alfa$):IF kontrole=0 THEN 1380
1370 PRINT FNat$(23,0,"GEEN LEVERANCIERS MET DEZE BEGINLETTERS !!");eol$;:GOSUB 1240:FOR t=1 TO 1000:NEXT t:GOTO 1350 ELSE 1380
1380 GET 1:GOSUB 1210:GOSUB 900
1390 PRINT FNat$(23,0,"<O>K ? - <V>ERDER ZOEKEN ? : DRUK DE JUISTE TOETS");eol$;
1400 antw$="OoVv":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 1400
1410 IF a$="O"THEN RETURN
1420 kontrole=SEEKNEXT(1,1):IF kontrole<>0 THEN PRINT FNat$(23,0,"GEEN KLANTEN MEER MET DEZE BEGINLETTERS !!");eol$:GOSUB 1240:FOR t=1 TO 1000:NEXT t:GOTO 1350 ELSE 1380
1520 REM subroutine SEEKKEY
1530 kontrole=SEEKKEY(file%,2,rank%,key$):IF kontrole<>0 THEN PRINT CHR$(7);
1540 RETURN
1550 REM subroutine DELKEY
1560 kontrole=DELKEY(file%,2,rank%,key$,record):IF kontrole>103 THEN PRINT CHR$(7);
1570 RETURN
1580 REM subroutine ADDKEY
1590 kontrole=ADDKEY(file%,2,rank%,key$,record)
1600 RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"TH