1 ON ERROR GOTO 20000
10 REM KB.MAR=KREATIE BTW KODES DD versie 040487
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN 
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 BUFFERS 6
80 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN"r",1,drive$+"99.rnd",30
100 FIELD 1,28 AS rec.nummer$
110 GOSUB 970:CLOSE 1
120 recleng=4:OPEN"R",1,drb$+"btw.rnd",recleng
130 FIELD 1,2 AS rec.btwpct$
140 PRINT cls$
150 PRINT FNat$(0,0,"KREATIE BTWKODES ====>"):PRINT FNat$(0,39,"<N>ieuwe,<O>pvragen,<W>ijzigen,<E>inde:O")
160 PRINT STRING$(80,CHR$(172))
170 PRINT FNat$(5,5,"1. KODE     NUMMER :"):PRINT FNat$(7,5,"2. BTW %-AGE       :")
180 PRINT FNat$(22,0,STRING$(80,CHR$(172)))
190 PRINT FNat$(0,78,"");:INPUT"",p$:p$=UPPER$(p$)
200 IF p$="N"THEN keuze=1 ELSE IF p$="O"OR p$=""THEN keuze=2 ELSE IF p$="W"THEN keuze=3 ELSE IF p$="A"THEN keuze=4 ELSE IF p$="E"THEN 950 ELSE 190
210 ON keuze GOSUB 230,340,440,570
220 GOTO 140
230 REM invoer nieuwe rekening
240 GOSUB 710:REM invoer rekeningnummer
250 GET 1,VAL(nummer$)
260 IF INSTR("0 6 17192533",rec.btwpct$)<>0 THEN 240
270 GOSUB 780:REM intijpen gegevens
280 GOSUB 870:REM lset routine
290 PUT 1,VAL(nummer$)
300 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
310 antw$="EV":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 310
320 IF a$="V"THEN GOSUB 930 ELSE 950
330 GOTO 240
340 REM opvragen rekening
350 GOSUB 710:REM invoer rekeningnummer
360 GET 1,VAL(nummer$)
370 IF INSTR(" 0 6 17192533",rec.btwpct$)=0 THEN 350
380 GOSUB 900:REM variabele routine
390 GOSUB 740:REM rekeninggegevens op scherm
400 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
410 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 410
420 IF a$="V"THEN GOSUB 930 ELSE 950
430 GOTO 350
440 REM wijzigen rekening
450 GOSUB 710:REM invoer rekeningnummer
460 GET 1,VAL(nummer$)
470 IF INSTR(" 0 6 17192533",rec.btwpct$)=0 THEN 450
480 GOSUB 900:REM variabele routine
490 GOSUB 740:REM gegevens op scherm
500 GOSUB 810:REM wijzigen gegevens
510 GOSUB 870:REM lset routine
520 PUT 1,VAL(nummer$)
530 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
540 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 540
550 IF a$="V"THEN GOSUB 930 ELSE 950
560 GOTO 450
570 REM annuleren rekening
580 GOSUB 710:REM invoer rekeningnummer
590 GET 1,VAL(nummer$)
600 IF INSTR(" 0 6 17192533",rec.btwpct$)=0 THEN 580
610 GOSUB 900:REM variabele routine
620 GOSUB 740:REM rekeninggegevens op scherm
630 PRINT FNat$(23,0,"ANNULEREN (J/N) ?");eol$
640 antw$="JN":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 640
650 IF a$="N"THEN 670
660 LSET rec.btwpct$="":PUT 1,nummer
670 PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <E> VOOR EINDE");eol$
680 antw$="VE":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 680
690 IF a$="V"THEN GOSUB 930 ELSE 950
700 GOTO 580
710 REM invoer rekeningnummer
720 PRINT FNat$(5,26,".      "):PRINT FNat$(5,26,"");:INPUT"",nummer$:IF nummer$=""THEN 950 ELSE IF VAL(nummer$)<1 OR VAL(nummer$)>6 THEN 720
730 nummer=VAL(nummer$):IF nummer>10 OR nummer<0 THEN 720 ELSE RETURN
740 REM rekeninggegevens op scherm
750 PRINT FNat$(7,26,rec.btwpct$)
760 RETURN
770 REM intijpen gegevens
780 t=0
790 PRINT FNat$(7,26,"..      "):PRINT FNat$(7,26,"");:INPUT"",btwpct$:IF LEN(btwpct$)>2 OR INSTR("0 6 17192533",btwpct$)=0 THEN 790
800 IF t=1 THEN RETURN
810 t=1
820 PRINT FNat$(23,0,"GEEF <2> OM TE WIJZIGEN - <E> VOOR EINDE");eol$
830 antw$="2E":a$=UPPER$(INKEY$):IF INSTR(antw$,a$)=0 OR a$=""THEN 830
840 IF a$="E"THEN RETURN
850 ON VAL(a$)-1 GOSUB 790
860 GOTO 820
870 REM routine variabele=>rec
880 LSET rec.btwpct$=btwpct$
890 RETURN
900 REM routine rec=>variabele
910 btwpct$=rec.btwpct$
920 RETURN
930 FOR t=5 TO 7 STEP 2:PRINT FNat$(t,26,eol$):NEXT t
940 RETURN
950 CLOSE 1:RUN"menu.mar"
960 BUFFERS 6
970 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
980 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 1000 ELSE 990
990 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
1000 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE I