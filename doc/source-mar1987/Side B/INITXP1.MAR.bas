10 ON ERROR GOTO 900
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
80 OPEN "r",1,drive$+"99.rnd",30:FIELD 1,28 AS rec.nummer$:GOSUB 860:CLOSE 1
90 PRINT cls$:PRINT "DIT PROGRAMMA GAAT ALLE HOOFDBESTANDEN INITIALISEREN ALSOOK DE TIJDELIJKE       BESTANDEN ZOALS 'ALGEMEEN JOURNAAL' EN DE DIVERSE FINANCIELE BOEKEN VERNIETIGEN."
100 PRINT:PRINT "AKKOORD ? DRUK <V>ERVOLG OF <M>ENU : ";
110 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 110
120 IF at$="M" THEN 850
125 IF LEN(drive$)>2 THEN IF UPPER$(MID$(drive$,12,1))="Y" THEN PRINT FNat$(18,5,"KIES EERST EEN 'X.. BOEKJAAR ");:FOR t=1 TO 3000:NEXT t:RUN "menu.mar" ELSE 130
130 BUFFERS 6
140 OPEN "K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
150 FIELD 1,6 AS rec.levnum$,30 AS rec.naam2$,30 AS rec.straat2$,30 AS rec.stad2$,12 AS rec.telefoon2$,9 AS rec.jaaromzet2$,9 AS rec.saldo2$,14 AS rec.rekening2$,4 AS rec.vervaldag2$
160 PRINT cls$
170 PRINT FNat$(0,0,"I N I T I A L I S A T I E   L E V E R A N C I E R S B E S T A N D"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
180 kontrole=SEEKRANK(1,2,0)
190 nummer$=FETCHKEY$(1)
200 kontrole=SEEKKEY(1,2,0,nummer$)
210 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 190 ELSE STOP
220 PRINT FNat$(10,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(12,17,rec.naam2$);zcht$;
230 LSET rec.jaaromzet2$=STR$(0):PUT 1
240 kontrole=SEEKNEXT(1,0):IF kontrole=101 THEN 190 ELSE IF kontrole=102 THEN 260 ELSE STOP
250 GOTO 190
260 CLOSE 1
270 PRINT FNat$(23,0,"EINDE INITIALISATIE LEVERANCIERS");:FOR wacht=1 TO 2000:NEXT wacht
280 PRINT cls$
290 OPEN "K",1,drk$+"klanten.rnd",drk$+"klanten.key",2
300 FIELD 1,6 AS rec.numkl$,30 AS rec.naamb$,30 AS rec.straatb$,5 AS rec.postkodeb$,30 AS rec.stadb$,12 AS rec.telefoonb$,12 AS rec.btwnrb$,9 AS rec.btwbedragb$,9 AS rec.jaaromzetb$,9 AS rec.saldob$,4 AS rec.vervaldagb$
310 PRINT cls$
320 PRINT FNat$(0,0,"I N I T I A L I S A T I E   K L A N T E N B E S T A N D"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
330 kontrole=SEEKRANK(1,2,0)
340 nummer$=FETCHKEY$(1)
350 kontrole=SEEKKEY(1,2,0,nummer$)
360 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 350 ELSE STOP
370 PRINT FNat$(10,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(12,17,rec.naamb$);zcht$;
380 LSET rec.jaaromzetb$=STR$(0):LSET rec.btwbedragb$=STR$(0):PUT 1
390 kontrole=SEEKNEXT(1,0):IF kontrole=101 THEN 340 ELSE IF kontrole=102 THEN 410 ELSE STOP
400 GOTO 340
410 CLOSE 1
420 PRINT FNat$(23,0,"EINDE INITIALISATIE KLANTEN");:FOR wacht=1 TO 2000:NEXT wacht
430 OPEN "K",1,drr$+"rekening.rnd",drr$+"rekening.key",2
440 FIELD 1,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
450 PRINT cls$
460 PRINT FNat$(0,0,"I N I T I A L I S A T I E   J A A R R E K E N I N G E N B E S T A N D"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
470 kontrole=SEEKRANK(1,2,0)
480 nummer$=FETCHKEY$(1)
490 kontrole=SEEKKEY(1,2,0,nummer$)
500 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 480 ELSE STOP
510 LSET rec.jaarsaldo$=STR$(0):LSET rec.maandsaldo$=STR$(0):LSET rec.telhistor$=STR$(0):PUT 1
520 PRINT FNat$(10,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(12,17,rec.om$);zcht$;
530 kontrole=SEEKNEXT(1,0):IF kontrole=101 THEN 480 ELSE IF kontrole=102 OR kontrole=103 THEN 550 ELSE STOP
540 GOTO 480
550 PRINT FNat$(23,0,"EINDE INITIALISATIE REKENINGBESTAND");
560 CLOSE 1
570 OPEN "K",1,dra$+"artikels.rnd",dra$+"artikels.key",2
580 FIELD 1,25 AS rec.om$,9 AS rec.kostprijs$,2 AS rec.btw$,9 AS rec.jaaromzet$,9 AS rec.maandomzet$,9 AS rec.stock$,9 AS rec.minstock$
590 PRINT cls$
600 PRINT FNat$(0,0,"I N I T I A L I S A T I E    A R T I K E L B E S T A N D"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
610 kontrole=SEEKRANK(1,2,0)
620 nummer$=FETCHKEY$(1)
630 kontrole=SEEKKEY(1,2,0,nummer$)
640 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 620 ELSE STOP
650 PRINT FNat$(10,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(12,17,rec.om$);zcht$;
660 LSET rec.jaaromzet$=STR$(0):LSET rec.maandomzet$=STR$(0):PUT 1
670 kontrole=SEEKNEXT(1,0):IF kontrole=101 THEN 620 ELSE IF kontrole=102 THEN 690 ELSE STOP
680 GOTO 620
690 CLOSE 1
700 PRINT cls$:PRINT FNat$(0,0,"I N I T I A L I S A T I E   H O O F D R E K E N I N G E N   E N   T E L L E R S"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
710 BUFFERS 6
720 OPEN "R",1,drt$+"99.rnd",30
730 FIELD 1,6 AS rec.nummer$:GET 1,5:jaar=VAL(rec.nummer$):jaar=jaar+1
740 nummer$="0":LSET rec.nummer$=nummer$
750 FOR t=1 TO 6:PUT 1,t:NEXT t:LSET rec.nummer$=STR$(jaar):PUT 1,5
760 LSET rec.nummer$=nummer$:FOR t=11 TO 15:PUT 1,t:NEXT t
770 LSET rec.nummer$=nummer$:FOR t=31 TO 35:PUT 1,t:NEXT t
780 PUT 1,38
790 CLOSE 1
800 KILL drl$+"journaal.*"
810 NAME drt$+"99.rnd" AS drt$+"negen.rnd"
820 KILL drf$+"??.rnd"
830 KILL drf$+"??.key"
840 NAME drt$+"negen.rnd" AS drt$+"99.rnd"
850 RUN "menu.mar
860 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
870 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 890 ELSE 880
880 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
890 RETURN
900 PRINT FNat$(23,0,eol$+"foutkode :");ERR
910 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
920 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 920
930 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 940 ELSE RESUME NEXT
940 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
$="" THEN 920
930 IF at$="H" THEN RESUM