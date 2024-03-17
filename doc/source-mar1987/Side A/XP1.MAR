1 ON ERROR GOTO 20000
10 REM XP1=AFSLUITEN BOEKHOUDING JAAR Y (X-1) versie 01/06/87
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 PRINT cls$:PRINT FNat$(0,0,"UITSCHAKELEN CONSOLIDATIE/OVERDRACHT JAARSALDO/VERNIETIGEN VORIG BOEKJAAR"):PRINT FNat$(1,0,STRING$(79,172)):PRINT FNat$(22,0,STRING$(79,172));
80 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN "r",1,drive$+"99.rnd",30:FIELD 1,28 AS rec.nummer$:GOSUB 470:CLOSE 1
100 IF LEN(drive$)=2 THEN 110 ELSE 120
110 dd$="BRENG DE DATADISKETTE VOOR DRIVE A VAN "+hel$+"BOEKHOUDING Y IN DRIVE B"+zcht$+"                 EN DE DATADISKETTE VOOR DRIVE A VAN "+hel$+"BOEKHOUDING X IN DRIVE A"+zcht$:GOTO 130
120 dd$="JAARSALDO'S BOEKJAAR Y NAAR BOEKJAAR X OVERDRAGEN ?"
130 REM
140 PRINT FNat$(10,5,dd$):PRINT FNat$(23,0,"DRUK <V> VOOR VERVOLG - <M> VOOR MENU ");
150 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 150
160 IF at$="M" THEN RUN "menu.mar
170 IF LEN(drive$)>2 THEN IF UPPER$(MID$(drive$,12,1))<>"Y" THEN PRINT FNat$(18,5,"EERST EEN 'Y..' BOEKHOUDJAAR KIEZEN ! ");:FOR t=1 TO 3000:NEXT t:RUN "menu.mar" ELSE 180
180 IF LEN(drive$)=2 THEN dr2$=drive$ ELSE dr2$=MID$(drive$,1,11)+"X"+MID$(drive$,13,3)
190 IF LEN(drive$)=2 THEN dr1$="b:" ELSE dr1$=drive$
200 REM
210 BUFFERS 6
220 OPEN "K",1,dr1$+"rekening.rnd",dr1$+"rekening.key",2
230 FIELD 1,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
240 OPEN "K",2,dr2$+"rekening.rnd",dr2$+"rekening.key",2
250 FIELD 2,30 AS rec.omb$,9 AS rec.jaarsaldob$,9 AS rec.maandsaldob$,3 AS rec.telhistorb$
260 PRINT cls$
270 PRINT FNat$(0,0,"U P D A T E   J A A R R E K E N I N G E N B E S T A N D  X+1")
280 PRINT FNat$(1,0,STRING$(80,172))
290 kontrole=SEEKRANK(1,2,0)
300 nummer$=FETCHKEY$(1)
310 kontrole=SEEKKEY(1,2,0,nummer$)
320 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 300 ELSE STOP
330 kontrole=SEEKKEY(2,2,0,nummer$):IF kontrole=0 THEN GET 2 ELSE STOP
340 PRINT FNat$(15,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(16,17,rec.om$);zcht$;
350 LSET rec.jaarsaldob$=STR$(VAL(rec.jaarsaldob$)+VAL(rec.jaarsaldo$))
360 PUT 2
370 PRINT FNat$(17,5,"BEZIG AAN : ");hel$;nummer$:PRINT FNat$(16,17,rec.om$);zcht;
380 kontrole=SEEKNEXT(1,1):IF kontrole=101 THEN 300 ELSE IF kontrole=102 OR kontrole=103 THEN 400 ELSE STOP
390 GOTO 300
400 PRINT FNat$(23,0,"EINDE UPDATE REKENINGBESTAND");eol$;
410 CLOSE 1:CLOSE 2
420 IF LEN(drive$)=2 THEN RUN "menu.mar" ELSE PRINT FNat$(23,0,"BOEKHOUDING ";drive$;" VERNIETIGEN (J/N) ? : ");eol$;
430 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 430
440 IF at$="N" THEN RUN "menu.mar"
450 CHDIR dr1$:KILL "*.rnd":KILL "*.key":KILL "*.fil":KILL "*.rap":CHDIR MID$(dr1$,3,8):KILL MID$(dr1$,12,3)+".vsf":RMDIR MID$(dr1$,12,3):CHDIR "/boekhoud"
460 PRINT cls$:PRINT "BOEKJAAR "+dr1$+"WERD AFGESLOTEN EN VERWIJDERD":PRINT "START HET PAKKET TERUG MET 'MAR' GEVOLGD DOOR <RETURN>":SYSTEM
470 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
480 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 500 ELSE 490
490 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
500 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 E