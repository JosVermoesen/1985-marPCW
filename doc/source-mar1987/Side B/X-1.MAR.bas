1 ON ERROR GOTO 20000
10 REM X-1.MAR=KLAARMAKEN JAAR VOOR Y versie 01/05/87
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
80 IF LEN(drive$)=2 THEN 90 ELSE 100
90 PRINT cls$:PRINT FNat$(10,5,"CONSOLIDATIE ALLEEN MOGELIJK BIJ HARD DISK");:FOR t=1 TO 2000:NEXT t:RUN "menu.mar
100 PRINT cls$:PRINT FNat$(10,0,"HET M.A.R. PAKKET MET CONSOLIDATIE ZAL G R A T I S TER BESCHIKKING GESTELD      WORDEN VANAF 01/09/87.  ENKEL VOOR HARD DISK GEBRUIKERS...");:FOR t=1 TO 3000:NEXT t:RUN"menu.mar
110 PRINT FNat$(10,5,"HUIDIGE BOEKHOUDING KLAARMAKEN ALS X-1 (J/N) ? ");
120 antw$="JN":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$="" THEN 120
130 IF m$="N" THEN 180
140 OPEN "R",1,"A:99.rnd",30
150 FIELD 1,6 AS rec.nummer$
160 LSET rec.nummer$="ON"
170 PUT 1,37:CLOSE 1
180 RUN "menu.mar
190 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
200 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 220 ELSE 210
210 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
220 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3: