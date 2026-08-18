1 ON ERROR GOTO 20000
10 REM ERAAFAKT.MAR=AANKOOPFAKTUREN VERWIJDEREN UIT HET FAKTURATIEBESTAND
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 OPTION RUN
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 REM OPTION RUN
80 IF FIND$("version.vsf")="" THEN RUN "menu.mar" ELSE OPEN "i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
90 OPEN"r",1,drive$+"99.rnd",30
100 FIELD 1,28 AS rec.nummer$
110 GOSUB 420:CLOSE 1
120 PRINT cls$:PRINT FNat$(0,11,"V E R W I J D E R E N   A A N K O O P F A K T U R E N"):PRINT STRING$(80,CHR$(172))
130 PRINT FNat$(3,5,"DRUK <B> VOOR ALLE BETAALDE FAKTUREN OF <S> VOOR SELECTIEF");
140 antw$="BS":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 140
150 IF UPPER$(at$)="B"THEN flag=1 ELSE flag=0
160 BUFFERS 6
170 IF FIND$(draa$+"afactuur.rnd")="" THEN RUN "menu.mar" ELSE OPEN"K",1,draa$+"afactuur.rnd",draa$+"afactuur.key",2
180 FIELD 1,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
190 erateller=0
200 kontrole=SEEKRANK(1,2,1)
210 a$=FETCHKEY$(1)
220 IF flag=1 THEN 310
230 PRINT FNat$(22,0,STRING$(80,CHR$(172))):PRINT FNat$(23,5,"DRUK <M> + <RETURN>");eol$
240 PRINT FNat$(6,5,"JAAR   : ");eol$;hel$;:INPUT"",jj$:PRINT zcht$;:IF UPPER$(jj$)="M"THEN 410
250 PRINT FNat$(7,5,"AF/CA  : ");eol$;hel$;:INPUT"",vc$:PRINT zcht$;:vc$=UPPER$(vc$):IF vc$="M"THEN 410
260 PRINT FNat$(8,5,"NUMMER : ");eol$;hel$;:INPUT"",nummer$:PRINT zcht$;:IF UPPER$(nummer$)="M"THEN 410 ELSE nummer=VAL(nummer$)
270 a$=jj$+vc$+STR$(nummer)
280 PRINT FNat$(8,20,"OK (J/N) ?");
290 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 290
300 IF UPPER$(at$)="N"THEN 240
310 kontrole=SEEKKEY(1,2,1,a$):IF kontrole=105 THEN 210 ELSE IF kontrole=0 THEN GET 1 ELSE 240
320 tot=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+VAL(rec.invvhf$)+VAL(rec.sptaks$)-VAL(rec.btwmedektr$)
330 IF flag=0 THEN 340 ELSE IF tot<>VAL(rec.rbet$)THEN 380
340 PRINT FNat$(12,5,"BEZIG AAN : ");a$:PRINT FNat$(14,5,"TOTAAL BETAALD : ");VAL(rec.rbet$);
350 record=FETCHREC(1):kontrole=DELKEY(1,2):kontrole=SEEKKEY(1,2,0,rec.nrl$):IF kontrole<>0 THEN 360
360 kontrole=DELKEY(1,2,0,rec.nrl$,record):IF kontrole>102 THEN PRINT CHR$(7);
370 erateller=erateller+1
380 kontrole=SEEKNEXT(1,1):IF kontrole=0 THEN GET 1 ELSE IF kontrole<>101 THEN 400 ELSE 210
390 GOTO 210
400 IF erateller>0 THEN 190
410 CLOSE 1:RUN"menu.mar"
420 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
430 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 450 ELSE 440
440 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
450 RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$="" THEN 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
N 20020
20030 IF at$="H" THEN RESUME ELSE IF at$="M" THEN 20040 ELSE RESUME NEXT
200