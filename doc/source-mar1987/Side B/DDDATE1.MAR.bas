10 REM DDDATE1.MAR=UPDATE LEVERANCIERS- KLANTEN- EN FAKTURATIEBESTANDEN versie 01/04/87
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 REM
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 BUFFERS 20
80 PRINT cls$:PRINT "BRENG DE MAR DATADISKETTE VOOR A: IN DRIVE A: EN DE MAR DATADISKETTE VOOR B: IN DRIVE B:.  DRUK [RETURN] VOOR VERVOLG":INPUT "",pot$
90 pluto$="c:/mardata/x01/"
100 IF FIND$("a:"+"leveranc.rnd")="" THEN 290 ELSE OPEN "K",1,"a:"+"leveranc.rnd","a:"+"leveranc.key",2
110 FIELD 1,4 AS rec.levnum$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
120 CREATE 2,"m:"+"leveranc.rnd","m:"+"leveranc.key",2,152
130 FIELD 2,6 AS rec.levnum2$,30 AS rec.naam2$,30 AS rec.straat2$,30 AS rec.stad2$,12 AS rec.telefoon2$,9 AS rec.jaaromzet2$,9 AS rec.saldo2$,14 AS rec.rekening2$,4 AS rec.vervaldag2$
140 PRINT cls$
150 PRINT FNat$(0,5,"U P D A T E   L E V E R A N C I E R S B E S T A N D")
160 PRINT STRING$(79,CHR$(172))
170 kontrole=SEEKRANK(1,2,0)
180 nummer$=FETCHKEY$(1)
190 kontrole=SEEKKEY(1,2,0,nummer$)
200 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 180 ELSE STOP
210 PRINT FNat$(5,0,"BEZIG AAN : ");nummer$;" ";rec.naam$" ";rec.stad$
220 LSET rec.naam2$=rec.naam$:LSET rec.straat2$=rec.straat$:LSET rec.stad2$=rec.stad$:LSET rec.telefoon2$=rec.telefoon$:LSET rec.jaaromzet2$=rec.jaaromzet$:LSET rec.saldo2$=rec.saldo$:LSET rec.rekening2$=rec.rekening$:LSET rec.vervaldag2$=rec.vervaldag$
230 LSET rec.levnum2$="00"+rec.levnum$
240 kontrole=ADDREC(2,2,0,rec.levnum2$):a=FETCHREC(2):kontrole=ADDKEY(2,2,1,MID$(rec.naam2$,1,4),a)
250 kontrole=SEEKNEXT(1,1):IF kontrole=101 THEN 180 ELSE IF kontrole=102 THEN 270 ELSE STOP
260 GOTO 180
270 CLOSE 1:CLOSE 2
280 PRINT "EINDE UPDATE LEVERANCIERS":FOR wacht=1 TO 2000:NEXT wacht
290 BUFFERS 20:IF FIND$("a:"+"klanten.rnd")="" THEN 290 ELSE OPEN "K",1,"a:"+"klanten.rnd","a:"+"klanten.key",2
300 FIELD 1,4 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
310 CREATE 2,"m:"+"klanten.rnd","m:"+"klanten.key",2,162
320 FIELD 2,6 AS rec.numklb$,30 AS rec.naamb$,30 AS rec.straatb$,5 AS rec.postkodeb$,30 AS rec.stadb$,12 AS rec.telefoonb$,12 AS rec.btwnrb$,9 AS rec.btwbedragb$,9 AS rec.jaaromzetb$,9 AS rec.saldob$,4 AS rec.vervaldagb$
330 PRINT cls$
340 PRINT FNat$(0,5,"U P D A T E   K L A N T E N B E S T A N D")
350 PRINT STRING$(79,CHR$(172))
360 kontrole=SEEKRANK(1,2,0)
370 nummer$=FETCHKEY$(1)
380 kontrole=SEEKKEY(1,2,0,nummer$)
390 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 380 ELSE STOP
400 PRINT FNat$(5,0,"BEZIG AAN : ");nummer$;" ";rec.naam$;" ";rec.stad$
410 LSET rec.naamb$=rec.naam$:LSET rec.straatb$=rec.straat$:LSET rec.postkodeb$=rec.postkode$:LSET rec.stadb$=rec.stad$:LSET rec.telefoonb$=rec.telefoon$:LSET rec.btwnrb$=rec.btwnr$:LSET rec.btwbedragb$=rec.btwbedrag$
420 LSET rec.jaaromzetb$=rec.jaaromzet$:LSET rec.saldob$=rec.saldo$:LSET rec.vervaldagb$=rec.vervaldag$:LSET rec.numklb$="00"+rec.numkl$
430 kontrole=ADDREC(2,2,0,rec.numklb$):a=FETCHREC(2):kontrole=ADDKEY(2,2,1,MID$(rec.naamb$,1,4),a):kontrole=ADDKEY(2,2,2,rec.postkodeb$,a)
440 kontrole=SEEKNEXT(1,1):IF kontrole=101 THEN 370 ELSE IF kontrole=102 THEN 460 ELSE STOP
450 GOTO 370
460 CLOSE 1:CLOSE 2
470 BUFFERS 6
480 OPEN"R",1,"a:"+"99.RND",30
490 FIELD 1,6 AS rec.nummer$
500 LSET rec.nummer$="87":PUT 1,5:LSET rec.nummer$="711000":PUT 1,36
510 FIELD 1,28 AS rec.nummer$:drk$="a:":drl$="a:":drr$="a:":dra$="b:":drb$="a:":draa$="b:":drv$="b:":drj$="b:":drf$="b:":drt$="a:"
520 LSET rec.nummer$=drk$:PUT 1,61:LSET rec.nummer$=drl$:PUT 1,62:LSET rec.nummer$=drr$:PUT 1,63:LSET rec.nummer$=dra$:PUT 1,64:LSET rec.nummer$=drb$:PUT 1,65:LSET rec.nummer$=draa$:PUT 1,66
530 LSET rec.nummer$=drv$:PUT 1,67:LSET rec.nummer$=drf$:PUT 1,68:LSET rec.nummer$=drj$:PUT 1,69:LSET rec.nummer$=drt$:PUT 1,70
540 CLOSE 1
550 OPEN"K",1,"a:"+"rekening.rnd","a:"+"rekening.key",2
560 FIELD 1,30 AS rec.om$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
570 LSET rec.om$="VERKOPEN KODE 6 0 %":nummer$="711000":LSET rec.jaarsaldo$="":LSET rec.maandsaldo$="":LSET rec.telhistor$=""
580 kontrole=ADDREC(1,2,0,nummer$):CLOSE 1
590 KILL "a:btw.rnd":recleng=4:OPEN "R",2,"a:"+"btw.rnd",recleng
600 FIELD 2,2 AS rbtw$
610 LSET rbtw$=" 6":PUT 2,1:LSET rbtw$="17":PUT 2,2:LSET rbtw$="19":PUT 2,3:LSET rbtw$="25":PUT 2,4:LSET rbtw$="33":PUT 2,5:LSET rbtw$=" 0":PUT 2,6:CLOSE 2
620 PRINT cls$:PRINT "DRUK 'SUBMIT COPY1' GEVOLGD DOOR <RETURN> a.u.b.!":SYSTEM
"33":PUT 2,5:LSET rbtw$=" 0":PUT 2,6:CLOSE 2
620 PRINT cls$:PRINT "