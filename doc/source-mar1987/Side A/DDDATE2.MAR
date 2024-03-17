10 REM DDDATE2.MAR=UPDATE LEVERANCIERS- KLANTEN- EN FAKTURATIEBESTANDEN                 versie 01/04/87
20 cls$=CHR$(27)+"E":DEF FNat$(l,k,tekst$)=CHR$(27)+"Y"+CHR$(32+l)+CHR$(32+k)+tekst$
30 hel$="":zcht$=""
40 eol$=CHR$(27)+"K"
50 REM
60 hel$=CHR$(27)+CHR$(112):zcht$=CHR$(27)+CHR$(113)
70 BUFFERS 20:IF FIND$("B:"+"vkfaktr.rnd")="" THEN 260 ELSE OPEN "K",1,"B:"+"vkfaktr.rnd","B:"+"vkfaktr.key",2
80 FIELD 1,4 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datum$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
90 CREATE 2,"m:"+"vkfaktr.rnd","m:"+"vkfaktr.key",2,132
100 FIELD 2,6 AS rec.nrkb$,8 AS rec.datb$,8 AS rec.datumb$,9 AS rec.bt6b$,9 AS rec.bt19b$,9 AS rec.bt25b$,9 AS rec.bt33b$,9 AS rec.nttbb$,9 AS rec.ttbb$,9 AS rec.rbb$,7 AS rec.bdokb$,7 AS rec.boekb$,9 AS rec.medektrb$,9 AS rec.taksb$,9 AS rec.reserveb$
110 PRINT cls$
120 PRINT FNat$(0,5,"U P D A T E   V E R K O O P F A K T U R E N B E S T A N D")
130 PRINT STRING$(79,CHR$(172))
140 kontrole=SEEKRANK(1,2,1)
150 nummer$=FETCHKEY$(1)
160 kontrole=SEEKKEY(1,2,1,nummer$)
170 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 150 ELSE STOP
180 PRINT FNat$(5,0,"BEZIG AAN VF/CN : ");nummer$;" ";"KLANT ";rec.nrk$
190 LSET rec.nrkb$="00"+rec.nrk$:LSET rec.datb$=rec.dat$:LSET rec.datumb$=rec.datum$:LSET rec.bt6b$=rec.bt6$:LSET rec.bt19b$=rec.bt19$:LSET rec.bt25b$=rec.bt25$:LSET rec.bt33b$=rec.bt33$:LSET rec.nttbb$=rec.nttb$:LSET rec.ttbb$=rec.ttb$
200 LSET rec.rbb$=rec.rb$:LSET rec.bdokb$=rec.bdok$:LSET rec.boekb$=rec.boek$:LSET rec.medektrb$=rec.medektr$:LSET rec.taksb$=rec.taks$:LSET rec.reserveb$=rec.reserve$
210 kontrole=ADDREC(2,2,1,nummer$):a=FETCHREC(2):kontrole=ADDKEY(2,2,0,rec.nrkb$,a)
220 kontrole=SEEKNEXT(1,1):IF kontrole=101 THEN 150 ELSE IF kontrole=102 THEN 240 ELSE 240
230 GOTO 150
240 CLOSE 1:CLOSE 2
250 PRINT "EINDE UPDATE VERKOOPFAKTUREN":FOR wacht=1 TO 2000:NEXT wacht
260 BUFFERS 20:IF FIND$("B:"+"afactuur.rnd")="" THEN 450 ELSE OPEN "K",1,"B:"+"afactuur.rnd","B:"+"afactuur.key",2
270 FIELD 1,4 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
280 CREATE 2,"m:"+"afactuur.rnd","m:"+"afactuur.key",2,132
290 FIELD 2,6 AS rec.nrlb$,6 AS rec.nrrb$,8 AS rec.datb$,20 AS rec.omsb$,9 AS rec.hgb$,9 AS rec.btwb$,9 AS rec.nbtwb$,9 AS rec.rbetb$,7 AS rec.bdokb$,7 AS rec.kodeb$,9 AS rec.invvhfb$,9 AS rec.sptaksb$,9 AS rec.btwmedektrb$,9 AS rec.investb$
300 PRINT cls$
310 PRINT FNat$(0,5,"U P D A T E   A A N K O O P F A K T U R E N B E S T A N D")
320 PRINT STRING$(79,CHR$(172))
330 kontrole=SEEKRANK(1,2,1)
340 nummer$=FETCHKEY$(1)
350 kontrole=SEEKKEY(1,2,1,nummer$)
360 IF kontrole=0 THEN GET 1 ELSE IF kontrole=105 THEN 340 ELSE STOP
370 PRINT FNat$(5,0,"BEZIG AAN VF/CN : ");nummer$;" ";"LEVERANCIER ";rec.nrl$
380 LSET rec.nrlb$="00"+rec.nrl$:LSET rec.nrrb$=rec.nrr$:LSET rec.datb$=rec.dat$:LSET rec.omsb$=rec.oms$:LSET rec.hgb$=rec.hg$:LSET rec.btwb$=rec.btw$:LSET rec.nbtwb$=rec.nbtw$:LSET rec.rbetb$=rec.rbet$:LSET rec.bdokb$=rec.bdok$:LSET rec.kodeb$=rec.kode$
390 LSET rec.invvhfb$=rec.invvhf$:LSET rec.sptaksb$=rec.sptaks$:LSET rec.btwmedektrb$=rec.btwmedektr$:LSET rec.investb$=rec.invest$
400 kontrole=ADDREC(2,2,1,nummer$):a=FETCHREC(2):kontrole=ADDKEY(2,2,0,rec.nrlb$,a)
410 kontrole=SEEKNEXT(1,1):IF kontrole=101 THEN 340 ELSE IF kontrole=102 THEN 430 ELSE 430
420 GOTO 340
430 CLOSE 1:CLOSE 2
440 PRINT "EINDE UPDATE AANKOOPFAKTUREN":FOR wacht=1 TO 2000:NEXT wacht
450 PRINT cls$:PRINT "DRUK 'SUBMIT COPY2' GEVOLGD DOOR <RETURN> a.u.b. !":SYSTEM
OOPFAKTUREN":FOR wacht=1 TO 2000:NEXT wacht
45