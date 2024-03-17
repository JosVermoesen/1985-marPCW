1760 OPTION RUN:esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
1770 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
1780 sld=VAL(rec.saldo$):btwtot=VAL(rec.btwbedrag$):jvk=VAL(rec.jaaromzet$):IF r<>11 THEN 1800
1790 sld=sld+nnttb+nttb+tbtw+ttb:jvk=jvk+nttb+nnttb+ttb+tbtw:btwtot=btwtot+tbtw:GOTO 1810
1800 sld=sld-(tbtw+nnttb+nttb+ttb):jvk=jvk-(nnttb+nttb+ttb+tbtw):btwtot=btwtot-tbtw
1810 LSET rec.saldo$=STR$(sld):LSET rec.btwbedrag$=STR$(btwtot):LSET rec.jaaromzet$=STR$(jvk):PUT 1
1820 CLOSE 1
1830 IF r=11 THEN s$="VF"ELSE s$="CV"
1840 sleutel$=s$+STR$(anr)
1850 IF FIND$(drv$+"vkfaktr.rnd")=""THEN GOSUB 2940
1860 OPEN"K",1,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
1870 FIELD 1,6 AS rec.nrk$,8 AS rec.dat$,8 AS rec.datum$,9 AS rec.bt6$,9 AS rec.bt19$,9 AS rec.bt25$,9 AS rec.bt33$,9 AS rec.nttb$,9 AS rec.ttb$,9 AS rec.rb$,7 AS rec.bdok$,7 AS rec.boek$,9 AS rec.medektr$,9 AS rec.taks$,9 AS rec.reserve$
1880 LSET rec.nrk$=nrk$:LSET rec.dat$=d$:LSET rec.datum$=dat1$
1890 IF UPPER$(mktr$)="N"THEN 1910
1900 tb6=0:tb19=0:tb25=0:tb33=0
1910 LSET rec.bt6$=STR$(tb6):LSET rec.bt19$=STR$(tb19):LSET rec.bt25$=STR$(tb25):LSET rec.bt33$=STR$(tb33):LSET rec.nttb$=STR$(nttb):LSET rec.rb$="":LSET rec.reserve$="":LSET rec.taks$=STR$(ttaksbtw):LSET rec.reserve$=STR$(nnttb)
1920 LSET rec.ttb$=STR$(tbtw-ttaksbtw):temp=ttb:ttb=0
1930 IF mktr$="J"THEN LSET rec.medektr$=STR$(temp)ELSE LSET rec.medektr$=""
1940 LSET rec.bdok$="":LSET rec.boek$=s$+MID$(sleutel$,4,4):kontrole=ADDREC(1,2,0,nrk$):a=FETCHREC(1):kontrole=ADDKEY(1,2,1,MID$(d9$,1,2)+sleutel$,a)
1950 CLOSE 1
1960 IF MID$(boekj$,1,2)="ON"THEN GOSUB 3060
1970 OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$
1980 LSET rec.nummer$=STR$(anr):PUT 1,r
1990 GET 1,15:tvv=VAL(rec.nummer$):IF r=11 THEN tvv=tvv+vv ELSE tvv=tvv-vv
2000 LSET rec.nummer$=STR$(tvv):PUT 1,15:CLOSE 1
2010 OPEN"K",1,drr$+"rekening.rnd",drr$+"rekening.key",2
2020 FIELD 1,30 AS rec.oms$,9 AS rec.jaarsaldo$,9 AS rec.maandsaldo$,3 AS rec.telhistor$
2030 IF mktr$="N"THEN 2070
2040 kontrole=SEEKKEY(1,2,0,vkpmktr$):IF kontrole<>0 THEN STOP ELSE GET 1
2050 ms=VAL(rec.maandsaldo$):IF r<>11 THEN 2310
2060 ms=ms-temp-nttb-nnttb:LSET rec.maandsaldo$=STR$(ms):PUT 1:GOTO 2310
2070 kontrole=SEEKKEY(1,2,0,rv6$):IF kontrole<>0 THEN STOP ELSE GET 1
2080 ms=VAL(rec.maandsaldo$)
2090 IF r=11 THEN ms=ms-tb6 ELSE 2310
2100 LSET rec.maandsaldo$=STR$(ms):PUT 1
2110 kontrole=SEEKKEY(1,2,0,rv19$):IF kontrole<>0 THEN STOP ELSE GET 1
2120 ms=VAL(rec.maandsaldo$)
2130 IF r=11 THEN ms=ms-tb19 ELSE ms=ms+tb19
2140 LSET rec.maandsaldo$=STR$(ms):PUT 1
2150 kontrole=SEEKKEY(1,2,0,rv25$):IF kontrole<>0 THEN STOP ELSE GET 1
2160 ms=VAL(rec.maandsaldo$)
2170 IF r=11 THEN ms=ms-tb25 ELSE ms=ms+tb25
2180 LSET rec.maandsaldo$=STR$(ms):PUT 1
2190 kontrole=SEEKKEY(1,2,0,rv33$):IF kontrole<>0 THEN STOP ELSE GET 1
2200 ms=VAL(rec.maandsaldo$)
2210 IF r=11 THEN ms=ms-tb33 ELSE ms=ms+tb33
2220 LSET rec.maandsaldo$=STR$(ms):PUT 1
2230 kontrole=SEEKKEY(1,2,0,rv0$):IF kontrole<>0 THEN STOP ELSE GET 1
2240 ms=VAL(rec.maandsaldo$)
2250 IF r=11 THEN ms=ms-tb0 ELSE ms=ms+tb0
2260 LSET rec.maandsaldo$=STR$(ms):PUT 1
2270 kontrole=SEEKKEY(1,2,0,rv00$):IF kontrole<>0 THEN STOP ELSE GET 1
2280 ms=VAL(rec.maandsaldo$)
2290 IF r=11 THEN ms=ms-tb00 ELSE ms=ms+tb00
2300 LSET rec.maandsaldo$=STR$(ms):PUT 1
2310 IF r<>11 THEN 2360
2320 kontrole=SEEKKEY(1,2,0,btv$):IF kontrole<>0 THEN STOP ELSE GET 1
2330 ms=VAL(rec.maandsaldo$)
2340 ms=ms-tbtw+ttaksbtw
2350 LSET rec.maandsaldo$=STR$(ms):PUT 1:GOTO 2400
2360 kontrole=SEEKKEY(1,2,0,btc$):IF kontrole<>0 THEN STOP ELSE GET 1
2370 ms=VAL(rec.maandsaldo$)
2380 ms=ms+tbtw-ttaksbtw
2390 LSET rec.maandsaldo$=STR$(ms):PUT 1
2400 kontrole=SEEKKEY(1,2,0,taksen$):IF kontrole<>0 THEN STOP ELSE GET 1
2410 ms=VAL(rec.maandsaldo$):
2420 IF r=11 THEN ms=ms-ttaksbtw ELSE ms=ms+ttaksbtw
2430 LSET rec.maandsaldo$=STR$(ms):PUT 1
2440 IF r=11 THEN 2480
2450 kontrole=SEEKKEY(1,2,0,creditnr$):IF kontrole<>0 THEN STOP ELSE GET 1
2460 ms=VAL(rec.maandsaldo$):ms=ms+temp+nttb+nnttb
2470 LSET rec.maandsaldo$=STR$(ms):PUT 1
2480 kontrole=SEEKKEY(1,2,0,ark$):IF kontrole<>0 THEN STOP ELSE GET 1
2490 ms=VAL(rec.maandsaldo$)
2500 IF r=11 THEN ms=ms+tbtw+temp+nttb+nnttb ELSE ms=ms-(tbtw+temp+nttb+nnttb)
2510 LSET rec.maandsaldo$=STR$(ms):PUT 1
2520 CLOSE 1
2530 IF FIND$(drj$+"journaal.rnd")=""THEN GOSUB 2970 ELSE OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2
2540 FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
2550 n=0:nr$=STR$(n):kontrole=SEEKKEY(1,2,0,nr$):GET 1:n=VAL(rec.datum$)
2560 LSET rec.datum$=d$:LSET rec.omschr$=s$+MID$(STR$(anr),2,LEN(STR$(anr)))+" "+naam$
2570 IF mktr$="N"THEN 2600
2580 IF r=11 THEN bedrag=-temp-nttb-nnttb ELSE 2760
2590 LSET rec.num$=vkpmktr$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,vkpmktr$,y):GOTO 2800
2600 IF tb6=0 THEN 2630
2610 IF r=11 THEN bedrag=-tb6 ELSE 2750
2620 LSET rec.num$=rv6$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv6$,y)
2630 IF tb19=0 THEN 2660
2640 IF r=11 THEN bedrag=-tb19 ELSE 2750
2650 LSET rec.num$=rv19$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv19$,y)
2660 IF tb25=0 THEN 2690
2670 IF r=11 THEN bedrag=-tb25 ELSE 2750
2680 LSET rec.num$=rv25$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv25$,y)
2690 IF tb33=0 THEN 2720
2700 IF r=11 THEN bedrag=-tb33 ELSE 2750
2710 LSET rec.num$=rv33$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv33$,y)
2720 IF tb0=0 THEN 2750
2730 IF r=11 THEN bedrag=-tb0 ELSE 2750
2740 LSET rec.num$=rv0$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv0$,y)
2750 IF tb00=0 THEN 2780
2760 IF r=11 THEN bedrag=-tb00 ELSE 2780
2770 LSET rec.num$=rv00$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,rv00$,y)
2780 IF r=11 THEN 2800
2790 LSET rec.num$=creditnr$:bedrag=temp+nttb+nnttb:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,creditnr$,y)
2800 IF tbtw=0 THEN 2870
2810 IF r=11 THEN bedrag=-tbtw+ttaksbtw ELSE bedrag=tbtw-ttaksbtw
2820 IF r=11 THEN num$=btv$ELSE num$=btc$
2830 LSET rec.num$=num$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,num$,y)
2840 IF ttaksbtw=0 THEN 2870
2850 IF r=11 THEN bedrag=-ttaksbtw ELSE bedrag=ttaksbtw
2860 LSET rec.num$=taksen$:LSET rec.bedrg$=STR$(bedrag):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,taksen$,y)
2870 IF r<>11 THEN ms=-(temp+nnttb+nttb+tbtw)ELSE ms=temp+nttb+tbtw+nnttb
2880 LSET rec.num$=ark$:LSET rec.bedrg$=STR$(ms):n=n+1:nr$=STR$(n):kontrole=ADDREC(1,2,0,nr$):y=FETCHREC(1):kontrole=ADDKEY(1,2,1,ark$,y)
2890 nr=0:nr$=STR$(nr):kontrole=SEEKKEY(1,2,0,nr$):LSET rec.datum$=STR$(n):PUT 1
2900 CLOSE 1:recleng=162:OPEN"K",1,drk$+"klanten.rnd",drk$+"klanten.key",2,recleng
2910 FIELD 1,6 AS rec.numkl$,30 AS rec.naam$,30 AS rec.straat$,5 AS rec.postkode$,30 AS rec.stad$,12 AS rec.telefoon$,12 AS rec.btwnr$,9 AS rec.btwbedrag$,9 AS rec.jaaromzet$,9 AS rec.saldo$,4 AS rec.vervaldag$
2920 d$=dat$
2930 CHAIN"vfakt.mar",,ALL
2940 CREATE 1,drv$+"vkfaktr.rnd",drv$+"vkfaktr.key",2
2950 CLOSE 1
2960 RETURN
2970 CREATE 1,drj$+"journaal.rnd",drj$+"journaal.key",2,50
2980 CLOSE 1
2990 OPEN"K",1,drj$+"journaal.rnd",drj$+"journaal.key",2:FIELD 1,6 AS rec.num$,8 AS rec.datum$,20 AS rec.omschr$,9 AS rec.bedrg$
3000 nr=0:nr$=STR$(nr):LSET rec.datum$=nr$:LSET rec.omschr$="":LSET rec.bedrg$="":kontrole=ADDREC(1,0,0,nr$):IF kontrole<>0 THEN STOP ELSE PUT 1
3010 RETURN
3150 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
3160 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 3180 ELSE 3170
3170 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
3180 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,eol$+"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$