1 ON ERROR GOTO 20000
10 REM UABOEK.MAR=UITDRUKKEN AANKOOPBOEK DD versie 230687
20 esc$=CHR$(27):rev$=esc$+"p":nrm$=esc$+"q":cls$=esc$+"E"+esc$+"H":on$=esc$+"e":off$=esc$+"f"
30 flash$=esc$+"p":flashoff$=esc$+"q":DEF FNat$(l,k,a$)=esc$+"Y"+CHR$(32+l)+CHR$(32+k)+a$:eol$=esc$+"K":hel$=esc$+"p":zcht$=esc$+"q"
60 OPTION RUN
90 PRINT cls$:PRINT FNat$(12,12,"FAKTUREN/CREDITNOTA'S  (F/C) ?");
100 antw$="FC":m$=UPPER$(INKEY$):IF INSTR(antw$,m$)=0 OR m$=""THEN 100
110 IF m$="F"THEN r=1 ELSE r=3
120 IF m$="F"THEN r2=2 ELSE r2=4
130 BUFFERS 40
140 IF FIND$("version.vsf")=""THEN RUN"menu.mar"ELSE OPEN"i",1,"version.vsf",2:INPUT #1,drive$:CLOSE 1
150 OPEN"R",1,drive$+"99.rnd",30
160 FIELD 1,6 AS rec.nummer$
170 GET 1,r:anr=VAL(rec.nummer$):GET 1,20:d$=MID$(rec.nummer$,5,2)+"/"+MID$(rec.nummer$,3,2)+"/"+MID$(rec.nummer$,1,2):GET 1,r2:unr=VAL(rec.nummer$):FIELD 1,28 AS rec.bedrijf$:GET 1,46:bedrijf$=rec.bedrijf$:FIELD 1,28 AS rec.nummer$:GOSUB 720:CLOSE 1
175 IF bl=72 THEN lnbl=26 ELSE lnbl=23
176 LPRINT CHR$(27);"C";CHR$(bl);
178 LPRINT CHR$(27);"N";CHR$(1);
180 IF anr=unr THEN PRINT"AANKOOPBOEK IS REEDS UITGEDRUKT !!!!!"ELSE 200
190 FOR teller=1 TO 5000:NEXT teller:RUN"menu.mar"
200 unr=unr+1
210 OPEN"K",1,drl$+"leveranc.rnd",drl$+"leveranc.key",2
220 FIELD 1,6 AS rec.levnum$,30 AS rec.naam$,30 AS rec.straat$,30 AS rec.stad$,12 AS rec.telefoon$,9 AS rec.jaaromzet$,9 AS rec.saldo$,14 AS rec.rekening$,4 AS rec.vervaldag$
230 OPEN"K",3,draa$+"afactuur.rnd",draa$+"afactuur.key",2
240 FIELD 3,6 AS rec.nrl$,6 AS rec.nrr$,8 AS rec.dat$,20 AS rec.oms$,9 AS rec.hg$,9 AS rec.btw$,9 AS rec.nbtw$,9 AS rec.rbet$,7 AS rec.bdok$,7 AS rec.kode$,9 AS rec.invvhf$,9 AS rec.sptaks$,9 AS rec.btwmedektr$,9 AS rec.invest$
250 z$=STRING$(132,"-")
260 IF m$="F"OR m$="f"THEN tp$="UITDRUKKEN AANKOOPBOEK FAKTUREN"ELSE tp$="UITDRUKKEN AANKOOPBOEK CREDITNOTA'S"
270 PRINT cls$:PRINT FNat$(0,0,tp$):PRINT FNat$(1,0,STRING$(80,CHR$(172)))
280 PRINT FNat$(8,12,"DATUM DRUKKEN : ");hel$;d$:PRINT FNat$(8,28,"");:INPUT"",dat$:PRINT zcht$;:IF dat$=""THEN dat$=d$
290 PRINT FNat$(17,12,"GECONDENSEERD AFDRUKKEN (J/N) ? ");
300 antw$="JN":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 300
310 IF at$="J"THEN LPRINT CHR$(15);ELSE LPRINT CHR$(18);
320 PRINT FNat$(19,12,"PRINTER KLAAR ? DRUK <V> VOOR VERVOLG - <M> VOOR MENU");
330 antw$="VM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 330
340 IF at$="M"THEN 710
350 nr=anr+1
360 t=0:pteller=0
370 REM uitdrukken hoofdding
380 pteller=pteller+1:lteller=0
390 LPRINT
400 LPRINT CHR$(14);bedrijf$
410 IF r=1 THEN tp$="AANKOOPFAKTUREN"ELSE tp$="CREDITNOTA'S OP AANKOOPFAKTUREN"
420 LPRINT CHR$(14);"AANKOOPBOEK";STRING$(45," ");"PAGINA : ";USING"##";pteller:LPRINT tp$;TAB(113);"DATUM : ";dat$:LPRINT
430 IF r=1 THEN tp2$="BTW VAK 21"ELSE tp2$="BTW VK31/32"
440 LPRINT z$:LPRINT"LIJN";TAB(6);"DATUM";TAB(15);"NAAM EN OMSCHRIJVING";TAB(62);"FAKT.";TAB(70);"LEVERANCIERS";TAB(85);"GOEDEREN VAK51";TAB(100);"INVEST. VAK 52";TAB(115);tp2$
450 LPRINT TAB(70);"BTW N.AFTRKB.";TAB(85);"MEDEKTR. VAK14";TAB(100);"BTW INV. VERL. HEFF. VAK13":LPRINT z$:LPRINT
460 REM afdrukken faktuurgegevens
470 IF r=1 THEN s$="AF"ELSE s$="CA"
480 sleutel$=MID$(d$,7,2)+s$+STR$(unr):kontrole=SEEKKEY(3,2,1,sleutel$):GET 3
490 kontrole=SEEKKEY(1,2,0,rec.nrl$):GET 1
500 t=t+1:tot=VAL(rec.hg$)+VAL(rec.btw$)+VAL(rec.nbtw$)+VAL(rec.sptaks$)+VAL(rec.invvhf$)-VAL(rec.btwmedektr$)
510 LPRINT t;TAB(6);rec.dat$;TAB(15);MID$(rec.naam$,1,25)+" "+rec.oms$;TAB(62);rec.kode$;TAB(70);USING"#########";tot;:LPRINT TAB(85);USING"#########";VAL(rec.hg$);:LPRINT TAB(100);USING"#########";VAL(rec.invest$);:
520 LPRINT TAB(115);USING"#########";VAL(rec.btw$)+VAL(rec.sptaks$):LPRINT TAB(70);USING"#########";VAL(rec.nbtw$);:LPRINT TAB(85);USING"#########";VAL(rec.btwmedektr$);:LPRINT TAB(100);USING"#########";VAL(rec.invvhf$)
530 ttot=ttot+tot:thg=thg+VAL(rec.hg$):tbtw=tbtw+VAL(rec.btw$)+VAL(rec.sptaks$):tnbtw=tnbtw+VAL(rec.nbtw$):tmktr=tmktr+VAL(rec.btwmedektr$):tinvhf=tinvhf+VAL(rec.invvhf$):tinvest=tinvest+VAL(rec.invest$)
540 IF unr=anr THEN 590 ELSE unr=unr+1
550 lteller=lteller+1
560 IF lteller<lnbl THEN 580
570 LPRINT CHR$(12);:GOTO 380
580 GOTO 470
590 LPRINT z$:LPRINT TAB(5);"T O T A A L :";TAB(70);USING"#########";ttot;:LPRINT TAB(85);USING"#########";thg;:LPRINT TAB(100);USING"#########";tinvest;:LPRINT TAB(115);USING"#########";tbtw:LPRINT TAB(70);USING"#########";tnbtw;
600 LPRINT TAB(85);USING"#########";tmktr;:LPRINT TAB(100);USING"#########";tinvhf
610 LPRINT:LPRINT
620 IF r=1 THEN LPRINT"TOTAAL DEBET : ";thg;" + ";tbtw;" + ";tnbtw;" + ";tmktr;" + ";tinvhf;" = ";thg+tbtw+tnbtw+tinvhf-tmktr ELSE LPRINT"TOTAAL CREDIT : ";thg;" + ";tbtw;" + ";tnbtw;" + ";tmktr;" + ";tinvhf;" = ";thg+tbtw+tnbtw-tmktr+tinvhf
630 IF r=1 THEN LPRINT"TOTAAL CREDIT : ";ttot ELSE LPRINT"TOTAAL DEBET : ";ttot
640 LPRINT CHR$(12);
650 PRINT FNat$(19,12,eol$):PRINT FNat$(19,12,"DRUK <M> VOOR MENU - <H> VOOR HERDRUKKEN ");
660 antw$="MH":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 660
670 CLOSE 1:CLOSE 3
680 ttot=0:thg=0:tbtw=0:tnbtw=0:t=0:tmktr=0:tinvhf=0:tinvest=0
690 IF at$="H"THEN 90
700 OPEN"R",1,drt$+"99.rnd",30:FIELD 1,6 AS rec.nummer$:LSET rec.nummer$=STR$(unr):PUT 1,r2:CLOSE 1
710 RUN"menu.mar"
720 GET 1,61:drk$=rec.nummer$:GET 1,62:drl$=rec.nummer$:GET 1,63:drr$=rec.nummer$:GET 1,64:dra$=rec.nummer$:GET 1,65:drb$=rec.nummer$:GET 1,66:draa$=rec.nummer$:GET 1,67:drv$=rec.nummer$:GET 1,68:drf$=rec.nummer$:GET 1,69:drj$=rec.nummer$
730 IF MID$(drj$,3,1)="/"THEN drk$=MID$(drk$,1,15):drl$=MID$(drl$,1,15):drr$=MID$(drr$,1,15):dra$=MID$(dra$,1,15):drb$=MID$(drb$,1,15):draa$=MID$(draa$,1,15):drv$=MID$(drv$,1,15):drf$=MID$(drf$,1,15):drj$=MID$(drj$,1,15):drt$=drk$:GOTO 750 ELSE 740
740 drk$=MID$(drk$,1,2):drl$=MID$(drl$,1,2):drr$=MID$(drr$,1,2):dra$=MID$(dra$,1,2):drb$=MID$(drb$,1,2):draa$=MID$(draa$,1,2):drv$=MID$(drv$,1,2):drf$=MID$(drf$,1,2):drj$=MID$(drj$,1,2):drt$=drk$
750 GET 1,72:bl=VAL(rec.nummer$):RETURN
20000 PRINT FNat$(23,0,"foutkode :");ERR
20010 PRINT FNat$(23,30,"<H>ERHALEN - <O>VERSLAAN - <M>ENU ?");eol$;
20020 antw$="HOM":at$=UPPER$(INKEY$):IF INSTR(antw$,at$)=0 OR at$=""THEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"THEN 20040 ELSE RESUME NEXT
20040 CLOSE 1:CLOSE 2:CLOSE 3:RUN"menu.mar
HEN 20020
20030 IF at$="H"THEN RESUME ELSE IF at$="M"T