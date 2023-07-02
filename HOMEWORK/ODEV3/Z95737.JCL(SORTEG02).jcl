//SORTEG02 JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=&SYSUID
//DELET100 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    DELETE Z95737.QSAM.AA NONVSAM
    DELETE Z95737.QSAM.BB NONVSAM
    IF LASTCC LE 08 THEN SET MAXCC = 00
//SORT0200 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD *
10002949SULEYMAN       BOZAN          19980811
10002840SULEYMAN       BOZAN          19980811
10002978SULEYMAN       BOZAN          19980811
10001949METIN          SAKTAT         19910804
10001840METIN          SAKTAT         19910804
//SORTOUT  DD DSN=Z95737.QSAM.AA,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=60)
//SYSIN    DD *
    SORT FIELDS=(1,7,CH,A)
    OUTREC FIELDS=(1,38,39,8,Y4T,TOJUL=Y4T,15C'0')
//DELET300 EXEC PGM=IEFBR14
//FILE01   DD DSN=Z95737.QSAM.BB,
//             DISP=(MOD,DELETE,DELETE),SPACE=(TRK,0)
//SORT0400 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=Z95737.QSAM.AA,DISP=SHR
//SORTOUT  DD DSN=Z95737.QSAM.BB,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=50)
//SYSIN    DD *
    SORT FIELDS=COPY
        OUTREC FIELDS=(1,5,ZD,TO=PD,LENGTH=3,
                       6,3,ZD,TO=PD,LENGTH=2,
                       9,30,
                       39,7,ZD,TO=PD,LENGTH=4,
                       46,15,ZD,TO=PD,LENGTH=8)