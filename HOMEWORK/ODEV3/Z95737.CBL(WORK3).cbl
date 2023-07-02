        IDENTIFICATION DIVISION.
      *IDENTIFICATION DIVISION ve ENVIRONMENT DIVISION bölümleri:
      *Program kimlik bilgilerini (PROGRAM-ID ve AUTHOR) ve çalma
      *ortam tanmlarn içerir.
       PROGRAM-ID. WORK3.
       AUTHOR. Suleyman Bozan.
       ENVIRONMENT DIVISION.
      *INPUT-OUTPUT SECTION ve FILE-CONTROL bölümleri: Giri ve çk
      *dosyalarnn tanmlarn içerir.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE ASSIGN TO IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS RANDOM
                             RECORD KEY IDX-KEY
                             STATUS ST-IDX-FILE.
           SELECT INP-FILE ASSIGN TO INPFILE
                             STATUS ST-INP-FILE.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS ST-OUT-FILE.
       DATA DIVISION.
      *DATA DIVISION bölümü: Veri alanlarnn ve dosya tanmlarnn
      *bulunduu bölümdür. FD ifadeleri, dosyalarn tanmlarn içerir.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
         01  OUT-REC.
           03 REC-ID-O          PIC 9(5).
           03 REC-DVZ-O         PIC 9(3).
           03 REC-NAME-O        PIC X(15).
           03 REC-SRNAME-O      PIC X(15).
           03 REC-DATE-O        PIC 9(08).
           03 REC-BALANCE-O     PIC 9(15).
       FD  INP-FILE RECORDING MODE F.
         01  INP-REC.
           03 INP-ID            PIC X(5).
           03 INP-DVZ           PIC X(3).
       FD  IDX-FILE.
         01  IDX-REC.
           03 IDX-KEY.
              05 IDX-ID         PIC S9(5) COMP-3.
              05 IDX-DVZ        PIC S9(3) COMP.
           03 IDX-NAME          PIC X(15).
           03 IDX-SRNAME        PIC X(15).
           03 IDX-DATE          PIC S9(07) COMP-3.
           03 IDX-BALANCE       PIC S9(15) COMP-3.

       WORKING-STORAGE SECTION.
      *WORKING-STORAGE SECTION bölümü: Program çalmas için kullanlan
      *geçici deikenlerin tanmland bölümdür.
         01  WS-WORK-AREA.
           03 INT-DATE          PIC 9(7).
           03 GREG-DATE         PIC 9(8).
           03 ST-INP-FILE       PIC 9(2).
              88 INP-FILE-EOF                   VALUE 10.
              88 INP-SUCCESS                    VALUE 00 97.
           03 ST-OUT-FILE       PIC 9(2).
              88 OUT-SUCCESS                    VALUE 00 97.
           03 ST-IDX-FILE       PIC 9(2).
              88 IDX-SUCCESS                    VALUE 00 97.

      *--------------------
       PROCEDURE DIVISION.
      *PROCEDURE DIVISION bölümü: Programn ana prosedürü burada yer
      *alr. 0000-MAIN etiketiyle balar.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           READ INP-FILE.
           PERFORM H200-PROCESS UNTIL INP-FILE-EOF
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
      *H100-OPEN-FILES adl bir prosedür, giri ve çk dosyalarn
      *açar ve olas hata durumlarn kontrol eder.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT  IDX-FILE.
           IF (ST-INP-FILE NOT = 0) AND (ST-INP-FILE NOT = 97)
           DISPLAY 'UNABLE TO OPEN INPFILE: ' ST-INP-FILE
           MOVE ST-INP-FILE TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (ST-OUT-FILE NOT = 0) AND (ST-OUT-FILE NOT = 97)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' ST-OUT-FILE
           MOVE ST-OUT-FILE TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (ST-IDX-FILE NOT = 0) AND (ST-IDX-FILE NOT = 97)
           DISPLAY 'UNABLE TO OPEN IDXFILE: ' ST-IDX-FILE
           MOVE ST-IDX-FILE TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
       H100-END. EXIT.

       H200-PROCESS.
      *H200-PROCESS adl bir prosedür, giri dosyasndan veri okur ve
      *bu veriyi indeksli dosyada arar. Geçerli kaytlarn ilenmesi ve
      *yazlmas için H220-VALIDREC adl bir prosedür çarlr.
      *Geçersiz bir kayt durumunda ise H210-INVALIDMESSAGE adl bir
      *prosedür çarlr.
           COMPUTE IDX-ID = FUNCTION NUMVAL(INP-ID)
           COMPUTE IDX-DVZ = FUNCTION NUMVAL (INP-DVZ)
           READ IDX-FILE KEY IS IDX-KEY
           INVALID KEY PERFORM H210-INVALIDMESSAGE
           NOT INVALID KEY PERFORM H220-VALIDREC.
       H200-END. EXIT.

       H210-INVALIDMESSAGE.
      *H210-INVALIDMESSAGE adl bir prosedür, geçersiz bir anahtar
      *durumunda bir hata mesaj görüntüler ve bir sonraki giri
      *kaydn okur.
           DISPLAY 'INVALID KEY :' IDX-ID IDX-DVZ.
           READ INP-FILE.
       H210-END. EXIT.

       H220-VALIDREC.
      *H220-VALIDREC adl bir prosedür, geçerli bir kaydn ilenmesini
      *salar. Verileri uygun alanlara tar ve çk dosyasna yazar.
      *Son olarak bir sonraki giri kaydn okur.
           COMPUTE INT-DATE = FUNCTION INTEGER-OF-DAY(IDX-DATE)
           COMPUTE GREG-DATE = FUNCTION DATE-OF-INTEGER(INT-DATE)
           MOVE IDX-ID TO REC-ID-O.
           MOVE IDX-DVZ TO REC-DVZ-O.
           MOVE IDX-NAME TO REC-NAME-O.
           MOVE IDX-SRNAME TO REC-SRNAME-O.
           MOVE GREG-DATE TO REC-DATE-O.
           MOVE IDX-BALANCE TO REC-BALANCE-O.
           WRITE OUT-REC.
           READ INP-FILE.
       H220-END. EXIT.
      *
       H999-PROGRAM-EXIT.
      *H999-PROGRAM-EXIT adl bir prosedür, programn sonlandrlmas
      *için kullanlr. Dosyalar kapatlr ve program durdurulur.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           CLOSE IDX-FILE.
           STOP RUN.
       H999-END. EXIT.
      *Bu ekilde, program giri dosyasndan kaytlar okur,
      *bu kaytlar indeksli dosyada arar, uygun alanlara tar ve
      *çk dosyasna yazar. Program, giri dosyasndaki tüm kaytlar
      *ilendikten sonra sonlanr.
