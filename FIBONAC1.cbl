       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONAC1.
       AUTHOR. Maricely Villalobos.
       DATE-WRITTEN. August 2025.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FIBONACCI.
           05 FIB-CONT        PIC 9(3) VALUE 1.
           05 FIB-NUM1         PIC 9(18) VALUE 0.
           05 FIB-NUM2         PIC 9(18) VALUE 1.
           05 FIB-SIG         PIC 9(18) VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY 'Fibonacci:'
           DISPLAY FIB-NUM1
           DISPLAY FIB-NUM2

           PERFORM UNTIL FIB-CONT > 98
               ADD FIB-NUM1 FIB-NUM2 GIVING FIB-SIG
               DISPLAY FIB-SIG
               MOVE FIB-NUM2 TO FIB-NUM1
               MOVE FIB-SIG TO FIB-NUM2
               ADD 1 TO FIB-CONT
           END-PERFORM

           GOBACK.
