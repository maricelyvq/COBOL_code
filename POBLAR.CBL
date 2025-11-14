       IDENTIFICATION DIVISION.
       PROGRAM-ID. PBLRKSDS.
       AUTHOR. Maricely Villalobos.
       DATE-WRITTEN. August 2025.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT ARCHSEC ASSIGN TO SECIN
                ORGANIZATION IS SEQUENTIAL.

            SELECT TABLA ASSIGN TO VSAMOUT
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CAMPO-KEY.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHSEC
           RECORD CONTAINS 17 CHARACTERS.
       01 REG-SECUENCIAL.
          05 SCAMPO-KEY   PIC X(09).
          05 SCAMPO1      PIC X(03).
          05 SCAMPO2      PIC 9(03)V99.      *> 3 enteros, 2 decimales

       FD  TABLA.
       01 REG-VSAM.
          05 CAMPO-KEY    PIC X(09).
          05 CAMPO1       PIC X(03).
          05 CAMPO2       PIC 9(03)V99.

       WORKING-STORAGE SECTION.
       01 EOF-ARCHSEC     PIC X     VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT ARCHSEC
           OPEN OUTPUT TABLA

           PERFORM UNTIL EOF-ARCHSEC = 'Y'
                   READ ARCHSEC
                   AT END
                      MOVE 'Y' TO EOF-ARCHSEC
                   NOT AT END
                       MOVE REG-SECUENCIAL TO REG-VSAM
                       WRITE REG-VSAM
                   END-READ
           END-PERFORM
           CLOSE ARCHSEC
           CLOSE TABLA
           GOBACK.