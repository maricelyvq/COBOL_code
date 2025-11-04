       IDENTIFICATION DIVISION.
       PROGRAM-ID. LMONEDA.
       AUTHOR. Maricely Villalobos.
       DATE-WRITTEN. August 2025.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT MFILE ASSIGN TO MONREC
                ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD MFILE.
          01  MRECORD.
            05 RMONEDA-CODE     PIC X(3).
            05 FILLER          PIC X(1).
            05 RMONEDA-SYMBOL   PIC X(3).
            05 FILLER          PIC X(1).
            05 RMONEDA-COUNTRY  PIC X(13).
            05 RMONEDA-NAME     PIC X(59).

       WORKING-STORAGE SECTION.
       01  MONEDA-TABLE.
           05  MONEDA-ITEM OCCURS 15 TIMES.
               10  MONEDA-CODE     PIC X(3).
               10  MONEDA-SYMBOL   PIC X(3).
               10  MONEDA-COUNTRY  PIC X(13).
               10  MONEDA-NAME     PIC X(19).
       01  CONTADOR     PIC 9(3) VALUE 0.
       01  INDICE       PIC 9(3) VALUE 1.
       PROCEDURE DIVISION.
           PERFORM INICIAR
           PERFORM PROCESAR
           PERFORM FINALIZAR
           CONTINUE.
           GOBACK.
       INICIAR.
              OPEN INPUT MFILE
              MOVE SPACE TO MONEDA-TABLE
              CONTINUE.

       PROCESAR.
           PERFORM UNTIL RMONEDA-CODE = 'END'
                READ MFILE
                    AT END MOVE 'END' TO RMONEDA-CODE
                NOT AT END
                    MOVE RMONEDA-CODE TO MONEDA-CODE (CONTADOR + 1)
                    MOVE RMONEDA-SYMBOL TO MONEDA-SYMBOL (CONTADOR + 1)
                    MOVE RMONEDA-COUNTRY TO MONEDA-COUNTRY (CONTADOR +
                    1)
                    MOVE RMONEDA-NAME TO MONEDA-NAME (CONTADOR + 1)
                    ADD 1 TO CONTADOR
                END-READ
           END-PERFORM
           CONTINUE.
        FINALIZAR.
              CLOSE MFILE
              PERFORM VARYING INDICE FROM 1 BY 1 UNTIL INDICE > CONTADOR
                   DISPLAY "Codigo: " MONEDA-CODE (INDICE)
                   DISPLAY "Simbolo: " MONEDA-SYMBOL (INDICE)
                   DISPLAY "Pais: " MONEDA-COUNTRY (INDICE)
                   DISPLAY "Nombre: " MONEDA-NAME (INDICE)
                   DISPLAY "------------------------"              END-PERFORM
              DISPLAY "Total de monedas procesadas: " CONTADOR
              MOVE SPACE TO MONEDA-TABLE
              CONTINUE.


