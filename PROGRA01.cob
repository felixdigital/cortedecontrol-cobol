       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRA01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO ENTRADA
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS ENTRADA-ST.

       DATA DIVISION.
       FILE SECTION.
       FD ENTRADA BLOCK CONTAINS 0 RECORDS
                  RECORDING MODE IS F
                  DATA RECORD IS ENTRADA-REG.
       01 ENTRADA-REG.
          05 ENTRADA-ID PIC 9(2).
          05 ENTRADA-CANT PIC 9(2).
          05 FILLER PIC X(76).
       WORKING-STORAGE SECTION.
       01 WS-ENTRADA.
          05 ENTRADA-ST PIC 9(2).
          05 FIN-FICHERO PIC 9(2) VALUE 10.
       01 WS-ENTRADA-REG.
          05 WS-ENTRADA-ID PIC 9(2).
          05 WS-ENTRADA-CANT PIC 9(2).
          05 WS-ENTRADA-ACUM PIC 9(4).

       PROCEDURE DIVISION.

           OPEN INPUT ENTRADA.

           INITIALIZE WS-ENTRADA-REG.

           DISPLAY "---------------------------------"
           DISPLAY "ID CANTIDAD".

           PERFORM UNTIL ENTRADA-ST = FIN-FICHERO

              READ ENTRADA NEXT RECORD

              IF ENTRADA-ST NOT = FIN-FICHERO

                 EVALUATE TRUE
                    WHEN ENTRADA-ID NOT = WS-ENTRADA-ID
                          IF WS-ENTRADA-ID NOT = 0
                             PERFORM MOSTRAR-REGISTRO-ANTERIOR
                             PERFORM MOSTRAR-DATOS
                             DISPLAY "---------------------------------"
                          END-IF
                          MOVE ENTRADA-ID TO WS-ENTRADA-ID
                          MOVE ENTRADA-CANT TO WS-ENTRADA-CANT
                          MOVE ENTRADA-CANT TO WS-ENTRADA-ACUM
                    WHEN ENTRADA-ID = WS-ENTRADA-ID
                         PERFORM MOSTRAR-REGISTRO
                         ADD ENTRADA-CANT TO WS-ENTRADA-ACUM
                 END-EVALUATE
              END-IF

           END-PERFORM.

           PERFORM MOSTRAR-REGISTRO-ANTERIOR.
           PERFORM MOSTRAR-DATOS.
           DISPLAY "---------------------------------".

           CLOSE ENTRADA.

           STOP RUN.

      *-------------------------------------------------------
      *MOSTRAR REGISTRO
      *-------------------------------------------------------
       MOSTRAR-REGISTRO.
            DISPLAY ENTRADA-ID " " ENTRADA-CANT.
      *-------------------------------------------------------
      *MOSTRAR REGISTRO ANTERIOR
      *-------------------------------------------------------
       MOSTRAR-REGISTRO-ANTERIOR.
            DISPLAY WS-ENTRADA-ID " " WS-ENTRADA-CANT.
      *-------------------------------------------------------
      *MOSTRAR DATOS
      *-------------------------------------------------------
       MOSTRAR-DATOS.
           DISPLAY
                   "ID: " WS-ENTRADA-ID " "
                   "TOTAL: " WS-ENTRADA-ACUM.

       END PROGRAM PROGRA01.
