      ******************************************************************
      * Author:JEFEFRSON MOTA(GERO)
      * Date:26/03/23
      * Purpose:VALIDACAO DE PASSWORD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VLMAILII.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 VERIFICA                              PIC 9.
        88 VERIFICA-OK                          VALUE "S" FALSE "N".
       77   CONTADOR                            PIC 9.
       77  CARACTER                             PIC 9(1).
       77 LETRA-MAIUSC                          PIC 9(1).
       77 LETRA-MINUSC                          PIC 9(1).
       77 NUMERO                                PIC 9(1).


       77 WS-USER-PASSWORD                      PIC X(8).
       PROCEDURE DIVISION.

           P02-PASSWORD.
            DISPLAY "CADASTRE O PASSWORD: "
           ACCEPT  WS-USER-PASSWORD
           DISPLAY "==================================================="
           .
           PERFORM VALIDAR-SENHA

           IF VERIFICA = 1
           DISPLAY "Senha valida!"
           ELSE
           DISPLAY "Senha invalida! Deve ter no maximo 8 caracteres"
                "no minimo 1 letra maiuscula, 1 minuscula e 1 numero !"
           PERFORM P02-PASSWORD
           END-IF.
           DISPLAY "==================================================="
           .
           VALIDAR-SENHA.
           PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 8

           IF CONTADOR = 1
            MOVE WS-USER-PASSWORD(1:1) TO CARACTER
            IF CARACTER >= "A" AND CARACTER <= "Z"
                ADD 1 TO LETRA-MAIUSC
            END-IF

           ELSE IF CONTADOR >= 2 AND CONTADOR <= 8
            MOVE WS-USER-PASSWORD(CONTADOR:1) TO CARACTER
            IF CARACTER >= "a" AND CARACTER <= "z"
                ADD 1 TO LETRA-MINUSC
            ELSE IF CARACTER >= "0" AND CARACTER <= "9"
                ADD 1 TO NUMERO
            END-IF

           END-IF
           END-PERFORM
           DISPLAY "==================================================="

           IF LETRA-MAIUSC >= 1 AND LETRA-MINUSC >= 1 AND NUMERO >= 1
           MOVE 1 TO VERIFICA
           END-IF.

           DISPLAY " LETRA MAIUSCULA: "LETRA-MAIUSC
           DISPLAY " LETRA MINUSC   : "LETRA-MINUSC
           DISPLAY " NUMERO         : "NUMERO.

       P02-FIM.
            STOP RUN.
       END PROGRAM VLMAILII.
