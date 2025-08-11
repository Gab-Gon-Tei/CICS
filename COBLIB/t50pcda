      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         T10PCDA.
       AUTHOR.                             PEDRO.
      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
       77  WS-MSG-ERRO                     PIC X(80).
       77  WS-LENGTH                       PIC S9(04) COMP.

       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
           05 WS-ID-PROD                   PIC S9(09).
           05 WS-ID-PEDIDO                 PIC S9(09).
           05 WS-CONT-CARR                 PIC S9(09).
           05 WS-ID-CONTA-BANCARIA         PIC S9(09).
           05 WS-TIPO                      PIC X(01).
           05 WS-NUMERO-CARTAO             PIC X(16).

       01  WS-VAR-TEMPO.
           05 WS-DATA                      PIC X(10).
           05 WS-HORARIO                   PIC X(08).
       77  ANO-AUX                         PIC X(04).
       01  EXPIRACAO-F.
           05 WS-MES-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE '/'.
           05 WS-ANO-F                     PIC X(04).

       77  WS-SQLCODE                      PIC +9(09).
       77  WS-RESP                         PIC S9(08) COMP.
       77  WS-MSG                          PIC X(60).
       77  WS-FS                           PIC X(02).
       01  WS-ENDFILE-AUX                  PIC X(01).
           88 ENDFILE-YES                          VALUE 'S'.
           88 ENDFILE-NO                           VALUE 'N'.
       77  TELA                            PIC S9(09).

       01  CARTAO-ESPELHO.
           05 CARTAO-AUX                   PIC X(16).
           05 NOME-AUX                     PIC X(30).
           05 CVV-AUX                      PIC X(03).
           05 DATA-AUX                     PIC X(07).
           05 TIPO-AUX                     PIC X(1).
           05 ID-AUX                       PIC S9(09) COMP.
       01  WS-DATA-SYS.
           05 WS-ANO                          PIC 99.
           05 WS-MES                          PIC 99.
           05 WS-DIA                          PIC 99.
           COPY T10MCDA.
           COPY DFHAID.
           COPY DFHBMSCA.
      *
           EXEC SQL
              INCLUDE TCDCLPDD
           END-EXEC.
           EXEC SQL
              INCLUDE TCDCLCRT
           END-EXEC.
      *
      *
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
       01  DFHCOMMAREA.
           05 OCCURS 0 TO 24576 TIMES DEPENDING ON EIBCALEN
                                           PIC X(01).
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
           EXEC CICS HANDLE CONDITION
              NOTFND(999-NOTFND)
              MAPFAIL(999-MAPFAIL)
              ERROR(999-ERRO-GENERICO)
           END-EXEC
      ******************************************************************
      *         SELETOR DE FASE - O MENU PRINCIPAL POSSUI 2 FASES
      *         FASE 1 - ENVIA O MAPA PARA O TERMINAL
      *         FASE 2 - TRATA O CAMPO T2COD
      *         FASE 3 - TRATA O CAMPO T2CONF
      ******************************************************************
           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA

           EVALUATE WS-FASE
              WHEN '1' PERFORM 100-FASE1
              WHEN '2' PERFORM 200-FASE2
      *       WHEN '3' PERFORM 300-FASE3
              WHEN OTHER
                   MOVE +80                TO WS-LENGTH
                   MOVE 'ERRO NO NUMERO DA FASE'
                                           TO T5MSGO
      *
                   PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE
           .
       100-FASE1.
           MOVE LOW-VALUES                  TO MAPACDAO
           MOVE -1                          TO T5NCARTL
           MOVE 'DIGITE OS DADOS DO CARTAO!'
                                           TO T5MSGO


           PERFORM 999-TRATA-FASE2
           .
       200-FASE2.
           EXEC CICS HANDLE AID
              PF1     (210-PF1)
              PF2     (220-PF2)
              PF10    (260-PF10)
              PF11    (280-PF11)
              PF12    (290-PF12)
              ENTER   (290-ENTER)
              ANYKEY  (240-ANYKEY)
           END-EXEC
      *
           EXEC CICS RECEIVE
                MAP     ('MAPACDA')
                MAPSET  ('T10MCDA')
                INTO    (MAPACDAI)
           END-EXEC
           .
       210-PF1.
           IF T5NCARTL < 1 OR T5NIMPRL < 1 OR T5MESEL < 1 OR
              T5ANOEL < 1 OR T5CVVL < 1
              MOVE 'PREENCHA TODOS OS CAMPOS' TO T5MSGO
              PERFORM 999-TRATA-FASE2
           END-IF
           IF T5NCARTI IS NOT NUMERIC OR (T5NCARTL < 16)
              MOVE 'CARTAO INVALIDO' TO T5MSGO
              PERFORM 999-TRATA-FASE2
           END-IF
           IF T5MESEI IS NOT NUMERIC OR T5ANOEI IS NOT NUMERIC
              MOVE 'DATA TEM QUE SER NUMERICA' TO T5MSGO
              PERFORM 999-TRATA-FASE2
           END-IF
           ACCEPT WS-DATA-SYS FROM DATE
           MOVE WS-ANO                   TO ANO-AUX(3:2)
           MOVE 20                       TO ANO-AUX(1:2)
           EVALUATE TRUE
              WHEN T5MESEI > 12
                   MOVE 'EXISTE SOMENTE 12 MESES' TO T5MSGO
                   PERFORM 999-TRATA-FASE2
              WHEN T5ANOEI < ANO-AUX
                   MOVE 'ANO DE EXPIRACAO INVALIDA' TO T5MSGO
                   PERFORM 999-TRATA-FASE2
           END-EVALUATE
           IF T5CVVI IS NOT NUMERIC
              MOVE 'CVV SOMENTE NUMEROS' TO T5MSGO
              PERFORM 999-TRATA-FASE2
           END-IF
           MOVE T5NCARTI                     TO DCLCRT-NUMERO-CARTAO
           MOVE T5NIMPRI                     TO DCLCRT-NOME-IMPRESSO
           MOVE T5CVVI                       TO DCLCRT-CVV
           MOVE T5MESEI                      TO WS-MES-F
           MOVE T5ANOEI                      TO WS-ANO-F
           MOVE WS-TIPO                      TO DCLCRT-TIPO
           MOVE EXPIRACAO-F                  TO DCLCRT-DATA-EXPIRACAO
           MOVE WS-ID-CONTA-BANCARIA         TO DCLCRT-ID-CONTA-BANCARIA
           EXEC SQL
               SELECT NUMERO_CARTAO
                     ,NOME_IMPRESSO
                     ,CVV
                     ,DATA_EXPIRACAO
                     ,TIPO
                     ,ID_CONTA_BANCARIA
               INTO  :CARTAO-AUX
                    ,:NOME-AUX
                    ,:CVV-AUX
                    ,:DATA-AUX
                    ,:TIPO-AUX
                    ,:ID-AUX
               FROM CARTOES
               WHERE (NUMERO_CARTAO     = :DCLCRT-NUMERO-CARTAO) AND
                     (NOME_IMPRESSO     = :DCLCRT-NOME-IMPRESSO)  AND
                     (CVV               = :DCLCRT-CVV)            AND
                     (DATA_EXPIRACAO    = :DCLCRT-DATA-EXPIRACAO) AND
                     (TIPO              = :DCLCRT-TIPO)           AND
                     (ID_CONTA_BANCARIA = :DCLCRT-ID-CONTA-BANCARIA)
           END-EXEC
           EVALUATE TRUE
              WHEN SQLCODE = 0
                   MOVE DCLCRT-NUMERO-CARTAO   TO WS-NUMERO-CARTAO
                   MOVE '1'                    TO WS-FASE
                   EXEC CICS XCTL
                       PROGRAM('T10PCON')
                       COMMAREA(WS-DFHCOMMAREA)
                       LENGTH(LENGTH OF WS-DFHCOMMAREA)
                   END-EXEC
              WHEN SQLCODE = +100
                   MOVE '1'                    TO WS-FASE
                   EXEC CICS XCTL
                       PROGRAM('T10PCAV')
                       COMMAREA(WS-DFHCOMMAREA)
                       LENGTH(LENGTH OF WS-DFHCOMMAREA)
                   END-EXEC
              WHEN OTHER
                   PERFORM 999-ERRO-GENERICO
           END-EVALUATE
           .
       220-PF2.
           MOVE '1'                        TO WS-FASE
      *
           EXEC CICS XCTL
               PROGRAM('T11PPAG')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
      *
           .
       260-PF10.
           MOVE '1'                        TO WS-FASE
      *
           EXEC CICS XCTL
               PROGRAM('T11PCAR')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
      *
           .
       280-PF11.
           MOVE '1'                        TO WS-FASE
      *
           EXEC CICS XCTL
               PROGRAM('T11PPED')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
      *
           .
       290-PF12.
           MOVE '1'                        TO WS-FASE
           MOVE 'F'                        TO DCLPDD-ATIVO
           EXEC SQL
              UPDATE  PEDIDOS
              SET ATIVO       = :DCLPDD-ATIVO
              WHERE ID_PEDIDO = :DCLPDD-ID-PEDIDO
           END-EXEC

           IF SQLCODE NOT EQUAL 0
              MOVE 'ERRO UPDATE NA TABELA TBPRODUTO'
                                         TO T5MSGO
               MOVE SQLCODE            TO WS-SQLCODE
               PERFORM 999-ERRO-GENERICO
           END-IF
      *
           EXEC CICS XCTL
               PROGRAM('T11PLOG')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
      *
           .
       240-ANYKEY.
           MOVE 'TECLA PRESSIONADA INVALIDA'
                                           TO T5MSGO
           PERFORM 999-TRATA-FASE2
           .
       290-ENTER.
           MOVE 'TECLA PRESSIONADA INVALIDA'
                                           TO T5MSGO
           PERFORM 999-TRATA-FASE2
           .
       999-ENCERRA-TRANSACAO.
           MOVE +80                TO WS-LENGTH
           EXEC CICS SEND TEXT
               FROM (WS-MSG-ERRO)
               LENGTH(WS-LENGTH)
               ERASE FREEKB ALARM
           END-EXEC
      *
           EXEC CICS RETURN
           END-EXEC
           .
       999-MANDA-TELA.
           MOVE EIBTRMID                    TO T5TERO
           MOVE EIBTRNID                    TO T5TRAO
           MOVE EIBTASKN                    TO T5TASO
           MOVE WS-FASE                     TO T5FASO
      *
      *
           EXEC CICS LINK
              PROGRAM('AUXCICS1')
              COMMAREA(WS-VAR-TEMPO)
              LENGTH(+15)
           END-EXEC
           MOVE WS-DATA                    TO T5DATAO
           MOVE WS-HORARIO                 TO T5HORAO
      *
           EXEC CICS SEND
              MAP      ('MAPACDA')
              MAPSET   ('T10MCDA')
              FROM     (MAPACDAO)
              ERASE  FREEKB ALARM CURSOR
           END-EXEC
           .
       999-CHAMA-FASE1.
           MOVE '1'                        TO WS-FASE
      *
           EXEC CICS XCTL
               PROGRAM('T10PCDA')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
      *
           .
       999-CHAMA-FASE2.
           MOVE '2'                        TO WS-FASE
      *
           EXEC CICS RETURN
               TRANSID('FT0I')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH (LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       999-TRATA-FASE2.
           MOVE -1                        TO T5NCARTL
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .
       999-MAPFAIL.
           MOVE 'ERRO MAPA T10MCDA'       TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
       999-ERRO-GENERICO.
           MOVE SQLCODE             TO WS-SQLCODE
           STRING  'ERRO GENERICO' DELIMITED BY SPACES
                   'SQLC:' WS-SQLCODE DELIMITED BY SIZE
                      INTO WS-MSG-ERRO
      *    MOVE 'ERRO GENERICO'          TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
       999-NOTFND.
           MOVE 'NAO EXISTEM PEDIDOS  COM CODIGO MAIOR OU IGUAL AO INFOR
      -         'MADO'                     TO T5MSGO
           PERFORM 999-TRATA-FASE2
           .

