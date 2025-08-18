      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         T04PPRL.
       AUTHOR.                             FELIPE.

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

      *----------------------------------------------------------------*
      *    IMPRESSAO DO MAPA                           *
      *----------------------------------------------------------------*
       01  LD-LINDET.
           05 FILLER                       PIC X(01) VALUE SPACES.

           05 LD-ID                        PIC ZZZZZ9.
           05 FILLER                       PIC X(03) VALUE SPACES.

           05 LD-PRODUTO                   PIC X(25).
           05 FILLER                       PIC X(02) VALUE SPACES.

           05 LD-VNDDPOR                   PIC X(20).
           05 FILLER                       PIC X(03) VALUE SPACES.

           05 FILLER                       PIC X(02) VALUE 'R$'.
           05 LD-VALOR                     PIC Z.ZZZ.ZZ9,99.
           05 FILLER                       PIC X(06) VALUE SPACES.

      *----------------------------------------------------------------*
      *    VARIAVEIS DE DATA
      *----------------------------------------------------------------*

       01  WS-DATA.
           05 WS-ANO                       PIC X(02).
           05 WS-MES                       PIC X(02).
           05 WS-DIA                       PIC X(02).

       01  WS-HORARIO.
           05 WS-HORA                      PIC X(02).
           05 WS-MIN                       PIC X(02).
           05 WS-SEG                       PIC X(02).

       01  WS-DATA-F.
           05 WS-ANO-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE '/'.
           05 WS-MES-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE '/'.
           05 WS-DIA-F                     PIC X(02).

       01  WS-HORARIO-F.
           05 WS-HORA-F                    PIC X(02).
           05 FILLER                       PIC X(01) VALUE ':'.
           05 WS-MIN-F                     PIC X(02).
           05 FILLER                       PIC X(01) VALUE ':'.
           05 WS-SEG-F                     PIC X(02).

      *----------------------------------------------------------------*
      *    DFHCOMMAREA E FILA
      *----------------------------------------------------------------*

       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-CPF-DFHCOMMAREA           PIC X(11).
           05 WS-ID-PRODUTO                PIC 9(09).
           05 WS-CONT-CARR                 PIC 9(09).
           05 WS-ID-PEDIDO                 PIC 9(09).
           05 WS-ID-CONTA-BANCARIA         PIC 9(09).
           05 WS-PAGA                      PIC 9(02).
           05 WS-PAGT                      PIC 9(02).
                 
       77  WS-EIBRESP                      PIC X(30).
       77  WS-SQLCODE                      PIC +9(9).
       77  WS-FILE-NAME                    PIC X(08)
                                                   VALUE 'T04MPRLT'.
       77  WS-QUEUE-NAME                   PIC X(08)
                                                   VALUE 'QT3PPRD'.
       77  WS-ITEM                         PIC S9(04) COMP.
       77  WS-RESP                         PIC S9(08) COMP.
       77  WS-RESTO                        PIC 9(01).
       77  WS-INDEX                        PIC 9(02).
       77  WS-ATIVO                  PIC X(01) VALUE 'A'.

       01  WS-ENDFILE-AUX                  PIC X(01).
           88 ENDFILE-YES                          VALUE 'S'.
           88 ENDFILE-NO                           VALUE 'N'.

      *----------------------------------------------------------------*
      *    AUXILIARES
      *----------------------------------------------------------------*

       01  WS-AUXILIARES.
           05  WS-PRODUTO                   PIC X(24).
           05  WS-PRODUTO-AUX               PIC X(24).
           05  WS-PRODUTO-MAIN              PIC X(25).
           05  WS-VENDIDO-POR               PIC X(19).
           05  WS-VENDIDO-POR-AUX           PIC X(19).
           05  WS-VENDIDO-MAIN              PIC X(20).
           05  WS-ID                        PIC 9(9).
           05  WS-VALOR-MINIMO              PIC S9(7) USAGE COMP-3.
           05  WS-VALOR-MAXIMO              PIC S9(7) USAGE COMP-3.

       01  WS-DFHCOMMAREA-AUX.
           05 WS-FASE-AUX                  PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
           05 WS-ID-PROD                   PIC 9(09).
           05 WS-CONT-CARR                 PIC 9(09).

           COPY T04MPRL.

           COPY DFHAID.
           COPY DFHBMSCA.

           EXEC SQL
              INCLUDE TCDCLPRP
           END-EXEC.
           EXEC SQL
              INCLUDE TCDCLPDD
           END-EXEC.

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
              MAPFAIL (999-MAPFAIL)
              ERROR   (999-ERROR  )
           END-EXEC

      * SELETOR DE FASE - A CONSULTA POSSUI 4 FASES
      *    FASE 1 - ENVIA O MAPA PARA O TERMINAL
      *    FASE 2 - TRATA O CAMPO T2COD (CHAVE)
           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA

           IF EIBCALEN = 0
              MOVE '1'                     TO WS-FASE
           END-IF

           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA

           EVALUATE WS-FASE
              WHEN '1' PERFORM 100-FASE1
              WHEN '2' PERFORM 200-FASE2
              WHEN OTHER
                 MOVE +80                  TO WS-LENGTH
                 MOVE 'ERRO NO NUMERO DA FASE'
                                           TO WS-MSG-ERRO
                 PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE
           .

        100-FASE1.
           MOVE '1'                        TO WS-FASE
           MOVE -1                         TO PRDL

           INITIALIZE WS-AUXILIARES

           EXEC SQL
               DECLARE LISTADB2 CURSOR FOR
               SELECT ID_PRODUTO
                     ,NOME_PRODUTO
                     ,VENDIDO_POR
                     ,VALOR_UNITARIO
               FROM PRODUTOS
               WHERE UPPER(NOME_PRODUTO) LIKE :WS-PRODUTO-MAIN
               AND VALOR_UNITARIO
               BETWEEN :WS-VALOR-MINIMO AND :WS-VALOR-MAXIMO
               AND VENDIDO_POR LIKE :WS-VENDIDO-MAIN
               ORDER BY NOME_PRODUTO
           END-EXEC

           MOVE '%'                        TO WS-PRODUTO
           MOVE 0                          TO WS-VALOR-MINIMO
           MOVE 99999                      TO WS-VALOR-MAXIMO
           MOVE '%'                        TO WS-VENDIDO-POR

           PERFORM 999-CHAMA-CURSOR
           .

      *----------------------------------------------------------------*
      *    SETOR FASE2, BINDS DE COMANDO                               *
      *----------------------------------------------------------------*

        200-FASE2.
           MOVE '2'                        TO WS-FASE
           EXEC CICS HANDLE AID
               ENTER   (200-ENTER)
               PF1     (210-PF1)
               PF2     (220-PF2)
               PF5     (250-PF5)
               PF7     (270-PF7)
               PF8     (280-PF8)
               PF10    (290-PF10)
               PF12    (212-PF12)
              ANYKEY   (240-ANYKEY)
           END-EXEC

           EXEC CICS RECEIVE
              MAP   ('T04MPRL')
              MAPSET('T04MPRL')
              INTO  (T04MPRLI)
           END-EXEC
           .

        200-ENTER.
           INITIALIZE WS-AUXILIARES

           IF PRDL EQUAL 0 OR PRDI EQUAL SPACES
               MOVE '%'                TO WS-PRODUTO
           ELSE
               IF PRDI(1:1) = SPACE
                   MOVE PRDI                   TO WS-PRODUTO
                   INSPECT WS-PRODUTO REPLACING LEADING SPACES BY '%'
               ELSE
                   MOVE PRDI                   TO WS-PRODUTO
               END-IF
           END-IF

           IF VLRMINL EQUAL 0 OR VLRMINI EQUAL ZEROES
               MOVE 0                      TO WS-VALOR-MINIMO
           ELSE
               EXEC CICS BIF DEEDIT FIELD(VLRMINI)END-EXEC
               MOVE VLRMINI                TO WS-VALOR-MINIMO
           END-IF

           IF VLRMAXL EQUAL 0 OR VLRMAXI EQUAL ZEROES
               MOVE 99999                     TO WS-VALOR-MAXIMO
           ELSE
               EXEC CICS BIF DEEDIT FIELD(VLRMAXI)END-EXEC
               MOVE VLRMAXI                TO WS-VALOR-MAXIMO
           END-IF

           IF FTVDDPRL EQUAL 0 OR FTVDDPRI EQUAL SPACES
              MOVE ALL '%'                 TO WS-VENDIDO-POR
           ELSE
               IF FTVDDPRI(1:1) = SPACE
                  MOVE FTVDDPRI               TO WS-VENDIDO-POR
                  INSPECT WS-VENDIDO-POR REPLACING LEADING SPACES BY '%'
               ELSE
                  MOVE '%'                    TO WS-VENDIDO-POR(1:1)
                  MOVE FTVDDPRI               TO WS-VENDIDO-POR(2:18)
               END-IF
           END-IF

           PERFORM 999-CHAMA-CURSOR
           .

        210-PF1.
           IF EIBCPOSN <= 560 OR EIBCPOSN >= 1520
              MOVE 'POSICAO INVALIDA DO CURSOR'
                                           TO MSGO
             PERFORM 999-TRATA-FASE2

           END-IF

           COMPUTE WS-INDEX = EIBCPOSN / 80 - 6
           MOVE LINDETI(WS-INDEX)         TO LD-LINDET

           IF LD-PRODUTO EQUAL SPACES
              MOVE 'POSICAO NAO CONTEM PRODUTO'
                                           TO MSGO
              PERFORM 999-TRATA-FASE2

           END-IF

           MOVE '1'                            TO WS-FASE-AUX
           MOVE WS-CPF-DFHCOMMAREA             TO WS-ID-CPF


           MOVE LD-ID                          TO WS-ID-PROD


           EXEC CICS XCTL
                PROGRAM('T04PDTL')
                COMMAREA(WS-DFHCOMMAREA-AUX)
                LENGTH(LENGTH OF WS-DFHCOMMAREA-AUX)
           END-EXEC
           .

        220-PF2.
           MOVE '1'                          TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T04PLOG')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

        240-ANYKEY.
           MOVE 'TECLA PRESSIONADA INVALIDA!'
                                           TO MSGO
      *    PERFORM 999-CARREGA-DETALHES
           PERFORM 999-TRATA-FASE2
           .

        250-PF5.
           MOVE '1'                        TO WS-FASE

           MOVE 'LIMPEZA DE TELA EXECUTADA' TO MSGO
           MOVE -1                          TO PRDL
           EXEC CICS XCTL
               PROGRAM('T04PPRL')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

        270-PF7.
           IF WS-PAGA = 1
              MOVE 'TETO DA PAGINA ANTIGIDA'
                                          TO MSGO
              PERFORM 999-CARREGA-DETALHES
           ELSE
              SUBTRACT 1 FROM WS-PAGA
              PERFORM 999-CARREGA-DETALHES
           END-IF

           PERFORM 999-TRATA-FASE2
           MOVE -1                        TO PRDL
           .

        280-PF8.
           IF WS-PAGA = WS-PAGT
              MOVE 'ULTIMA PAGINA ANTIGIDA'
                                          TO MSGO
              PERFORM 999-CARREGA-DETALHES
           ELSE
              ADD 1 TO WS-PAGA
              PERFORM 999-CARREGA-DETALHES
           END-IF

           PERFORM 999-TRATA-FASE2
           MOVE -1                        TO PRDL
           .

        212-PF12.
           MOVE '1'                          TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T04PLOG')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC.

        290-PF10.
           EXEC SQL
               SELECT ID_PEDIDO
               INTO :DCLPDD-ID-PEDIDO
               FROM PEDIDOS
               WHERE CPF = :WS-CPF-DFHCOMMAREA
                 AND ATIVO = :WS-ATIVO
           END-EXEC

           MOVE '1'                        TO WS-FASE
           MOVE DCLPDD-ID-PEDIDO           TO WS-ID-PEDIDO

           EXEC CICS XCTL
               PROGRAM('T04PCAR')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
      *----------------------------------------------------------------*
      *    SETOR SQL                                                   *
      *----------------------------------------------------------------*

        999-CARREGA-TS.
           EXEC SQL
               FETCH LISTADB2 INTO
                    :DCLPRD-ID-PRODUTO,
                    :DCLPRD-NOME-PRODUTO,
                    :DCLPRD-VENDIDO-POR,
                    :DCLPRD-VALOR-UNITARIO
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                    ADD 1                    TO WS-ITEM
                    MOVE DCLPRD-ID-PRODUTO      TO LD-ID
      *             MOVE WS-ID                  TO LD-ID
                    MOVE DCLPRD-NOME-PRODUTO    TO LD-PRODUTO
                    MOVE DCLPRD-VENDIDO-POR     TO LD-VNDDPOR
                    MOVE DCLPRD-VALOR-UNITARIO  TO LD-VALOR

                  EXEC CICS WRITEQ TS
                     QUEUE(WS-QUEUE-NAME)
                     FROM(LD-LINDET)
                     ITEM(WS-ITEM)
                     RESP(WS-RESP)
                  END-EXEC

                  IF WS-RESP NOT = DFHRESP(NORMAL)
                     PERFORM 999-ERROR
                  END-IF

               WHEN +100
                IF WS-ITEM = 0
                   MOVE 'PRODUTO NAO ENCONTRADO'     TO MSGO
                   PERFORM 999-TRATA-FASE2
                ELSE
                   SET ENDFILE-YES TO TRUE
                END-IF
            .

        999-CHAMA-CURSOR.
           MOVE FUNCTION REVERSE(WS-PRODUTO) TO WS-PRODUTO-AUX
           INSPECT WS-PRODUTO-AUX REPLACING LEADING SPACES BY '%'
           MOVE FUNCTION REVERSE(WS-PRODUTO-AUX) TO WS-PRODUTO

           MOVE FUNCTION REVERSE(WS-VENDIDO-POR) TO WS-VENDIDO-POR-AUX
           INSPECT WS-VENDIDO-POR-AUX REPLACING LEADING SPACES BY '%'
           MOVE FUNCTION REVERSE(WS-VENDIDO-POR-AUX) TO WS-VENDIDO-POR

           MOVE '%'                        TO WS-PRODUTO-MAIN(1:1)
           MOVE WS-PRODUTO                 TO WS-PRODUTO-MAIN(2:24)

           MOVE '%'                        TO WS-VENDIDO-MAIN(1:1)
           MOVE WS-VENDIDO-POR             TO WS-VENDIDO-MAIN(2:19)

           EXEC CICS DELETEQ TS
              QUEUE(WS-QUEUE-NAME)
              RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL) OR DFHRESP(QIDERR)
              CONTINUE
           ELSE
              PERFORM 999-ERROR
           END-IF

           EXEC SQL
               OPEN LISTADB2
           END-EXEC

           MOVE ZERO                       TO WS-ITEM
           SET ENDFILE-NO TO TRUE
           PERFORM 999-CARREGA-TS UNTIL ENDFILE-YES

           EXEC SQL
              CLOSE LISTADB2
           END-EXEC

           MOVE 1                          TO WS-PAGA

           DIVIDE WS-ITEM BY 12 GIVING WS-PAGT REMAINDER WS-RESTO

           IF WS-RESTO > 0
              ADD 1 TO WS-PAGT
           END-IF

           PERFORM 999-CARREGA-DETALHES
           PERFORM 999-TRATA-FASE2
           .

        999-CARREGA-DETALHES.
           COMPUTE WS-ITEM = (WS-PAGA - 1) * 10 + 1
           MOVE 0                          TO WS-INDEX

           PERFORM WITH TEST AFTER UNTIL WS-INDEX = 10
              EXEC CICS READQ TS
                 QUEUE (WS-QUEUE-NAME)
                 INTO  (LD-LINDET)
                 ITEM  (WS-ITEM)
                 RESP  (WS-RESP)
              END-EXEC

              IF WS-RESP = DFHRESP(NORMAL)
                 ADD 1 TO WS-INDEX
                 MOVE LD-LINDET            TO LINDETO(WS-INDEX)
              ELSE
                 IF WS-RESP = DFHRESP(ITEMERR)
                    PERFORM WITH TEST AFTER UNTIL WS-INDEX = 10
                       ADD 1 TO WS-INDEX
                       MOVE SPACES         TO LINDETO(WS-INDEX)
                    END-PERFORM
                 ELSE
                    PERFORM 999-ERROR
                 END-IF
              END-IF
              ADD 1 TO WS-ITEM
           END-PERFORM
           .

      *----------------------------------------------------------------*
      *    SETOR DE TELAS CICS                                         *
      *----------------------------------------------------------------*

        999-MANDA-TELA.
           MOVE EIBTRMID                  TO TERMO
           MOVE EIBTRNID                  TO TRANSO
           MOVE EIBTASKN                  TO TASKO
           MOVE WS-FASE                   TO FASEO

           ACCEPT WS-DATA    FROM DATE
           ACCEPT WS-HORARIO FROM TIME

           MOVE WS-DIA     TO WS-DIA-F
           MOVE WS-MES     TO WS-MES-F
           MOVE WS-ANO     TO WS-ANO-F

           MOVE WS-HORA    TO WS-HORA-F
           MOVE WS-MIN     TO WS-MIN-F
           MOVE WS-SEG     TO WS-SEG-F

           MOVE WS-DATA-F                 TO DATAO
           MOVE WS-HORARIO-F              TO HORAO

           MOVE WS-PAGA                   TO PAGO
           MOVE WS-PAGT                   TO PAG2O

           EXEC CICS SEND
               MAP ('T04MPRL')
               MAPSET('T04MPRL')
               FROM(T04MPRLO)
               ERASE FREEKB ALARM CURSOR
           END-EXEC
           .

        999-CHAMA-FASE1.
           MOVE '1'                        TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T04PPRL')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

        999-CHAMA-FASE2.
           MOVE '2'                        TO WS-FASE

           EXEC CICS RETURN
               TRANSID('FT4C')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .

        999-TRATA-FASE2.
           MOVE -1 TO PRDL
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
      *     PERFORM 999-CHAMA-CURSOR
           .

        999-ENCERRA-TRANSACAO.
           EXEC CICS SEND TEXT
               FROM (WS-MSG-ERRO)
               LENGTH(WS-LENGTH)
               ERASE FREEKB ALARM
           END-EXEC

           EXEC CICS RETURN
           END-EXEC
           .

        999-MAPFAIL.
           MOVE +80                        TO WS-LENGTH
           MOVE 'ERRO MAPA'                TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .

        999-ERROR.
           MOVE +80                        TO WS-LENGTH
           STRING 'CODIGO DO ERRO '        DELIMITED BY SIZE
                  WS-EIBRESP               DELIMITED BY SPACES
                  ' SQLCODE: '             DELIMITED BY SIZE
                  WS-SQLCODE               DELIMITED BY SIZE
                  'PRODUTO:  '             DELIMITED BY SIZE
                  WS-PRODUTO               DELIMITED BY SIZE
               INTO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
