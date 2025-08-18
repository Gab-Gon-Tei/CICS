      *--------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *--------------------------------------------------------------*
       PROGRAM-ID.                         T50PENT.
       AUTHOR.                             LUIS GUSTAVO E LUCAS
      *--------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *--------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *--------------------------------------------------------------*
       DATA                                DIVISION.
      *--------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
       77  WS-MSG-ERRO                     PIC X(80).
       77  WS-LENGTH                       PIC S9(04) COMP.
       01  WS-DFHCOMMAREA.
           05 WS-FASE                      PIC X(01).
           05 WS-ID-CPF                    PIC X(11).
           05 WS-ID-PROD                   PIC 9(09).
           05 WS-CONT-CARR                 PIC 9(09).
           05 WS-ID-PEDIDO                 PIC 9(09).   
           05 WS-CONTA-BANCARIA            PIC 9(09).   
           05 WS-EMAIL-COMMAREA            PIC X(40).  

       01  WS-DFHCOMMAREA-AUX.
           05 WS-FASE-AUX                  PIC X(01).
           05 WS-ID-CPF-AUX                PIC X(11).
           05 WS-ID-PROD-AUX               PIC 9(09).
           05 WS-CONT-CARR-AUX             PIC 9(09).
           05 WS-ID-PEDIDO-AUX             PIC 9(09).   
           05 WS-CONTA-BANCARIA-AUX        PIC 9(09).          

      *
       01  WS-VAR-TEMPO.
           05 WS-DATA                      PIC X(10).
           05 WS-HORARIO                   PIC X(08).
      *77  WS-ENDERECO                     PIC X(40).
       77  WS-VALIDA                       PIC X(01) VALUE '0'.
       77  WS-EMAIL                        PIC X(40).
       77  WS-TEXTO-LIMPO                  PIC X(30) VALUE SPACES.
       77  WS-INICIO                       PIC 9(02) VALUE 0.
       77  WS-FIM                          PIC 9(02) VALUE 0.
       77  WS-TAMANHO                      PIC 9(02) VALUE 0.
       77  WS-EMAIL-AUX                 PIC X(40).
       77  WS-OLD-EMAIL-TRIMMED          PIC X(40).
      *
           COPY T50MENT.
           COPY DFHAID.
           COPY DFHBMSCA.
           EXEC SQL
              INCLUDE TCDCLPDD
           END-EXEC.
           EXEC SQL
              INCLUDE TCDCLCLI
           END-EXEC.

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      *--------------------------------------------------------------*
       LINKAGE                             SECTION.
      *--------------------------------------------------------------*
       01  DFHCOMMAREA.
           05 OCCURS 0 TO 24576 TIMES DEPENDING ON EIBCALEN
                                           PIC X(01).
      *--------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *--------------------------------------------------------------*
           EXEC CICS HANDLE CONDITION
              MAPFAIL(999-ERRO-MAPA)
              ERROR(999-ERRO-GENERICO)
           END-EXEC
      *
      *  SELETOR DE FASE - O MENU PRINCIPAL POSSUI 2 FASES
      *    FASE 1 - ENVIA O MAPA PARA O TERMINALO
      *    FASE 2 - TRATA O CAMPO T1OPC
      *
           MOVE DFHCOMMAREA                TO WS-DFHCOMMAREA
           IF EIBCALEN EQUAL 0
              MOVE '1'                     TO WS-FASE
           END-IF
      *
           EVALUATE WS-FASE
               WHEN '1' PERFORM 100-FASE1
               WHEN '2' PERFORM 200-FASE2
               WHEN OTHER
                  MOVE +80                 TO WS-LENGTH
                  MOVE 'ERRO NO NUMERO FASE'
                                           TO WS-MSG-ERRO
                  PERFORM 999-ENCERRA-TRANSACAO
           END-EVALUATE
           .
      *--------------------------------------------------------------*
       100-FASE1.
      * LIMPAR AS VARIAVEIS DO MAPA
           MOVE LOW-VALUES                 TO MAPAENTO
      * POSICIONAMNETO ENTICO - DIRETO NO CAMPO DESEJADO
           MOVE -1                         TO ENDENTL
      * MOVER A MENSAGEM PARA O USUARIO SELECIONAR A OPCAO
           MOVE 'DIGITE SEU ENDERECO E CONFIRME SEU EMAIL'
                                           TO MSGENTO
      * DESPROTEGER O CAMPO OPCAO PARA O USUARIO PODER DIGITAR
           MOVE DFHUNIMD                   TO ENDENTA
           MOVE DFHUNIMD                   TO CONENTA
      * MANDAR A TELA PARA O TERMINAL
      *    PERFORM 999-MANDA-TELA
      * ENCERRA A TRANSACAO CHAMANDO A PROXIMA FASE
      *    MOVE 'LUISLUCAS@FOURSYS.COM'    TO DCLCLI-EMAIL
      *    EXEC SQL
      *         SELECT  EMAIL
      *         INTO  :WS-EMAIL
      *         FROM CLIENTES
      *         WHERE EMAIL = :DCLCLI-EMAIL
      *    END-EXEC
           EXEC SQL
                SELECT  CPF, EMAIL
                INTO :DCLCLI-CPF, :WS-EMAIL
                FROM CLIENTES
                WHERE CPF = :WS-ID-CPF
           END-EXEC
           EVALUATE SQLCODE
              WHEN 0
                 MOVE WS-EMAIL            TO CONENTO
                 MOVE WS-EMAIL            TO WS-EMAIL-COMMAREA
      *          MOVE WS-EMAIL            TO DCLCLI-EMAIL
                 PERFORM 999-TRATA-FASE2
              WHEN +100
                 MOVE 'EMAIL NAO ENCONTADO(REALIZE UM CADASTRO)'
                                           TO MSGENTO
                 MOVE '1'                  TO WS-VALIDA
              WHEN OTHER
                 PERFORM 999-ERRO-GENERICO
           END-EVALUATE
           .
      *--------------------------------------------------------------*
       200-FASE2.
           EXEC CICS HANDLE AID
              ENTER  (210-ENTER)
      *       PF3    (220-PF3)
              PF2    (250-PF2)
      *        PF1    (250-PF1)
              PF10   (250-PF10)
      *       PF11   (230-PF11)
              PF12   (290-PF12)
              CLEAR  (230-PF5)
              ANYKEY (240-ANYKEY)
           END-EXEC
      *
           EXEC CICS RECEIVE
              MAP   ('MAPAENT')
              MAPSET('T50MENT')
              INTO  (MAPAENTI)
           END-EXEC
           .
       210-ENTER.
           PERFORM 250-PF1
           MOVE WS-DFHCOMMAREA             TO WS-DFHCOMMAREA-AUX
      *    MOVE +80                        TO WS-LENGHT
           EXEC CICS XCTL
              PROGRAM('T50PPAG')
              COMMAREA(WS-DFHCOMMAREA-AUX)
              LENGTH(LENGTH OF WS-DFHCOMMAREA-AUX)
           END-EXEC
           .
       250-PF1.
           IF WS-VALIDA = 1
              MOVE DFHBMPRF                  TO ENDENTA
              MOVE DFHBMPRF                  TO CONENTA
              MOVE 'EMAIL NAO ENCONTADO(REALIZE UM CADASTRO)'
                                             TO MSGENTO
              PERFORM 999-TRATA-FASE2
           END-IF
           IF CONENTI NOT = WS-EMAIL-COMMAREA
              AND CONENTL NOT = 0  AND CONENTI NOT EQUAL SPACES
              MOVE CONENTI                       TO WS-EMAIL-AUX
              INSPECT WS-EMAIL-AUX
                      TALLYING WS-INICIO
              FOR LEADING SPACES

              COMPUTE WS-INICIO = WS-INICIO + 1

              INSPECT FUNCTION REVERSE(WS-EMAIL-AUX)
                    TALLYING WS-FIM
              FOR LEADING SPACES

              COMPUTE WS-FIM = LENGTH OF WS-EMAIL-AUX - WS-FIM
              COMPUTE WS-TAMANHO = WS-FIM - WS-INICIO + 1
              MOVE WS-EMAIL-AUX(WS-INICIO:WS-TAMANHO)
                                           TO WS-TEXTO-LIMPO
              EXEC SQL
                 UPDATE CLIENTES
                 SET   EMAIL       = :WS-TEXTO-LIMPO
                 WHERE EMAIL = :WS-EMAIL-COMMAREA
              END-EXEC
              IF SQLCODE = 0
                 MOVE 'EMAIL ATUALIZADO COM SUCESSO'
                                            TO MSGENTO
              ELSE
                 MOVE 'EMAIL DIGITADO INVALIDO'
                                            TO MSGENTO
              END-IF
           END-IF
           IF ENDENTL NOT = 0 AND ENDENTI NOT = SPACES
              MOVE WS-ID-PEDIDO            TO DCLPDD-ID-PEDIDO
              MOVE ENDENTI                 TO DCLPDD-ENDERECO
              EXEC SQL
                 UPDATE PEDIDOS
                 SET   ENDERECO    = :DCLPDD-ENDERECO
                 WHERE ID_PEDIDO = :DCLPDD-ID-PEDIDO
              END-EXEC
              IF SQLCODE = 0
                 MOVE 'ENDERECO CADASTRADO COM SUCESSO'
                                            TO MSGENTO
              ELSE
                 MOVE 'ERRO AO CADASTRAR ENDERECO'
                                            TO MSGENTO
              END-IF
           END-IF
      *     PERFORM 999-TRATA-FASE2
           .
      *--------------------------------------------------------------*
       250-PF10.
           MOVE 1                          TO WS-FASE
      *    MOVE +80                        TO WS-LENGHT
           EXEC CICS XCTL
              PROGRAM('T50PCAR')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
      *--------------------------------------------------------------*
       230-PF5.
           PERFORM 999-CHAMA-FASE1
           .
       240-ANYKEY.
           MOVE 'TECLA PRESSIONADA INVALIDA'
                                           TO MSGENTO
           PERFORM 999-TRATA-FASE2

           .
       250-PF2.
           MOVE 1                          TO WS-FASE
      *    MOVE +80                        TO WS-LENGHT
           EXEC CICS XCTL
              PROGRAM('T50PPRL')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       290-PF12.
           MOVE '1'                        TO WS-FASE

           EXEC CICS XCTL
               PROGRAM('T50PLOG')
               COMMAREA(WS-DFHCOMMAREA)
               LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       999-ENCERRA-TRANSACAO.
           MOVE +80                        TO WS-LENGTH
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
           MOVE EIBTRMID                   TO TERMENTO
           MOVE EIBTRNID                   TO TRANSENTO
           MOVE EIBTASKN                   TO TASKENTO
           MOVE WS-FASE                    TO FASEENTO
      *
      *    EXEC CICS ASSIGN
      *       USERID(T1USRO)
      *    END-EXEC
      *
           EXEC CICS LINK
              PROGRAM('AUXCICS1')
              COMMAREA(WS-VAR-TEMPO)
              LENGTH(+18)
           END-EXEC
      *
      *    MOVE WS-DATA                    TO ENTDATO
      *    MOVE WS-HORARIO                 TO ENTHORO
           MOVE WS-DATA(01:02)              TO DIADATO
           MOVE WS-DATA(04:02)              TO MESDATO
           MOVE WS-DATA(07:04)              TO ANODATO
           MOVE WS-HORARIO(01:02)           TO HORADATO
           MOVE WS-HORARIO(04:02)           TO MINDATO
      *
           EXEC CICS SEND
              MAP   ('MAPAENT')
              MAPSET('T50MENT')
              FROM  (MAPAENTO)
              ERASE FREEKB ALARM CURSOR
           END-EXEC
           .
       999-CHAMA-FASE1.
           MOVE '1'                        TO WS-FASE
      *
           EXEC CICS XCTL
              PROGRAM('T50PENT')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH (LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       999-CHAMA-FASE2.
           MOVE '2'                        TO WS-FASE
      *
           EXEC CICS RETURN
              TRANSID('FTMF')
              COMMAREA(WS-DFHCOMMAREA)
              LENGTH(LENGTH OF WS-DFHCOMMAREA)
           END-EXEC
           .
       999-TRATA-FASE2.
           MOVE DFHUNIMD                   TO ENDENTA
           MOVE DFHUNIMD                   TO CONENTA
           MOVE -1                         TO ENDENTL
           PERFORM 999-MANDA-TELA
           PERFORM 999-CHAMA-FASE2
           .
       999-ERRO-MAPA.
           MOVE 'ERRO MAPA T50MENT'        TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
       999-ERRO-GENERICO.
           MOVE 'ERRO GENERICO T50PENT'   TO WS-MSG-ERRO
           PERFORM 999-ENCERRA-TRANSACAO
           .
