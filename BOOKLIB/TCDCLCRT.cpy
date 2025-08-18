      ******************************************************************
      * DCLGEN TABLE(CARTOES)                                          *
      *        LIBRARY(FS.FSYS050.DB2LIB(TCDCLCRT))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DCLCRT-)                                          *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CARTOES TABLE
           ( NUMERO_CARTAO                  DECIMAL(16, 0) NOT NULL,
             NOME_IMPRESSO                  CHAR(30) NOT NULL,
             CVV                            SMALLINT NOT NULL,
             DATA_EXPIRACAO                 CHAR(7) NOT NULL,
             LIMITE_APROVADO                DECIMAL(8, 2) NOT NULL,
             LIMITE_DISPONIVEL              DECIMAL(8, 2) NOT NULL,
             TIPO                           CHAR(1) NOT NULL,
             ID_CONTA_BANCARIA              INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CARTOES                            *
      ******************************************************************
       01  DCLCARTOES.
      *                       NUMERO_CARTAO
           10 DCLCRT-NUMERO-CARTAO
              PIC S9(16)V USAGE COMP-3.
      *                       NOME_IMPRESSO
           10 DCLCRT-NOME-IMPRESSO
              PIC X(30).
      *                       CVV
           10 DCLCRT-CVV           PIC S9(4) USAGE COMP.
      *                       DATA_EXPIRACAO
           10 DCLCRT-DATA-EXPIRACAO
              PIC X(7).
      *                       LIMITE_APROVADO
           10 DCLCRT-LIMITE-APROVADO
              PIC S9(6)V9(2) USAGE COMP-3.
      *                       LIMITE_DISPONIVEL
           10 DCLCRT-LIMITE-DISPONIVEL
              PIC S9(6)V9(2) USAGE COMP-3.
      *                       TIPO
           10 DCLCRT-TIPO          PIC X(1).
      *                       ID_CONTA_BANCARIA
           10 DCLCRT-ID-CONTA-BANCARIA
              PIC S9(9) USAGE COMP.
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ICARTOES.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 8 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
