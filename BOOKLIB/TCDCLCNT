      ******************************************************************
      * DCLGEN TABLE(CONTA_BANCARIA)                                   *
      *        LIBRARY(FS.FSYS050.DB2LIB(TCDCLCNT))                    *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(DCLCNT-)                                          *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE CONTA_BANCARIA TABLE
           ( ID_CONTA_BANCARIA              INTEGER NOT NULL,
             AGENCIA                        SMALLINT NOT NULL,
             CONTA                          INTEGER NOT NULL,
             DIGITO                         SMALLINT NOT NULL,
             SALDO                          DECIMAL(10, 2),
             CPF                            CHAR(11) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE CONTA_BANCARIA                     *
      ******************************************************************
       01  DCLCONTA-BANCARIA.
      *                       ID_CONTA_BANCARIA
           10 DCLCNT-ID-CONTA-BANCARIA
              PIC S9(9) USAGE COMP.
      *                       AGENCIA
           10 DCLCNT-AGENCIA       PIC S9(4) USAGE COMP.
      *                       CONTA
           10 DCLCNT-CONTA         PIC S9(9) USAGE COMP.
      *                       DIGITO
           10 DCLCNT-DIGITO        PIC S9(4) USAGE COMP.
      *                       SALDO
           10 DCLCNT-SALDO         PIC S9(8)V9(2) USAGE COMP-3.
      *                       CPF
           10 DCLCNT-CPF           PIC X(11).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ICONTA-BANCARIA.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 6 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
