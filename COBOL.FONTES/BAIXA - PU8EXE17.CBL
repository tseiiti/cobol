       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE17.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GRAVAR CADASTRO DE MATERIAIS DESPREZADOS E SELECIONADOS.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    SELECT CADMAT          ASSIGN TO UT-S-CADMAT
      *               FILE STATUS IS FS-CADMAT.
      *
      *    SELECT MATDES          ASSIGN TO UT-S-MATDES
      *               FILE STATUS IS FS-MATDES.
      *
      *    SELECT MATSEL          ASSIGN TO UT-S-MATSEL
      *               FILE STATUS IS FS-MATSEL.
      *
           SELECT CADMAT          ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADMAT.
      *
           SELECT MATDES          ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-MATDES.
      *
           SELECT MATSEL          ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-MATSEL.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADMAT
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADMAT
          VALUE OF FILE-ID IS "ARQUIVOS/CADMAT.EXER0017.TXT".
       01 REG-CADMAT.
          05 COD-MAT-R.
             10 COD-MAT           PIC 9(05).
          05 NOME-MAT             PIC X(20).
          05 QTD-MAT-R.
             10 QTD-MAT           PIC 9(05).
          05 VLR-UNIT-R.
             10 VLR-UNIT          PIC 9(05)V99.
          05 CONTA-MAT            PIC X(10).
          05 FILLER               PIC X(03).
      *
       FD MATDES
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-MATDES
          VALUE OF FILE-ID IS "ARQUIVOS/MATDES.TXT".
       01 REG-MATDES.
          05 COD-MAT-D            PIC 9(05).
          05 NOME-MAT-D           PIC X(20).
          05 QTD-MAT-D            PIC 9(05).
          05 VLR-UNIT-D           PIC 9(05)V99.
          05 CONTA-MAT-D          PIC X(10).
          05 FILLER               PIC X(03).
      *
       FD MATSEL
           RECORD     CONTAINS    70 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-MATSEL
          VALUE OF FILE-ID IS "ARQUIVOS/MATSEL.TXT".
       01 REG-MATSEL.
          05 COD-MAT-S            PIC 9(05).
          05 NOME-MAT-S           PIC X(20).
          05 QTD-MAT-S            PIC 9(05).
          05 VLR-UNIT-S           PIC 9(05)V99.
          05 CONTA-MAT-S          PIC X(10).
          05 VLR-TOTAL-S          PIC 9(11)V99.
          05 FILLER               PIC X(10).
      *
       WORKING-STORAGE SECTION.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-VLR-TOTAL            PIC 9(11)V99.
      *
      * FILE STATUS
      *
       77 FS-CADMAT               PIC X(02) VALUE SPACES.
       77 FS-MATDES               PIC X(02) VALUE SPACES.
       77 FS-MATSEL               PIC X(02) VALUE SPACES.
       77 FS-COD-STATUS           PIC X(02) VALUE SPACES.
       77 FS-ARQUIVO              PIC X(08) VALUE SPACES.
       77 FS-OPERACAO             PIC X(13) VALUE SPACES.
       77 FS-ABERTURA             PIC X(13) VALUE 'NA ABERTURA'.
       77 FS-LEITURA              PIC X(13) VALUE 'NA LEITURA'.
       77 FS-GRAVACAO             PIC X(13) VALUE 'NA GRAVACAO'.
       77 FS-FECHAMENTO           PIC X(13) VALUE 'NO FECHAMENTO'.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-VER-ARQ-VAZIO.
           PERFORM 003-00-TRATAR
               UNTIL FS-CADMAT EQUAL '10'.
           PERFORM 004-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADMAT
                OUTPUT MATDES
                       MATSEL.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADMAT.
           PERFORM 001-03-FS-MATDES.
           PERFORM 001-04-FS-MATSEL.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADMAT           SECTION.
      *================================================================*
           MOVE 'CADMAT'          TO FS-ARQUIVO.
           MOVE FS-CADMAT         TO FS-COD-STATUS.

           IF FS-CADMAT NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-MATDES           SECTION.
      *================================================================*
           MOVE 'MATDES'          TO FS-ARQUIVO.
           MOVE FS-MATDES         TO FS-COD-STATUS.

           IF FS-MATDES NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-MATSEL           SECTION.
      *================================================================*
           MOVE 'MATSEL'          TO FS-ARQUIVO.
           MOVE FS-MATSEL         TO FS-COD-STATUS.

           IF FS-MATSEL NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-04-FIM.                EXIT.

      *================================================================*
       002-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 002-01-LER-CADMAT.

           IF FS-CADMAT EQUAL '10'
               DISPLAY '* ARQUIVO CADMAT  VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 004-00-FECHAR-ARQUIVOS
               STOP RUN.

       002-00-FIM.                EXIT.

      *================================================================*
       002-01-LER-CADMAT          SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADMAT.

           IF FS-CADMAT NOT EQUAL '10'
               PERFORM 001-02-FS-CADMAT.

       002-01-FIM.                EXIT.

      *================================================================*
       003-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF COD-MAT-R  NOT EQUAL SPACES AND
              COD-MAT-R  IS NUMERIC       AND
              COD-MAT-R  GREATER 0        AND
              
              NOME-MAT   NOT EQUAL SPACES AND
              
              QTD-MAT-R  NOT EQUAL SPACES AND
              QTD-MAT-R  IS NUMERIC       AND
              QTD-MAT-R  GREATER 0        AND
              
              VLR-UNIT-R NOT EQUAL SPACES AND
              VLR-UNIT-R IS NUMERIC       AND 
              VLR-UNIT-R GREATER 0        AND
               
              CONTA-MAT  NOT EQUAL SPACES
               PERFORM 003-01-GRAVAR-MATSEL
           ELSE
               WRITE REG-MATDES   FROM REG-CADMAT
               PERFORM 001-03-FS-MATDES.
           
           PERFORM 002-01-LER-CADMAT.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-GRAVAR-MATSEL       SECTION.
      *================================================================*
           COMPUTE WS-VLR-TOTAL = QTD-MAT * VLR-UNIT.
           
           MOVE COD-MAT-R         TO COD-MAT-S.
           MOVE NOME-MAT          TO NOME-MAT-S.
           MOVE QTD-MAT-R         TO QTD-MAT-S.
           MOVE VLR-UNIT-R        TO VLR-UNIT-S.
           MOVE CONTA-MAT         TO CONTA-MAT-S.
           MOVE WS-VLR-TOTAL      TO VLR-TOTAL-S.
           
           WRITE REG-MATSEL.
           PERFORM 001-04-FS-MATSEL.

       003-01-FIM.                EXIT.
       
      *================================================================*
       004-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADMAT
                 MATDES
                 MATSEL.
           PERFORM 001-01-TESTAR-FS.

       004-00-FIM.                EXIT.

      *================================================================*
       900-00-ERRO                SECTION.
      *================================================================*
           DISPLAY '* ERRO ' FS-OPERACAO ' DO ARQUIVO ' FS-ARQUIVO
           DISPLAY '* FILE STATUS = ' FS-COD-STATUS
           DISPLAY '* PROGRAMA ENCERRADO'
           STOP RUN.

       900-00-FIM.                EXIT.
