       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE04.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR ARQUIVO DE PECAS SELECIONADAS 2
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
           SELECT CADPECA         ASSIGN TO UT-S-CADPECA
                      FILE STATUS IS FS-CADPECA.
      *
           SELECT CADSEL          ASSIGN TO UT-S-CADSEL
                      FILE STATUS IS FS-CADSEL.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADPECA
           RECORD     CONTAINS    80 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADPECA.
       01 REG-CADPECA.
          05 COD-PECA             PIC 9(05).
          05 NOME-PECA            PIC X(30).
          05 QTD-PECA             PIC 9(05).
          05 QTD-MINIMA           PIC 9(05).
          05 QTD-MAXIMA           PIC 9(05).
          05 FORNECEDOR           PIC X(05).
          05 PR-UNITARIO          PIC 9(07)V99.
          05 PERDA                PIC 9(07)V99.
          05 TIPO                 PIC X(01).
          05 FILLER               PIC X(06).
      *
       FD CADSEL
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADSEL.
       01 REG-CADSEL.
          05 COD-SEL              PIC 9(05).
          05 NOME-SEL             PIC X(30).
          05 QTD-SEL              PIC 9(05).
          05 PUNIT-SEL            PIC 9(07)V99.
          05 FILLER               PIC X(01).
      *
       WORKING-STORAGE SECTION.
      *
      * FILE STATUS
      *
       77 FS-CADPECA              PIC X(02) VALUE SPACES.
       77 FS-CADSEL               PIC X(02) VALUE SPACES.
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
               UNTIL FS-CADPECA EQUAL '10'.
           PERFORM 004-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADPECA
                OUTPUT CADSEL.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADPECA.
           PERFORM 001-03-FS-CADSEL.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADPECA          SECTION.
      *================================================================*
           MOVE 'CADPECA'         TO FS-ARQUIVO.
           MOVE FS-CADPECA        TO FS-COD-STATUS.

           IF FS-CADPECA NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-CADSEL           SECTION.
      *================================================================*
           MOVE 'CADSEL'          TO FS-ARQUIVO.
           MOVE FS-CADSEL         TO FS-COD-STATUS.

           IF FS-CADSEL NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       002-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 002-01-LER-CADPECA.

           IF FS-CADPECA EQUAL '10'
               DISPLAY '* ARQUIVO CADPECA VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO    *'
               PERFORM 004-00-FECHAR-ARQUIVOS
               STOP RUN.

       002-00-FIM.                EXIT.

      *================================================================*
       002-01-LER-CADPECA         SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADPECA.

           IF FS-CADPECA NOT EQUAL '10'
               PERFORM 001-02-FS-CADPECA.

       002-01-FIM.                EXIT.

      *================================================================*
       003-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF COD-PECA LESS 10001 AND QTD-PECA GREATER 0
               MOVE COD-PECA      TO COD-SEL
               MOVE NOME-PECA     TO NOME-SEL
               MOVE QTD-PECA      TO QTD-SEL
               MOVE PR-UNITARIO   TO PUNIT-SEL
               WRITE REG-CADSEL
               PERFORM 001-03-FS-CADSEL.

           PERFORM 002-01-LER-CADPECA.

       003-00-FIM.                EXIT.

      *================================================================*
       004-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADPECA
                 CADSEL.
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