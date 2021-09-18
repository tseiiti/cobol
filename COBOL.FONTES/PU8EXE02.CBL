       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE02.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR ARQUIVO DE PECAS ATUALIZADAS 2
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
           SELECT CADATU          ASSIGN TO UT-S-CADATU
                      FILE STATUS IS FS-CADATU.
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
       FD CADATU                       
           RECORD     CONTAINS    60 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADATU.
       01 REG-CADATU.
          05 COD-ATU              PIC 9(05).
          05 NOME-ATU             PIC X(30).
          05 QTD-ATU              PIC 9(05).
          05 PR-UNIT-ATU          PIC 9(07)V99.
          05 ASTER                PIC X(03).
          05 FILLER               PIC X(08).
      *
       WORKING-STORAGE SECTION.
      *
      * FILE STATUS
      *
       77 FS-CADPECA              PIC X(02) VALUE SPACES.
       77 FS-CADATU               PIC X(02) VALUE SPACES.
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
                OUTPUT CADATU.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADPECA.
           PERFORM 001-03-FS-CADATU.

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
       001-03-FS-CADATU           SECTION.
      *================================================================*
           MOVE 'CADATU'          TO FS-ARQUIVO.
           MOVE FS-CADATU         TO FS-COD-STATUS.

           IF FS-CADATU NOT EQUAL '00' AND '10'
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

           MOVE COD-PECA          TO COD-ATU.
           MOVE NOME-PECA         TO NOME-ATU.
           MOVE QTD-PECA          TO QTD-ATU.
           MOVE PR-UNITARIO       TO PR-UNIT-ATU.
           MOVE '***'             TO ASTER.
           WRITE REG-CADATU.
           PERFORM 001-03-FS-CADATU.
           
           PERFORM 002-01-LER-CADPECA.

       003-00-FIM.                EXIT.

      *================================================================*
       004-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADPECA
                 CADATU.
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