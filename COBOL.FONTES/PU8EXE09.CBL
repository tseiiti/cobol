       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE09.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GRAVAR ARQUIVO DE COTACOES SELECIONADAS E DESPREZADAS.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
           SELECT CADCOTA         ASSIGN TO UT-S-CADCOTA
                      FILE STATUS IS FS-CADCOTA.
      *
           SELECT COTASELE        ASSIGN TO UT-S-COTASELE
                      FILE STATUS IS FS-COTASELE.
      *
           SELECT COTADESP        ASSIGN TO UT-S-COTADESP
                      FILE STATUS IS FS-COTADESP.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADCOTA
           RECORD     CONTAINS    80 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADCOTA.
       01 REG-CADCOTA.
          05 CODCOTA              PIC 9(05).
          05 QTDCOTA              PIC 9(05).
          05 DATACOTA             PIC 9(06).
          05 FORN1COTA            PIC 9(05).
          05 PREC1COTA            PIC 9(13)V99.
          05 FORN2COTA            PIC 9(05).
          05 PREC2COTA            PIC 9(13)V99.
          05 FORN3COTA            PIC 9(05).
          05 PREC3COTA            PIC 9(13)V99.
          05 FILLER               PIC X(04).
      *
       FD COTASELE
           RECORD     CONTAINS    40 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-COTASELE.
       01 REG-COTASELE.
          05 CODSEL               PIC 9(05).
          05 QTDSEL               PIC 9(05).
          05 DATASEL              PIC 9(06).
          05 FORNSEL              PIC 9(05).
          05 PRECSEL              PIC 9(13)V99.
          05 FILLER               PIC X(04).
      *
       FD COTADESP
           RECORD     CONTAINS    40 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-COTADESP.
       01 REG-COTADESP.
          05 CODDESP              PIC 9(05).
          05 QTDDESP              PIC 9(05).
          05 DATADESP             PIC 9(06).
          05 FORNDESP             PIC 9(05).
          05 PRECDESP             PIC 9(13)V99.
          05 FILLER               PIC X(04).
      *
       WORKING-STORAGE SECTION.
      *
      * FILE STATUS
      *
       77 FS-CADCOTA              PIC X(02) VALUE SPACES.
       77 FS-COTASELE             PIC X(02) VALUE SPACES.
       77 FS-COTADESP             PIC X(02) VALUE SPACES.
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
               UNTIL FS-CADCOTA EQUAL '10'.
           PERFORM 004-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADCOTA
                OUTPUT COTASELE
                OUTPUT COTADESP.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADCOTA.
           PERFORM 001-03-FS-COTASELE.
           PERFORM 001-04-FS-COTADESP.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADCOTA          SECTION.
      *================================================================*
           MOVE 'CADCOTA'         TO FS-ARQUIVO.
           MOVE FS-CADCOTA        TO FS-COD-STATUS.

           IF FS-CADCOTA NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-COTASELE              SECTION.
      *================================================================*
           MOVE 'COTASELE'        TO FS-ARQUIVO.
           MOVE FS-COTASELE       TO FS-COD-STATUS.

           IF FS-COTASELE NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-COTADESP         SECTION.
      *================================================================*
           MOVE 'COTADESP'        TO FS-ARQUIVO.
           MOVE FS-COTADESP       TO FS-COD-STATUS.

           IF FS-COTADESP NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       002-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 002-01-LER-CADCOTA.

           IF FS-CADCOTA EQUAL '10'
               DISPLAY '* ARQUIVO CADCOTA VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO    *'
               PERFORM 004-00-FECHAR-ARQUIVOS
               STOP RUN.

       002-00-FIM.                EXIT.

      *================================================================*
       002-01-LER-CADCOTA         SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADCOTA.

           IF FS-CADCOTA NOT EQUAL '10'
               PERFORM 001-02-FS-CADCOTA.

       002-01-FIM.                EXIT.

      *================================================================*
       003-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF PREC1COTA LESS PREC2COTA AND PREC3COTA
               PERFORM 003-01-GRAVA-FORN1SELE 
               PERFORM 003-05-GRAVA-FORN2DESP
               PERFORM 003-06-GRAVA-FORN3DESP
           ELSE IF PREC2COTA LESS PREC3COTA
               PERFORM 003-02-GRAVA-FORN2SELE
               PERFORM 003-04-GRAVA-FORN1DESP
               PERFORM 003-06-GRAVA-FORN3DESP
           ELSE
               PERFORM 003-03-GRAVA-FORN3SELE
               PERFORM 003-04-GRAVA-FORN1DESP
               PERFORM 003-05-GRAVA-FORN2DESP.

           PERFORM 002-01-LER-CADCOTA.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-GRAVA-FORN1SELE     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODSEL.
           MOVE QTDCOTA           TO QTDSEL.
           MOVE DATACOTA          TO DATASEL.
           MOVE FORN1COTA         TO FORNSEL.
           MOVE PREC1COTA         TO PRECSEL.
           
           WRITE REG-COTASELE.
           PERFORM 001-03-FS-COTASELE.

       003-01-FIM.                EXIT.

      *================================================================*
       003-02-GRAVA-FORN2SELE     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODSEL.
           MOVE QTDCOTA           TO QTDSEL.
           MOVE DATACOTA          TO DATASEL.
           MOVE FORN2COTA         TO FORNSEL.
           MOVE PREC2COTA         TO PRECSEL.
           WRITE REG-COTASELE.
           PERFORM 001-03-FS-COTASELE.

       003-02-FIM.                EXIT.

      *================================================================*
       003-03-GRAVA-FORN3SELE     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODSEL.
           MOVE QTDCOTA           TO QTDSEL.
           MOVE DATACOTA          TO DATASEL.
           MOVE FORN3COTA         TO FORNSEL.
           MOVE PREC3COTA         TO PRECSEL.
           WRITE REG-COTASELE.
           PERFORM 001-03-FS-COTASELE.

       003-03-FIM.                EXIT.

      *================================================================*
       003-04-GRAVA-FORN1DESP     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODDESP.
           MOVE QTDCOTA           TO QTDDESP.
           MOVE DATACOTA          TO DATADESP.
           MOVE FORN1COTA         TO FORNDESP.
           MOVE PREC1COTA         TO PRECDESP.
           WRITE REG-COTADESP.
           PERFORM 001-04-FS-COTADESP.

       003-04-FIM.                EXIT.

      *================================================================*
       003-05-GRAVA-FORN2DESP     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODDESP.
           MOVE QTDCOTA           TO QTDDESP.
           MOVE DATACOTA          TO DATADESP.
           MOVE FORN2COTA         TO FORNDESP.
           MOVE PREC2COTA         TO PRECDESP.
           WRITE REG-COTADESP.
           PERFORM 001-04-FS-COTADESP.

       003-05-FIM.                EXIT.

      *================================================================*
       003-06-GRAVA-FORN3DESP     SECTION.
      *================================================================*
           MOVE CODCOTA           TO CODDESP.
           MOVE QTDCOTA           TO QTDDESP.
           MOVE DATACOTA          TO DATADESP.
           MOVE FORN3COTA         TO FORNDESP.
           MOVE PREC3COTA         TO PRECDESP.
           WRITE REG-COTADESP.
           PERFORM 001-04-FS-COTADESP.

       003-06-FIM.                EXIT.

      *================================================================*
       004-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO TO FS-OPERACAO.
           CLOSE CADCOTA
                 COTASELE
                 COTADESP.
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