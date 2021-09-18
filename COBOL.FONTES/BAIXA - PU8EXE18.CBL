       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE18.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GRAVAR CADASTRO DE MATERIAIS NOVOS E ATUALIZADOS.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    SELECT CADMATAN        ASSIGN TO UT-S-CADMATAN
      *               FILE STATUS IS FS-CADMATAN.
      *
      *    SELECT CADMATNO        ASSIGN TO UT-S-CADMATNO
      *               FILE STATUS IS FS-CADMATNO.
      *
      *    SELECT CADMATAT        ASSIGN TO UT-S-CADMATAT
      *               FILE STATUS IS FS-CADMATAT.
      *
           SELECT CADMATAN        ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADMATAN.
      *
           SELECT CADMATNO        ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADMATNO.
      *
           SELECT CADMATAT        ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADMATAT.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADMATAN
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADMATAN
          VALUE OF FILE-ID IS "ARQUIVOS/CADMATAN.TXT".
       01 REG-CADMATAN.
          05 COD-MATANT           PIC 9(05).
          05 NOME-MATANT          PIC X(20).
          05 QTD-MATANT           PIC 9(05).
          05 VLR-UNITANT          PIC 9(05)V99.
          05 CONTA-MATANT         PIC X(10).
          05 FILLER               PIC X(03).
      *
       FD CADMATNO
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADMATNO
          VALUE OF FILE-ID IS "ARQUIVOS/CADMATNO.TXT".
       01 REG-CADMATNO.
          05 COD-MATNOV           PIC 9(05).
          05 NOME-MATNOV          PIC X(20).
          05 QTD-MATNOV           PIC 9(05).
          05 VLR-UNITNOV          PIC 9(05)V99.
          05 CONTA-MATNOV         PIC X(10).
          05 FILLER               PIC X(03).
      *
       FD CADMATAT
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADMATAT
          VALUE OF FILE-ID IS "ARQUIVOS/CADMATAT.TXT".
       01 REG-CADMATAT.
          05 COD-MATATU           PIC 9(05).
          05 NOME-MATATU          PIC X(20).
          05 QTD-MATATU           PIC 9(05).
          05 VLR-UNITATU          PIC 9(05)V99.
          05 CONTA-MATATU         PIC X(10).
          05 FILLER               PIC X(03).
      *
       WORKING-STORAGE SECTION.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-COD-MAT              PIC 9(05) VALUE ZEROS.
      *
      * FILE STATUS
      *
       77 FS-CADMATAN             PIC X(02) VALUE SPACES.
       77 FS-CADMATNO             PIC X(02) VALUE SPACES.
       77 FS-CADMATAT             PIC X(02) VALUE SPACES.
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
               UNTIL FS-CADMATAN EQUAL '10'.
           PERFORM 004-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADMATAN
                       CADMATNO
                OUTPUT CADMATAT.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADMATAN.
           PERFORM 001-03-FS-CADMATNO.
           PERFORM 001-04-FS-CADMATAT.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADMATAN         SECTION.
      *================================================================*
           MOVE 'CADMATAN'        TO FS-ARQUIVO.
           MOVE FS-CADMATAN       TO FS-COD-STATUS.

           IF FS-CADMATAN NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-CADMATNO         SECTION.
      *================================================================*
           MOVE 'CADMATNO'        TO FS-ARQUIVO.
           MOVE FS-CADMATNO       TO FS-COD-STATUS.

           IF FS-CADMATNO NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-CADMATAT         SECTION.
      *================================================================*
           MOVE 'CADMATAT'        TO FS-ARQUIVO.
           MOVE FS-CADMATAT       TO FS-COD-STATUS.

           IF FS-CADMATAT NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-04-FIM.                EXIT.

      *================================================================*
       002-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 002-01-LER-CADMATAN.

           IF FS-CADMATAN EQUAL '10'
               DISPLAY '* ARQUIVO CADMATAN VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 004-00-FECHAR-ARQUIVOS
               STOP RUN.
                                                                        
           PERFORM 002-02-LER-CADMATNO.

           IF FS-CADMATNO EQUAL '10'
               DISPLAY '* ARQUIVO CADMATNO VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 004-00-FECHAR-ARQUIVOS
               STOP RUN.

       002-00-FIM.                EXIT.

      *================================================================*
       002-01-LER-CADMATAN        SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADMATAN.

           IF FS-CADMATAN NOT EQUAL '10'
               PERFORM 001-02-FS-CADMATAN.

       002-01-FIM.                EXIT.

      *================================================================*
       002-02-LER-CADMATNO        SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADMATNO.

           IF FS-CADMATNO NOT EQUAL '10'
               PERFORM 001-03-FS-CADMATNO.

       002-02-FIM.                EXIT.

      *================================================================*
       003-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF COD-MATANT LESS COD-MATNOV
               IF COD-MATANT NOT EQUAL WS-COD-MAT
                   WRITE REG-CADMATAT FROM REG-CADMATAN
                   MOVE COD-MATANT TO WS-COD-MAT
               END-IF
           ELSE
               IF COD-MATNOV NOT EQUAL WS-COD-MAT                                    
                   WRITE REG-CADMATAT FROM REG-CADMATNO
                   MOVE COD-MATNOV TO WS-COD-MAT.
               
           PERFORM 001-04-FS-CADMATAT.
           PERFORM 002-01-LER-CADMATAN.
           PERFORM 002-02-LER-CADMATNO.

       003-00-FIM.                EXIT.
       
      *================================================================*
       004-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADMATAN
                 CADMATNO
                 CADMATAT.
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
