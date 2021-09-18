       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE06.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR RELATORIO ANALITICO DO CADASTRO DE PECAS.
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
           SELECT RELATO          ASSIGN TO UR-S-RELATO
                      FILE STATUS IS FS-RELATO.
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
       FD RELATO
           RECORD     CONTAINS    104 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS OMITTED
           DATA       RECORD      IS REG-RELATO.
       01 REG-RELATO.
          05 FILLER               PIC X(104).
      *
       WORKING-STORAGE SECTION.
      *
      *  AREA PARA OBTER DATA E HORA
      *
       01 WS-DATA-HORARIO-SYS.
          05 WS-DATA-SYS.
             10 WS-ANO-SYS        PIC 9(04).
             10 WS-MES-SYS        PIC 9(02).
             10 WS-DIA-SYS        PIC 9(02).
          05 WS-HORARIO-SYS.
             10 WS-HOR-SYS        PIC 9(02).
             10 WS-MIN-SYS        PIC 9(02).
          05 FILLER               PIC X(09).
      *
      *  ACUMULADORES
      *
       77 ACUM-LINHAS             PIC 9(02) VALUE 60.
       77 ACUM-PAG                PIC 9(04) VALUE ZEROS.
       77 ACUM-LIDOS              PIC 9(06) VALUE ZEROS.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-TOTAL                PIC 9(09)V99 VALUE ZEROS.
      *
      * FILE STATUS
      *
       77 FS-CADPECA              PIC X(02) VALUE SPACES.
       77 FS-RELATO               PIC X(02) VALUE SPACES.
       77 FS-COD-STATUS           PIC X(02) VALUE SPACES.
       77 FS-ARQUIVO              PIC X(08) VALUE SPACES.
       77 FS-OPERACAO             PIC X(13) VALUE SPACES.
       77 FS-ABERTURA             PIC X(13) VALUE 'NA ABERTURA'.
       77 FS-LEITURA              PIC X(13) VALUE 'NA LEITURA'.
       77 FS-GRAVACAO             PIC X(13) VALUE 'NA GRAVACAO'.
       77 FS-FECHAMENTO           PIC X(13) VALUE 'NO FECHAMENTO'.
      *
      *  LINHAS DO RELATORIO
      *
       01  CAB001.
          05 FILLER               PIC X(93) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(82) VALUE
             'PROGRAMA = EXER0006'.
          05 CAB002-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB002-MIN           PIC 9(02).
          05 FILLER               PIC X(07) VALUE SPACES.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(32) VALUE SPACES.
          05 FILLER               PIC X(72) VALUE
             'RELATORIO ANALITICO DO CADASTRO DE PECAS'.
      *
       01  CAB004.
          05 FILLER               PIC X(62) VALUE SPACES.
          05 FILLER               PIC X(30) VALUE 'VALOR'.
          05 FILLER               PIC X(12) VALUE 'PERDA'.
      *
       01  CAB005.
          05 FILLER               PIC X(14) VALUE 'CODIGO'.
          05 FILLER               PIC X(27) VALUE 'NOME DA PECA'.
          05 FILLER               PIC X(07) VALUE 'QTDE'.
          05 FILLER               PIC X(11) VALUE 'FORN.'.
          05 FILLER               PIC X(13) VALUE 'UNITARIO'.
          05 FILLER               PIC X(17) VALUE 'VALOR TOTAL'.
          05 FILLER               PIC X(11) VALUE 'PREVISTA'.
          05 FILLER               PIC X(04) VALUE 'TIPO'.
      *
       01  DET001.
          05 DET001-COD-PECA      PIC 99.999.
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-NOME-PECA     PIC X(30).
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-QTD-PECA      PIC ZZ.ZZ9.
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-FORNECEDOR    PIC 99999.
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-PR-UNITARIO   PIC Z.ZZZ.ZZ9,99.
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-VLR-TOTAL     PIC ZZZ.ZZZ.ZZ9,99.
          05 FILLER               PIC X(02) VALUE SPACES.
          05 DET001-PERDA         PIC Z.ZZZ.ZZ9,99.
          05 FILLER               PIC X(04) VALUE SPACES.
          05 DET001-TIPO          PIC X(01).
      *
       01  TOT001.
          05 FILLER               PIC X(27) VALUE
             'TOTAL DE REGISTROS LIDOS = '.
          05 TOT001-LIDOS         PIC ZZZ.ZZ9.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR
               UNTIL FS-CADPECA  EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADPECA
                OUTPUT RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADPECA.
           PERFORM 001-03-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADPECA          SECTION.
      *================================================================*
           MOVE 'CADPECA '        TO FS-ARQUIVO.
           MOVE FS-CADPECA        TO FS-COD-STATUS.

           IF FS-CADPECA NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-RELATO           SECTION.
      *================================================================*
           MOVE 'RELATO'          TO FS-ARQUIVO.
           MOVE FS-RELATO         TO FS-COD-STATUS.

           IF FS-RELATO NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       002-00-OBTER-DATA-HORA     SECTION.
      *================================================================*
           MOVE FUNCTION CURRENT-DATE TO WS-DATA-HORARIO-SYS.
           MOVE WS-ANO-SYS        TO CAB002-ANO.
           MOVE WS-MES-SYS        TO CAB002-MES.
           MOVE WS-DIA-SYS        TO CAB002-DIA.
           MOVE WS-HOR-SYS        TO CAB002-HOR.
           MOVE WS-MIN-SYS        TO CAB002-MIN.

       002-00-FIM.                EXIT.

      *================================================================*
       003-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 003-01-LER-CADPECA.

           IF FS-CADPECA EQUAL '10'
               DISPLAY '* ARQUIVO CADPECA VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CADPECA         SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADPECA.

           IF FS-CADPECA NOT EQUAL '10'
               PERFORM 001-02-FS-CADPECA.

       003-01-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           PERFORM 004-01-IMPRESSAO.

           ADD 1                  TO ACUM-LINHAS
                                     ACUM-LIDOS.
           
           PERFORM 003-01-LER-CADPECA.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.
               
           MULTIPLY QTD-PECA BY PR-UNITARIO GIVING WS-TOTAL.

           MOVE COD-PECA          TO DET001-COD-PECA.
           MOVE NOME-PECA         TO DET001-NOME-PECA.
           MOVE QTD-PECA          TO DET001-QTD-PECA.
           MOVE FORNECEDOR        TO DET001-FORNECEDOR.
           MOVE PR-UNITARIO       TO DET001-PR-UNITARIO.
           MOVE WS-TOTAL          TO DET001-VLR-TOTAL.
           MOVE PERDA             TO DET001-PERDA.
           MOVE TIPO              TO DET001-TIPO.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-03-FS-RELATO.
           
       004-01-FIM.                EXIT.

      *================================================================*
       004-02-CABECALHOS          SECTION.
      *================================================================*
           ADD 1                  TO ACUM-PAG.
           MOVE ACUM-PAG          TO CAB001-PAG.

           WRITE REG-RELATO       FROM CAB001 AFTER PAGE.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM CAB002 AFTER 1.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM CAB003 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM CAB004 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM CAB005 AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE 8                 TO ACUM-LINHAS.

       004-02-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           MOVE ACUM-LIDOS        TO TOT001-LIDOS.
           WRITE REG-RELATO       FROM TOT001 AFTER 2.
           PERFORM 001-03-FS-RELATO.

       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADPECA
                 RELATO.
           PERFORM 001-01-TESTAR-FS.

       006-00-FIM.                EXIT.

      *================================================================*
       900-00-ERRO                SECTION.
      *================================================================*
           DISPLAY '* ERRO ' FS-OPERACAO ' DO ARQUIVO ' FS-ARQUIVO
           DISPLAY '* FILE STATUS = ' FS-COD-STATUS
           DISPLAY '* PROGRAMA ENCERRADO'
           STOP RUN.

       900-00-FIM.                EXIT.