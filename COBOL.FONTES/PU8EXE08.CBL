       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE08.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR LISTAGEM ANALITICA DE MATERIAIS EM DD/MM/AAAA.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
           SELECT CADMAT          ASSIGN TO UT-S-CADMAT
                      FILE STATUS IS FS-CADMAT.
      *
           SELECT RELATO          ASSIGN TO UR-S-RELATO
                      FILE STATUS IS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADMAT
           RECORD     CONTAINS    40 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADMAT.
       01 REG-CADMAT.
          05 CODIGO               PIC 9(05).
          05 NOME                 PIC X(20).
          05 QUANTIDADE           PIC 9(05).
          05 PRECO                PIC 9(05)V99.
          05 FILLER               PIC X(03).
      *
       FD RELATO
           RECORD     CONTAINS    82 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS OMITTED
           DATA       RECORD      IS REG-RELATO.
       01 REG-RELATO.
          05 FILLER               PIC X(82).
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
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-VLR-TOTAL            PIC 9(06)V99.
       77 WS-VLR-VENDA            PIC 9(06)V99.
      *
      * FILE STATUS
      *
       77 FS-CADMAT               PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(54) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB001-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB001-MIN           PIC 9(02).
          05 FILLER               PIC X(12) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(19) VALUE
             'PROGRAMA = EXER0008'.
      *
       01  CAB003.
          05 FILLER               PIC X(19) VALUE SPACES.
          05 FILLER               PIC X(35) VALUE
             'LISTAGEM ANALITICA DE MATERIAIS EM '.
          05 CAB003-DIA           PIC 9(02)/.
          05 CAB003-MES           PIC 9(02)/.
          05 CAB003-ANO           PIC 9(04).
      *
       01  CAB004.
          05 FILLER               PIC X(35) VALUE ' CODIGO'.
          05 FILLER               PIC X(15) VALUE 'QUANTIDADE'.
          05 FILLER               PIC X(05) VALUE 'PRECO'.
      *
       01  CAB005.
          05 FILLER               PIC X(14) VALUE 'MATERIAL'.
          05 FILLER               PIC X(21) VALUE
             'NOME DO MATERIAL'.
          05 FILLER               PIC X(13) VALUE 'EM ESTOQUE'.
          05 FILLER               PIC X(10) VALUE 'UNITARIO'.
          05 FILLER               PIC X(13) VALUE 'VALOR TOTAL'.
          05 FILLER               PIC X(11) VALUE 'VALOR VENDA'.
      *
       01  DET001.
          05 FILLER               PIC X     VALUE SPACES.
          05 DET001-CODIGO        PIC 99.999.
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-NOME          PIC X(20).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-QUANTIDADE    PIC ZZ.ZZ9.
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-PRECO         PIC Z.ZZ9,99.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-VALOR-TOTAL   PIC ZZZ.ZZ9,99.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-VALOR-VENDA   PIC ZZZ.ZZ9,99.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR
               UNTIL FS-CADMAT EQUAL '10'.
           PERFORM 005-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADMAT
                OUTPUT RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADMAT.
           PERFORM 001-03-FS-RELATO.

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
           MOVE WS-ANO-SYS        TO CAB003-ANO.
           MOVE WS-MES-SYS        TO CAB003-MES.
           MOVE WS-DIA-SYS        TO CAB003-DIA.
           MOVE WS-HOR-SYS        TO CAB001-HOR.
           MOVE WS-MIN-SYS        TO CAB001-MIN.

       002-00-FIM.                EXIT.

      *================================================================*
       003-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 003-01-LER-CADMAT.

           IF FS-CADMAT EQUAL '10'
               DISPLAY '* ARQUIVO CADMAT  VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 005-00-FECHAR-ARQUIVOS
               STOP RUN.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CADMAT          SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADMAT.

           IF FS-CADMAT NOT EQUAL '10'
               PERFORM 001-02-FS-CADMAT.

       003-01-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           COMPUTE WS-VLR-TOTAL= QUANTIDADE * PRECO.

           IF CODIGO LESS 10000
               COMPUTE WS-VLR-VENDA= WS-VLR-TOTAL* 1,1
           ELSE IF CODIGO LESS 20000
               COMPUTE WS-VLR-VENDA= WS-VLR-TOTAL* 1,15
           ELSE IF CODIGO LESS 30000
               COMPUTE WS-VLR-VENDA= WS-VLR-TOTAL* 1,22
           ELSE
               COMPUTE WS-VLR-VENDA= WS-VLR-TOTAL* 1,3.

           PERFORM 004-01-IMPRESSAO.

           ADD 1                  TO ACUM-LINHAS.
           
           PERFORM 003-01-LER-CADMAT.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.

           MOVE CODIGO            TO DET001-CODIGO.
           MOVE NOME              TO DET001-NOME.
           MOVE QUANTIDADE        TO DET001-QUANTIDADE.
           MOVE PRECO             TO DET001-PRECO.
           MOVE WS-VLR-TOTAL      TO DET001-VALOR-TOTAL.
           MOVE WS-VLR-VENDA      TO DET001-VALOR-VENDA.

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

           WRITE REG-RELATO       FROM CAB005 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE 7                 TO ACUM-LINHAS.

       004-02-FIM.                EXIT.
       
      *================================================================*
       005-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADMAT
                 RELATO.
           PERFORM 001-01-TESTAR-FS.

       005-00-FIM.                EXIT.

      *================================================================*
       900-00-ERRO                SECTION.
      *================================================================*
           DISPLAY '* ERRO ' FS-OPERACAO ' DO ARQUIVO ' FS-ARQUIVO
           DISPLAY '* FILE STATUS = ' FS-COD-STATUS
           DISPLAY '* PROGRAMA ENCERRADO'
           STOP RUN.

       900-00-FIM.                EXIT.