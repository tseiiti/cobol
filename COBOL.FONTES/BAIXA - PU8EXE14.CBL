       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE14.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR RELATORIO DE SUMARIZACAO DO CADASTRO DE PRODUTOS.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    SELECT CADPRO          ASSIGN TO UT-S-CADPRO
      *               FILE STATUS IS FS-CADPRO.
      *
      *    SELECT RELATO          ASSIGN TO UR-S-RELATO
      *               FILE STATUS IS FS-RELATO.
      *            
           SELECT     CADPRO      ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADPRO.
      *
           SELECT     RELATO      ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADPRO
          RECORD      CONTAINS    30 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS STANDARD
          DATA        RECORD      IS REG-CADPRO
          VALUE OF FILE-ID IS "ARQUIVOS/CADPRO.TXT".
       01 REG-CADPRO.
          05 PRODUTO              PIC 9(05).
          05 SUB-PRODUTO          PIC 9(05).
          05 PECA                 PIC 9(05).
          05 CUSTO                PIC 9(07)V99.
          05 FILLER               PIC X(06).
      *
       FD RELATO
          RECORD      CONTAINS    52 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS OMITTED
          DATA        RECORD      IS REG-RELATO
          VALUE OF FILE-ID IS "ARQUIVOS/RELATO.TXT".
       01 REG-RELATO.
          05 FILLER               PIC X(67).
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
       77 ACUM-SUB-CUSTO          PIC 9(08)V99 VALUE ZEROS.
       77 ACUM-CUSTO              PIC 9(10)V99 VALUE ZEROS.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-SUB-PROD-ANT         PIC X(05).
       77 WS-PRODUTO-ANT          PIC X(05).
       77 WS-IMP-SUB-PROD         PIC X(05).
       77 WS-IMP-PROD             PIC X(05).
      *
      * FILE STATUS
      *
       77 FS-CADPRO               PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(41) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(42) VALUE
             'PROGRAMA = EXER0014'.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(08) VALUE SPACES.
          05 FILLER               PIC X(35) VALUE
             'SUMARIZACAO DO CADASTRO DE PRODUTOS'.
      *
       01  CAB004.
          05 FILLER               PIC X(13) VALUE ' CODIGO'.
          05 FILLER               PIC X(14) VALUE 'CODIGO'.
          05 FILLER               PIC X(17) VALUE 'CODIGO'.
          05 FILLER               PIC X(05) VALUE 'CUSTO'.
      *
       01  CAB005.
          05 FILLER               PIC X(11) VALUE 'PRODUTO'.
          05 FILLER               PIC X(17) VALUE 'SUBPRODUTO'.
          05 FILLER               PIC X(14) VALUE 'PECA'.
          05 FILLER               PIC X(08) VALUE 'UNITARIO'.
      *
       01  DET001.
          05 FILLER               PIC X     VALUE SPACES.
          05 DET001-PRODUTO       PIC X(05).
          05 FILLER               PIC X(08) VALUE SPACES.
          05 DET001-SUB-PRODUTO   PIC X(05).
          05 FILLER               PIC X(09) VALUE SPACES.
          05 DET001-PECA          PIC X(05).
          05 FILLER               PIC X(07) VALUE SPACES.
          05 DET001-CUSTO         PIC Z.ZZZ.ZZ9,99.
      *
       01  TOT001.
          05 FILLER               PIC X(28) VALUE
             '*  TOTAL CODIGO SUBPRODUTO'.
          05 TOT001-SUB-PRODUTO   PIC X(05).
          05 FILLER               PIC X(06) VALUE SPACES.
          05 TOT001-SUB-CUSTO     PIC ZZ.ZZZ.ZZ9,99.
      *
       01  TOT002.
          05 FILLER               PIC X(28) VALUE
             '** TOTAL CODIGO PRODUTO'.
          05 TOT002-PRODUTO       PIC X(05).
          05 FILLER               PIC X(03) VALUE SPACES.
          05 TOT002-CUSTO         PIC Z.ZZZ.ZZZ.ZZ9,99.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR
               UNTIL FS-CADPRO EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADPRO
                OUTPUT RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADPRO.
           PERFORM 001-03-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADPRO           SECTION.
      *================================================================*
           MOVE 'CADPRO'          TO FS-ARQUIVO.
           MOVE FS-CADPRO         TO FS-COD-STATUS.

           IF FS-CADPRO NOT EQUAL '00' AND '10'
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

       002-00-FIM.                EXIT.

      *================================================================*
       003-00-VER-ARQ-VAZIO       SECTION.
      *================================================================*
           PERFORM 003-01-LER-CADPRO.

           IF FS-CADPRO EQUAL '10'
               DISPLAY '* ARQUIVO CADPRO  VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

           MOVE SUB-PRODUTO       TO WS-SUB-PROD-ANT
                                     WS-IMP-SUB-PROD.
           MOVE PRODUTO           TO WS-PRODUTO-ANT
                                     WS-IMP-PROD.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CADPRO          SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADPRO.

           IF FS-CADPRO NOT EQUAL '10'
               PERFORM 001-02-FS-CADPRO.

       003-01-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF SUB-PRODUTO NOT EQUAL WS-SUB-PROD-ANT
               PERFORM 004-03-QUEBRA-SUB
               MOVE SPACES            TO REG-RELATO
               WRITE REG-RELATO       AFTER 1
               PERFORM 001-03-FS-RELATO.
           
           PERFORM 004-01-IMPRESSAO.
           
           ADD CUSTO              TO ACUM-SUB-CUSTO
                                     ACUM-CUSTO.
           MOVE SPACES            TO WS-IMP-PROD
                                     WS-IMP-SUB-PROD.

           ADD 1                  TO ACUM-LINHAS.
           
           PERFORM 003-01-LER-CADPRO.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.
           
           MOVE WS-IMP-PROD       TO DET001-PRODUTO.    
           MOVE WS-IMP-SUB-PROD   TO DET001-SUB-PRODUTO.
           MOVE PECA              TO DET001-PECA.
           MOVE CUSTO             TO DET001-CUSTO.

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
       004-03-QUEBRA-SUB          SECTION.
      *================================================================*
           MOVE WS-SUB-PROD-ANT   TO TOT001-SUB-PRODUTO.
           MOVE ACUM-SUB-CUSTO    TO TOT001-SUB-CUSTO.
           MOVE SUB-PRODUTO       TO WS-SUB-PROD-ANT
                                     WS-IMP-SUB-PROD.
           MOVE 0                 TO ACUM-SUB-CUSTO.
           
           WRITE REG-RELATO       FROM TOT001 AFTER 2.
           PERFORM 001-03-FS-RELATO.
           
           IF PRODUTO NOT EQUAL WS-PRODUTO-ANT
               PERFORM 004-04-QUEBRA-PRO.
           
           ADD 3                  TO ACUM-LINHAS.

       004-03-FIM.                EXIT.

      *================================================================*
       004-04-QUEBRA-PRO          SECTION.
      *================================================================*
           MOVE WS-PRODUTO-ANT    TO TOT002-PRODUTO.
           MOVE ACUM-CUSTO        TO TOT002-CUSTO.
           MOVE PRODUTO           TO WS-PRODUTO-ANT
                                     WS-IMP-PROD.
           MOVE 0                 TO ACUM-CUSTO.
       
           WRITE REG-RELATO       FROM TOT002 AFTER 0.
           PERFORM 001-03-FS-RELATO.
           
           ADD 1                  TO ACUM-LINHAS.

       004-04-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           PERFORM 004-03-QUEBRA-SUB.
           PERFORM 004-04-QUEBRA-PRO.

       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADPRO
                 RELATO.
           PERFORM 001-01-TESTAR-FS.

           DISPLAY 'PROGRAMA ENCERRADO COM SUCESSO'.
           
       006-00-FIM.                EXIT.

      *================================================================*
       900-00-ERRO                SECTION.
      *================================================================*
           DISPLAY '* ERRO ' FS-OPERACAO ' DO ARQUIVO ' FS-ARQUIVO
           DISPLAY '* FILE STATUS = ' FS-COD-STATUS
           DISPLAY '* PROGRAMA ENCERRADO'
           STOP RUN.

       900-00-FIM.                EXIT.
