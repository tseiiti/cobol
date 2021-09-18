       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE11.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR RELATORIO ANALITICO DE MATERIAIS.
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
      *           FILE STATUS     IS FS-CADMAT.
      *   
      *    SELECT CADCONTA        ASSIGN TO UT-S-CADCONTA
      *           FILE STATUS     IS FS-CADCONTA.
      *
      *    SELECT RELATO          ASSIGN TO UR-S-RELATO
      *           FILE STATUS     IS FS-RELATO.
      *
           SELECT     CADMAT      ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADMAT.
      *
           SELECT     CADCONTA    ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADCONTA.
      *
           SELECT     RELATO   ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADMAT
          RECORD      CONTAINS    50 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS STANDARD
          DATA        RECORD      IS REG-CADMAT
          VALUE OF FILE-ID IS "ARQUIVOS/CADMAT.EXER0011.TXT".
       01 REG-CADMAT.
          05 COD-MAT              PIC 9(05).
          05 NOME-MAT             PIC X(20).
          05 QTD-MAT              PIC 9(05). 
          05 VLR-UNIT             PIC 9(05)V99.
          05 CONTA-MAT            PIC X(10).
          05 FILLER               PIC X(03).
      *
       FD CADCONTA
          RECORD      CONTAINS    30 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS STANDARD
          DATA        RECORD      IS REG-CADCONTA
          VALUE OF FILE-ID IS "ARQUIVOS/CADCONTA.TXT".
       01 REG-CADCONTA.
          05 CONTA-CTA            PIC 9(10).
          05 QTD-CTA              PIC 9(06). 
          05 VALOR-CTA            PIC 9(11)V99.
          05 FILLER               PIC X(01).
      *
       FD RELATO
          RECORD      CONTAINS    80 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS OMITTED
          DATA        RECORD      IS REG-RELATO
          VALUE OF FILE-ID IS "ARQUIVOS/RELATO.TXT".
       01 REG-RELATO.
          05 FILLER               PIC X(80).
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
       77 ACUM-QTD-MAT            PIC 9(06) VALUE ZEROS.
       77 ACUM-TOTAL              PIC 9(10)V99 VALUE ZEROS.
       77 ACUM-GER-QTD-MAT        PIC 9(07) VALUE ZEROS.
       77 ACUM-GER-TOTAL          PIC 9(11)V99 VALUE ZEROS.
       77 COMP-VLR-TOT            PIC 9(07)V99.
       77 CONTA-MAT-ANT           PIC X(10).
      *
      * FILE STATUS
      *
       77 FS-CADMAT               PIC X(02) VALUE SPACES.
       77 FS-CADCONTA             PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(56) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB001-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB001-MIN           PIC 9(02).
          05 FILLER               PIC X(08) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(70) VALUE
             'PROGRAMA = EXER0011'.
          05 CAB003-DIA           PIC 9(02)/.
          05 CAB003-MES           PIC 9(02)/.
          05 CAB003-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(18) VALUE SPACES.
          05 FILLER               PIC X(32) VALUE
             'RELATORIO ANALITICO DE MATERIAIS'.
      *
       01  CAB004.
          05 FILLER               PIC X(80) VALUE ALL '='.
      *
       01  CAB005.
          05 FILLER               PIC X(46) VALUE ' CODIGO'.
          05 FILLER               PIC X(05) VALUE 'VALOR'.
      *
       01  CAB006.
          05 FILLER               PIC X(12) VALUE 'MATERIAL'.
          05 FILLER               PIC X(20) VALUE
             'NOME DO MATERIAL'.
          05 FILLER               PIC X(12) VALUE 'QUANTIDADE'.
          05 FILLER               PIC X(12) VALUE 'UNITARIO'.
          05 FILLER               PIC X(15) VALUE 'VALOR TOTAL'.
          05 FILLER               PIC X(05) VALUE 'CONTA'.
      *
       01  DET001.
          05 FILLER               PIC X     VALUE SPACES.
          05 DET001-COD-MAT       PIC ZZ.ZZ9.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-NOME-MAT      PIC X(20).
          05 FILLER               PIC X(04) VALUE SPACES.
          05 DET001-QTD-MAT       PIC ZZ.ZZ9.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-VLR-UNIT      PIC ZZ.ZZ9,99.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-VLR-TOT       PIC Z.ZZZ.ZZ9,99.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-CONTA-MAT     PIC X(10).
      *
       01  TOT001.
          05 FILLER               PIC X(19) VALUE
             ' *  TOTAL DA CONTA'.
          05 TOT001-CONTA-MAT     PIC X(10).
          05 FILLER               PIC X(04) VALUE SPACES.
          05 TOT001-QTD-MAT       PIC ZZZ.ZZ9.
          05 FILLER               PIC X(11) VALUE SPACES.
          05 TOT001-TOTAL         PIC Z.ZZZ.ZZZ.ZZ9,99.
      *
       01  TOT002.
          05 FILLER               PIC X(31) VALUE
             '**  TOTAL GERAL'.
          05 TOT002-QTD-MAT       PIC Z.ZZZ.ZZ9.
          05 FILLER               PIC X(11) VALUE SPACES.
          05 TOT002-TOTAL         PIC ZZ.ZZZ.ZZZ.ZZ9,99.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR-CADMAT
               UNTIL FS-CADMAT EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADMAT
                OUTPUT CADCONTA
                       RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADMAT.
           PERFORM 001-03-FS-CADCONTA.
           PERFORM 001-04-FS-RELATO.

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
       001-03-FS-CADCONTA         SECTION.
      *================================================================*
           MOVE 'CADCONTA'        TO FS-ARQUIVO.
           MOVE FS-CADCONTA       TO FS-COD-STATUS.

           IF FS-CADCONTA NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-RELATO           SECTION.
      *================================================================*
           MOVE 'RELATO'          TO FS-ARQUIVO.
           MOVE FS-RELATO         TO FS-COD-STATUS.

           IF FS-RELATO NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-04-FIM.                EXIT.

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
               DISPLAY '* ARQUIVO CADMAT VAZIO   *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.
               
           MOVE CONTA-MAT         TO CONTA-MAT-ANT.

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
       004-00-TRATAR-CADMAT       SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.
           
           IF CONTA-MAT NOT EQUAL CONTA-MAT-ANT
               PERFORM 004-02-QUEBRA.

           IF ACUM-LINHAS GREATER 59
               PERFORM 004-01-CABECALHOS.

           COMPUTE COMP-VLR-TOT = QTD-MAT * VLR-UNIT.

           MOVE COD-MAT           TO DET001-COD-MAT.
           MOVE NOME-MAT          TO DET001-NOME-MAT.
           MOVE QTD-MAT           TO DET001-QTD-MAT.
           MOVE VLR-UNIT          TO DET001-VLR-UNIT.
           MOVE COMP-VLR-TOT      TO DET001-VLR-TOT.
           MOVE CONTA-MAT         TO DET001-CONTA-MAT.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-04-FS-RELATO.

           ADD 1                  TO ACUM-LINHAS.
           ADD QTD-MAT            TO ACUM-QTD-MAT.
           ADD COMP-VLR-TOT       TO ACUM-TOTAL.
           MOVE CONTA-MAT         TO CONTA-MAT-ANT.
           
           PERFORM 003-01-LER-CADMAT.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-CABECALHOS          SECTION.
      *================================================================*
           ADD 1                  TO ACUM-PAG.
           MOVE ACUM-PAG          TO CAB001-PAG.

           WRITE REG-RELATO       FROM CAB001 AFTER PAGE.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB002 AFTER 1.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB003 AFTER 2.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB004 AFTER 2.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB005 AFTER 1.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB006 AFTER 1.
           PERFORM 001-04-FS-RELATO.

           WRITE REG-RELATO       FROM CAB004 AFTER 1.
           PERFORM 001-04-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-04-FS-RELATO.

           MOVE 7                 TO ACUM-LINHAS.

       004-01-FIM.                EXIT.

      *================================================================*
       004-02-QUEBRA              SECTION.
      *================================================================*
           MOVE CONTA-MAT-ANT     TO CONTA-CTA.
           MOVE ACUM-QTD-MAT      TO QTD-CTA.
           MOVE ACUM-TOTAL        TO VALOR-CTA.
           
           WRITE REG-CADCONTA.
           PERFORM 001-03-FS-CADCONTA.

           MOVE CONTA-MAT-ANT     TO TOT001-CONTA-MAT.
           MOVE ACUM-QTD-MAT      TO TOT001-QTD-MAT.
           MOVE ACUM-TOTAL        TO TOT001-TOTAL.

           ADD ACUM-QTD-MAT       TO ACUM-GER-QTD-MAT.
           ADD ACUM-TOTAL         TO ACUM-GER-TOTAL.
           
           MOVE 0                 TO ACUM-QTD-MAT.
           MOVE 0                 TO ACUM-TOTAL.

           WRITE REG-RELATO       FROM TOT001 AFTER 2.
           PERFORM 001-04-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-04-FS-RELATO.

           ADD 3                  TO ACUM-LINHAS.

       004-02-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           PERFORM 004-02-QUEBRA.
           
           MOVE ACUM-GER-QTD-MAT  TO TOT002-QTD-MAT.
           MOVE ACUM-GER-TOTAL    TO TOT002-TOTAL.
           
           WRITE REG-RELATO       FROM TOT002 AFTER 1.
           PERFORM 001-04-FS-RELATO.

       005-00-FIM.                EXIT.
       
      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADMAT
                 CADCONTA
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