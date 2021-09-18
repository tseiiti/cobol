       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE10.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR RELATORIO DE ALUNOS MATRICULADOS POR TURMA.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    SELECT CADALUNO        ASSIGN TO UT-S-CADALUNO
      *               FILE STATUS IS FS-CADALUNO.
      *
      *    SELECT RELATO          ASSIGN TO UR-S-RELATO
      *               FILE STATUS IS FS-RELATO.
      *
           SELECT     CADALUNO    ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADALUNO.
      *
           SELECT     RELATO   ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADALUNO
          RECORD      CONTAINS    60 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS STANDARD
          DATA        RECORD      IS REG-CADALUNO
          VALUE OF FILE-ID IS "ARQUIVOS/CADALUNO.TXT".
       01 REG-CADALUNO.
          05 CODALU               PIC 9(04).
          05 NOMEALU              PIC X(30).
          05 TURMAALU             PIC 9(03).
          05 NOTA1-ALU            PIC 9(02)V99.
          05 NOTA2-ALU            PIC 9(02)V99.
          05 NOTA3-ALU            PIC 9(02)V99.
          05 NOTA4-ALU            PIC 9(02)V99.
          05 FILLER               PIC X(07).
      *
       FD RELATO
          RECORD      CONTAINS    60 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS OMITTED
          DATA        RECORD      IS REG-RELATO
          VALUE OF FILE-ID IS "ARQUIVOS/RELATO.TXT".
       01 REG-RELATO.
          05 FILLER               PIC X(60).
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
       77 ACUM-LIDOS              PIC 9(05) VALUE ZEROS.
       77 ACUM-TURMALIDOS         PIC 9(04) VALUE ZEROS.
       77 TURMA-ANT               PIC 9(04) VALUE ZEROS.
      *
      * FILE STATUS
      *
       77 FS-CADALUNO             PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(39) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB002-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB002-MIN           PIC 9(02).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(50) VALUE
             'PROGRAMA = EXER0010'.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(09) VALUE SPACES.
          05 FILLER               PIC X(52) VALUE
             'RELATORIO DE ALUNOS MATRICULADOS POR TURMA'.
      *
       01  CAB004.
          05 FILLER               PIC X(60) VALUE ALL '='.
      *                          
       01  CAB005.
          05 FILLER               PIC X(18) VALUE 'CODIGO'.
          05 FILLER               PIC X(37) VALUE 'NOME DO ALUNO'.
          05 FILLER               PIC X(05) VALUE 'TURMA'.
      *
       01  DET001.
          05 FILLER               PIC X     VALUE SPACES.
          05 DET001-CODALU        PIC 9(04).
          05 FILLER               PIC X(10) VALUE SPACES.
          05 DET001-NOMEALU       PIC X(30).
          05 FILLER               PIC X(11) VALUE SPACES.
          05 DET001-TURMAALU      PIC 9(03).
      *
       01  TOT001.
          05 FILLER               PIC X(39) VALUE
             '***** TOTAL DE ALUNOS LIDOS        =>'.
          05 TOT001-LIDOS         PIC ZZ.ZZ9.
      *
       01  TOT002.
          05 FILLER               PIC X(12) VALUE '***** TURMA'.
          05 TOT002-TURMAALU      PIC 9(03).
          05 FILLER               PIC X(04) VALUE SPACES.
          05 FILLER               PIC X(21) VALUE
             'TOTAL DE ALUNOS => '.
          05 TOT002-TURMALIDOS    PIC Z.ZZ9.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR-CADALUNO
               UNTIL FS-CADALUNO EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADALUNO
                OUTPUT RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADALUNO.
           PERFORM 001-03-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADALUNO         SECTION.
      *================================================================*
           MOVE 'CADALUNO'        TO FS-ARQUIVO.
           MOVE FS-CADALUNO       TO FS-COD-STATUS.

           IF FS-CADALUNO NOT EQUAL '00' AND '10'
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
           PERFORM 003-01-LER-CADALUNO.

           IF FS-CADALUNO EQUAL '10'
               DISPLAY '* ARQUIVO CADALUNO VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

           MOVE TURMAALU          TO TURMA-ANT.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CADALUNO        SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADALUNO.

           IF FS-CADALUNO NOT EQUAL '10'
               PERFORM 001-02-FS-CADALUNO.

       003-01-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR-CADALUNO     SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.
           
           IF TURMA-ANT NOT EQUAL TURMAALU
               PERFORM 004-02-QUEBRA.

           IF ACUM-LINHAS GREATER 59
               PERFORM 004-01-CABECALHOS.

           MOVE CODALU            TO DET001-CODALU.
           MOVE NOMEALU           TO DET001-NOMEALU.
           MOVE TURMAALU          TO DET001-TURMAALU.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-03-FS-RELATO.
           
           ADD 1                  TO ACUM-LIDOS.
           ADD 1                  TO ACUM-LINHAS.
           ADD 1                  TO ACUM-TURMALIDOS.
           
           PERFORM 003-01-LER-CADALUNO.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-CABECALHOS          SECTION.
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

           WRITE REG-RELATO       FROM CAB004 AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE 9                 TO ACUM-LINHAS.

       004-01-FIM.                EXIT.

      *================================================================*
       004-02-QUEBRA              SECTION.
      *================================================================*
           MOVE TURMA-ANT         TO TOT002-TURMAALU.
           MOVE ACUM-TURMALIDOS   TO TOT002-TURMALIDOS.
           MOVE 0                 TO ACUM-TURMALIDOS.

           WRITE REG-RELATO       FROM TOT002 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-03-FS-RELATO.
           
           MOVE TURMAALU          TO TURMA-ANT.
           ADD 3                  TO ACUM-LINHAS.

       004-01-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           PERFORM 004-02-QUEBRA.
           
           MOVE ACUM-LIDOS        TO TOT001-LIDOS.
           WRITE REG-RELATO       FROM TOT001 AFTER 1.
           PERFORM 001-03-FS-RELATO.

       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADALUNO
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