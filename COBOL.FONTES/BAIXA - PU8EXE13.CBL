       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE13.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GERAR RELATORIO DE FUNCIONAIOS POR SECAO.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
      *    SELECT CADFUNC         ASSIGN TO UT-S-CADFUNC
      *               FILE STATUS IS FS-CADFUNC.
      *
      *    SELECT RELATO          ASSIGN TO UR-S-RELATO
      *               FILE STATUS IS FS-RELATO.
      *            
           SELECT     CADFUNC     ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-CADFUNC.
      *
           SELECT     RELATO      ASSIGN TO DISK
                                  ORGANIZATION LINE SEQUENTIAL
                                  ACCESS SEQUENTIAL
                                  FILE STATUS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADFUNC
          RECORD      CONTAINS    110 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS STANDARD
          DATA        RECORD      IS REG-CADFUNC
          VALUE OF FILE-ID IS "ARQUIVOS/CADFUNC.EXER0013.TXT".
       01 REG-CADFUNC.
          05 CODIGO               PIC 9(05).
          05 NOME                 PIC X(30).
          05 DT-NASC              PIC 9(06).
          05 DT-ADMIS             PIC 9(06).
          05 ENDER                PIC X(30).
          05 CEP                  PIC 9(08).
          05 UF                   PIC X(02).
          05 FILLER               PIC X(04).
          05 SALARIO              PIC 9(07)V99.
          05 DESCON               PIC 9(07)V99.
          05 SECAO                PIC X(01).
      *
       FD RELATO
          RECORD      CONTAINS    67 CHARACTERS
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
       77 ACUM-FUNC-SEC           PIC 9(04) VALUE ZEROS.
       77 ACUM-SAL-SEC            PIC 9(09)V99 VALUE ZEROS.
       77 ACUM-GER-FUNC           PIC 9(05) VALUE ZEROS.
       77 ACUM-GER-SAL            PIC 9(10)V99 VALUE ZEROS.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-SECAO-ANT            PIC X.
       77 WS-IMP-SECAO            PIC X.
      *
      * FILE STATUS
      *
       77 FS-CADFUNC              PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(42) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB002-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB002-MIN           PIC 9(02).
          05 FILLER               PIC X(09) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(57) VALUE
             'PROGRAMA = EXER0013'.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(15) VALUE SPACES.
          05 FILLER               PIC X(35) VALUE
             'RELATORIO DE FUNCIONARIOS POR SECAO'.
      *                          
       01  CAB004.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 FILLER               PIC X(07) VALUE 'SECAO'.
          05 FILLER               PIC X(22) VALUE 'CODIGO'.
          05 FILLER               PIC X(25) VALUE 'NOME'.
          05 FILLER               PIC X(07) VALUE 'SALARIO'.
      *
       01  DET001.
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-SECAO         PIC X.
          05 FILLER               PIC X(04) VALUE SPACES.
          05 DET001-CODIGO        PIC ZZ.ZZ9.
          05 FILLER               PIC X(04) VALUE SPACES.
          05 DET001-NOME          PIC X(30).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-SALARIO       PIC Z.ZZZ.ZZ9,99.
      *
       01  TOT001.
          05 FILLER               PIC X(31) VALUE
             'TOTAL DE FUNCIONARIOS DA SECAO'.
          05 TOT001-SECAO         PIC X.
          05 FILLER               PIC X(05) VALUE SPACES.
          05 FILLER               PIC X(04) VALUE ':'.
          05 TOT001-FUNC-SEC      PIC Z.ZZ9.
      *
       01  TOT002.
          05 FILLER               PIC X(35) VALUE
             'TOTAL DE SALARIOS PAGOS PELA SECAO'.
          05 TOT002-SECAO         PIC X(02).
          05 FILLER               PIC X(04) VALUE ':'.
          05 TOT002-SAL-SEC       PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  TOT003.
          05 FILLER               PIC X(40) VALUE
             'TOTAL GERAL DE FUNCIONARIOS          :'.
          05 TOT003-GER-FUNC      PIC ZZ.ZZ9.
      *
       01  TOT004.
          05 FILLER               PIC X(39) VALUE
             'TOTAL GERAL DE SALARIOS PAGOS        :'.
          05 TOT004-GER-SAL       PIC Z.ZZZ.ZZZ.ZZ9,99.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR-CADFUNC
               UNTIL FS-CADFUNC  EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADFUNC
                OUTPUT RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADFUNC.
           PERFORM 001-03-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADFUNC          SECTION.
      *================================================================*
           MOVE 'CADFUNC'         TO FS-ARQUIVO.
           MOVE FS-CADFUNC        TO FS-COD-STATUS.

           IF FS-CADFUNC NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-RELATO           SECTION.
      *================================================================*
           MOVE 'RELATO'          TO FS-ARQUIVO.
           MOVE FS-RELATO      TO FS-COD-STATUS.

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
           PERFORM 003-01-LER-CADFUNC.

           IF FS-CADFUNC EQUAL '10'
               DISPLAY '* ARQUIVO CADFUNC VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

           MOVE SECAO             TO WS-SECAO-ANT
                                     WS-IMP-SECAO.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CADFUNC         SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CADFUNC.

           IF FS-CADFUNC NOT EQUAL '10'
               PERFORM 001-02-FS-CADFUNC.

       003-01-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR-CADFUNC      SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.
           
           IF WS-SECAO-ANT NOT EQUAL SECAO
               PERFORM 004-03-QUEBRA
               MOVE 60            TO ACUM-LINHAS
               MOVE SECAO         TO WS-SECAO-ANT
                                     WS-IMP-SECAO.

           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.

           PERFORM 004-01-IMPRESSAO.

           ADD 1                  TO ACUM-LINHAS
                                     ACUM-FUNC-SEC.
           ADD SALARIO            TO ACUM-SAL-SEC.
           
           PERFORM 003-01-LER-CADFUNC.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           MOVE WS-IMP-SECAO      TO DET001-SECAO.
           MOVE CODIGO            TO DET001-CODIGO.
           MOVE NOME              TO DET001-NOME.
           MOVE SALARIO           TO DET001-SALARIO.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-03-FS-RELATO.
           
           MOVE SPACES            TO WS-IMP-SECAO.
           
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

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-03-FS-RELATO.

           MOVE 7                 TO ACUM-LINHAS.

       004-02-FIM.                EXIT.

      *================================================================*
       004-03-QUEBRA              SECTION.
      *================================================================*
           MOVE WS-SECAO-ANT      TO TOT001-SECAO
                                     TOT002-SECAO.
                                                   
           MOVE ACUM-FUNC-SEC     TO TOT001-FUNC-SEC.
           MOVE ACUM-SAL-SEC      TO TOT002-SAL-SEC.
           
           ADD ACUM-FUNC-SEC      TO ACUM-GER-FUNC.
           ADD ACUM-SAL-SEC       TO ACUM-GER-SAL.
           
           MOVE 0                 TO ACUM-FUNC-SEC
                                     ACUM-SAL-SEC.

           WRITE REG-RELATO       FROM TOT001 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM TOT002 AFTER 1.
           PERFORM 001-03-FS-RELATO.

       004-03-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           PERFORM 004-03-QUEBRA.
           
           MOVE ACUM-GER-FUNC     TO TOT003-GER-FUNC.
           MOVE ACUM-GER-SAL      TO TOT004-GER-SAL.

           WRITE REG-RELATO       FROM TOT003 AFTER 2.
           PERFORM 001-03-FS-RELATO.

           WRITE REG-RELATO       FROM TOT004 AFTER 1.
           PERFORM 001-03-FS-RELATO.

       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADFUNC
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