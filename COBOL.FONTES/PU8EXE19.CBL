       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE19.
       AUTHOR.        OSCAR SEI ITI TANIGUCHI.
      *
      * GRAVAR ARQUIVO DE CONTA CORRENTE E LISTAR CORRENTISTAS.
      *
       ENVIRONMENT    DIVISION.
       CONFIGURATION  SECTION.
       SPECIAL-NAMES.
                      DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT   SECTION.
       FILE-CONTROL.
      *
           SELECT CTCORABC        ASSIGN TO UT-S-CTCORABC
                      FILE STATUS IS FS-CTCORABC.
      *
           SELECT CTCORXYZ        ASSIGN TO UT-S-CTCORXYZ
                      FILE STATUS IS FS-CTCORXYZ.
      *
           SELECT CTCORGER        ASSIGN TO UT-S-CTCORGER
                      FILE STATUS IS FS-CTCORGER.
      *
           SELECT RELATO          ASSIGN TO UR-S-RELATO
                      FILE STATUS IS FS-RELATO.
      *
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CTCORABC
           RECORD     CONTAINS    68 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CTCORABC.
       01 REG-CTCORABC.
          05 CPF-ABC              PIC 9(11).
          05 NOME-ABC             PIC X(30).
          05 DATA-ABC.
             10 DIA-ABC           PIC 9(02).
             10 MES-ABC           PIC 9(02).
             10 ANO-ABC           PIC 9(04).
          05 FILLER               PIC X(11).
      *
       FD CTCORXYZ
           RECORD     CONTAINS    68 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CTCORXYZ.
       01 REG-CTCORXYZ.
          05 CPF-XYZ              PIC 9(11).
          05 NOME-XYZ             PIC X(30).
          05 DATA-XYZ.
             10 DIA-XYZ           PIC 9(02).
             10 MES-XYZ           PIC 9(02).
             10 ANO-XYZ           PIC 9(04).
          05 FILLER               PIC X(11).
      *
       FD CTCORGER
           RECORD     CONTAINS    50 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CTCORGER.
       01 REG-CTCORGER.
          05 CPF-GER              PIC 9(11).
          05 NOME-GER             PIC X(30).
          05 BCO-GER              PIC X(03).
          05 FILLER               PIC X(06).
      *
       FD RELATO
          RECORD      CONTAINS    58 CHARACTERS
          RECORDING   MODE        IS F
          LABEL       RECORD      IS OMITTED
          DATA        RECORD      IS REG-RELATO.
       01 REG-RELATO.
          05 FILLER               PIC X(58).
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
       77 ACUM-ABC                PIC 9(06) VALUE ZEROS.
       77 ACUM-XYZ                PIC 9(06) VALUE ZEROS.
       77 ACUM-GRAV               PIC 9(06) VALUE ZEROS.
       01 WS-DATA-ABC.
          05 WS-ANO-ABC           PIC 9(04).
          05 WS-MES-ABC           PIC 9(02).
          05 WS-DIA-ABC           PIC 9(02).
       01 WS-DATA-XYZ.
          05 WS-ANO-XYZ           PIC 9(04).
          05 WS-MES-XYZ           PIC 9(02).
          05 WS-DIA-XYZ           PIC 9(02).
      *
      *  AREAS DE TRABALHO
      *
       01 WS-CPF.
          05 WS-CPF-NUM           PIC 9(09).
          05 WS-CPF-DIG           PIC 9(02).
       77 WS-NOME                 PIC X(30).
       77 WS-BCO                  PIC X(03).
       77 WS-CPF-ANT              PIC 9(11) VALUE ZEROS.
      *
      * FILE STATUS
      *
       77 FS-CTCORABC             PIC X(02) VALUE SPACES.
       77 FS-CTCORXYZ             PIC X(02) VALUE SPACES.
       77 FS-CTCORGER             PIC X(02) VALUE SPACES.
       77 FS-RELATO               PIC X(02) VALUE SPACES.
       77 FS-COD-STATUS           PIC X(02) VALUE SPACES.
       77 FS-ARQUIVO              PIC X(08) VALUE SPACES.
       77 FS-OPERACAO             PIC X(13) VALUE SPACES.
       77 FS-ABERTURA             PIC X(13) VALUE 'NA ABERTURA'.
       77 FS-LEITURA              PIC X(13) VALUE 'NA LEITURA'.
       77 FS-GRAVACAO             PIC X(13) VALUE 'NA GRAVACAO'.
       77 FS-FECHAMENTO           PIC X(13) VALUE 'NO FECHAMENTO'.
      *
      *
      *  LINHAS DO RELATORIO
      *
       01  CAB001.
          05 FILLER               PIC X(38) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB002-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB002-MIN           PIC 9(02).
          05 FILLER               PIC X(04) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(48) VALUE
             'PROGRAMA = EXER0019'.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(16) VALUE SPACES.
          05 FILLER               PIC X(25) VALUE
             'LISTAGEM DOS CORRENTISTAS'.
      *
       01  CAB004.
          05 FILLER               PIC X(58) VALUE ALL '='.
      *
       01  CAB005.
          05 FILLER               PIC X(24) VALUE '  CODIGO CPF'.
          05 FILLER               PIC X(29) VALUE
             'NOME DO CORRENTISTA'.
          05 FILLER               PIC X(05) VALUE 'BANCO'.
      *
       01  DET001.
          05 DET001-CPF-NUM       PIC 999.999.999.
          05 FILLER               PIC X(01) VALUE '-'.
          05 DET001-CPF-DIG       PIC 99.
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-NOME          PIC X(30).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-BANCO         PIC X(03).
      *
       01  TOT001.
          05 FILLER               PIC X(44) VALUE
             'TOTAL DE CORRENTISTAS LIDOS    (BANCO ABC):'.
          05 TOT001-ABC           PIC ZZZ.ZZ9.
      *
       01  TOT002.
          05 FILLER               PIC X(44) VALUE
             'TOTAL DE CORRENTISTAS LIDOS    (BANCO XYZ):'.
          05 TOT002-XYZ           PIC ZZZ.ZZ9.
      *
       01  TOT003.
          05 FILLER               PIC X(44) VALUE
             'TOTAL DE CORRENTISTAS GRAVADOS (BANCO GER):'.
          05 TOT003-GRAV          PIC ZZZ.ZZ9.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR
               UNTIL FS-CTCORABC EQUAL '10' AND
                     FS-CTCORXYZ EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CTCORABC
                       CTCORXYZ
                OUTPUT CTCORGER
                       RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CTCORABC.
           PERFORM 001-03-FS-CTCORXYZ.
           PERFORM 001-04-FS-CTCORGER.
           PERFORM 001-05-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CTCORABC         SECTION.
      *================================================================*
           MOVE 'CTCORABC'        TO FS-ARQUIVO.
           MOVE FS-CTCORABC       TO FS-COD-STATUS.

           IF FS-CTCORABC NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-CTCORXYZ         SECTION.
      *================================================================*
           MOVE 'CTCORXYZ'        TO FS-ARQUIVO.
           MOVE FS-CTCORXYZ       TO FS-COD-STATUS.

           IF FS-CTCORXYZ NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-CTCORGER         SECTION.
      *================================================================*
           MOVE 'CTCORGER'        TO FS-ARQUIVO.
           MOVE FS-CTCORGER       TO FS-COD-STATUS.

           IF FS-CTCORGER NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-04-FIM.                EXIT.

      *================================================================*
       001-05-FS-RELATO           SECTION.
      *================================================================*
           MOVE 'RELATO'          TO FS-ARQUIVO.
           MOVE FS-RELATO         TO FS-COD-STATUS.

           IF FS-RELATO NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-05-FIM.                EXIT.

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
           PERFORM 003-01-LER-CTCORABC.

           IF FS-CTCORABC EQUAL '10'
               DISPLAY '* ARQUIVO CTCORABC VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

           PERFORM 003-02-LER-CTCORXYZ.

           IF FS-CTCORXYZ EQUAL '10'
               DISPLAY '* ARQUIVO CTCORXYZ VAZIO *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

       003-00-FIM.                EXIT.

      *================================================================*
       003-01-LER-CTCORABC        SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CTCORABC.

           IF FS-CTCORABC NOT EQUAL '10'
               PERFORM 001-02-FS-CTCORABC
               ADD 1              TO ACUM-XYZ.

       003-01-FIM.                EXIT.

      *================================================================*
       003-02-LER-CTCORXYZ        SECTION.
      *================================================================*
           MOVE FS-LEITURA        TO FS-OPERACAO.
           READ CTCORXYZ.

           IF FS-CTCORXYZ NOT EQUAL '10'
               PERFORM 001-03-FS-CTCORXYZ
               ADD 1              TO ACUM-ABC.

       003-02-FIM.                EXIT.

      *================================================================*
       004-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.

           IF CPF-ABC EQUAL CPF-XYZ
               MOVE ANO-ABC       TO WS-ANO-ABC
               MOVE MES-ABC       TO WS-MES-ABC
               MOVE DIA-ABC       TO WS-DIA-ABC
               MOVE ANO-XYZ       TO WS-ANO-XYZ
               MOVE MES-XYZ       TO WS-MES-XYZ
               MOVE DIA-XYZ       TO WS-DIA-XYZ

               IF WS-DATA-ABC LESS WS-DATA-XYZ AND
                  FS-CTCORABC NOT EQUAL '10' OR
                  FS-CTCORXYZ EQUAL '10'
                   PERFORM 004-03-GRAVA-ABC
               ELSE
                   PERFORM 004-04-GRAVA-XYZ
               END-IF
           ELSE
               IF CPF-ABC LESS CPF-XYZ AND
                  FS-CTCORABC NOT EQUAL '10' OR
                  FS-CTCORXYZ EQUAL '10'
                   PERFORM 004-03-GRAVA-ABC
               ELSE
                   PERFORM 004-04-GRAVA-XYZ
               END-IF
           END-IF.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           ADD 1              TO ACUM-GRAV.

           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.

           MOVE WS-CPF-NUM        TO DET001-CPF-NUM.
           MOVE WS-CPF-DIG        TO DET001-CPF-DIG.
           MOVE WS-NOME           TO DET001-NOME.
           MOVE WS-BCO            TO DET001-BANCO.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           ADD 1                  TO ACUM-LINHAS.

       004-01-FIM.                EXIT.

      *================================================================*
       004-02-CABECALHOS          SECTION.
      *================================================================*
           ADD 1                  TO ACUM-PAG.
           MOVE ACUM-PAG          TO CAB001-PAG.

           WRITE REG-RELATO       FROM CAB001 AFTER PAGE.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM CAB002 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM CAB003 AFTER 2.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM CAB004 AFTER 2.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM CAB005 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM CAB004 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-05-FS-RELATO.

           MOVE 9                 TO ACUM-LINHAS.

       004-02-FIM.                EXIT.

      *================================================================*
       004-03-GRAVA-ABC           SECTION.
      *================================================================*
           IF CPF-ABC GREATER WS-CPF-ANT
               MOVE CPF-ABC       TO CPF-GER
                                     WS-CPF
               MOVE NOME-ABC      TO NOME-GER
                                     WS-NOME
               MOVE 'ABC'         TO BCO-GER
                                     WS-BCO

               WRITE REG-CTCORGER
               PERFORM 001-04-FS-CTCORGER

               MOVE CPF-ABC       TO WS-CPF-ANT
               PERFORM 004-01-IMPRESSAO.

           PERFORM 003-01-LER-CTCORABC.

       004-03-FIM.                EXIT.

      *================================================================*
       004-04-GRAVA-XYZ           SECTION.
      *================================================================*
           IF CPF-XYZ GREATER WS-CPF-ANT
               MOVE CPF-XYZ       TO CPF-GER
                                     WS-CPF
               MOVE NOME-XYZ      TO NOME-GER
                                     WS-NOME
               MOVE 'XYZ'         TO BCO-GER
                                     WS-BCO

               WRITE REG-CTCORGER
               PERFORM 001-04-FS-CTCORGER

               MOVE CPF-XYZ       TO WS-CPF-ANT
               PERFORM 004-01-IMPRESSAO.

           PERFORM 003-02-LER-CTCORXYZ.

       004-04-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           MOVE ACUM-ABC          TO TOT001-ABC.
           MOVE ACUM-XYZ          TO TOT002-XYZ.
           MOVE ACUM-GRAV         TO TOT003-GRAV.

           WRITE REG-RELATO       FROM TOT001 AFTER 3.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM TOT002 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM TOT003 AFTER 1.
           PERFORM 001-05-FS-RELATO.

       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CTCORABC
                 CTCORXYZ
                 CTCORGER
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
