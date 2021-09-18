       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PU8EXE15.
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
           SELECT CADFUNC         ASSIGN TO UT-S-CADFUNC
                      FILE STATUS IS FS-CADFUNC.
      *
           SELECT FUNCINCO        ASSIGN TO UT-S-FUNCINCO
                      FILE STATUS IS FS-FUNCINCO.
      *
           SELECT FUNCOK          ASSIGN TO UT-S-FUNCOK
                      FILE STATUS IS FS-FUNCOK.
      *
           SELECT RELATO          ASSIGN TO UR-S-RELATO
                      FILE STATUS IS FS-RELATO.
      *            
       DATA           DIVISION.
       FILE           SECTION.
      *
       FD CADFUNC
           RECORD     CONTAINS    110 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-CADFUNC.
       01 REG-CADFUNC.
          05 CODIGO-R.
             10 CODIGO            PIC 9(05).
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
       FD FUNCINCO
           RECORD     CONTAINS    35 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-FUNCINCO.
       01 REG-FUNCINCO.
          05 COD-ERRO             PIC 9(05).
          05 NOME-ERRO            PIC X(30).
      *
       FD FUNCOK
           RECORD     CONTAINS    35 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS STANDARD
           DATA       RECORD      IS REG-FUNCOK.
       01 REG-FUNCOK.
          05 COD-OK               PIC 9(05).
          05 NOME-OK              PIC X(30).
      *
       FD RELATO
           RECORD     CONTAINS    78 CHARACTERS
           RECORDING  MODE        IS F
           LABEL      RECORD      IS OMITTED
           DATA       RECORD      IS REG-RELATO.
       01 REG-RELATO.
          05 FILLER               PIC X(78).
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
       77 ACUM-LIDOS              PIC 9(04) VALUE ZEROS.
       77 ACUM-ERRO               PIC 9(04) VALUE ZEROS.
       77 ACUM-OK                 PIC 9(04) VALUE ZEROS.
      *
      *  AREAS DE TRABALHO 
      *
       77 WS-MENSAGEM             PIC X(30).
       77 WS-CODIGO               PIC X(05).
       77 WS-NOME                 PIC X(30).
      *
      * FILE STATUS
      *
       77 FS-CADFUNC              PIC X(02) VALUE SPACES.
       77 FS-FUNCINCO             PIC X(02) VALUE SPACES.
       77 FS-FUNCOK               PIC X(02) VALUE SPACES.
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
          05 FILLER               PIC X(49) VALUE
             'FUTURE SCHOOL CURSOS DE COMPUTACAO'.
          05 CAB002-HOR           PIC 9(02).
          05 FILLER               PIC X     VALUE ':'.
          05 CAB002-MIN           PIC 9(02).
          05 FILLER               PIC X(13) VALUE SPACES.
          05 FILLER               PIC X(06) VALUE 'PAG.: '.
          05 CAB001-PAG           PIC Z.ZZ9.
      *
       01  CAB002.
          05 FILLER               PIC X(68) VALUE
             'PROGRAMA = EXER0015'.
          05 CAB002-DIA           PIC 9(02)/.
          05 CAB002-MES           PIC 9(02)/.
          05 CAB002-ANO           PIC 9(04).
      *
       01  CAB003.
          05 FILLER               PIC X(24) VALUE SPACES.
          05 FILLER               PIC X(35) VALUE
             'RELATORIO DA MOVIMENTACAO'.
      *                          
       01  CAB004.
          05 FILLER               PIC X(22) VALUE '  CODIGO'.
      *
       01  CAB005.
          05 FILLER               PIC X(17) VALUE 'FUNCIONARIO'.
          05 FILLER               PIC X(38) VALUE 
             'NOME DO FUNCIONARIO'.
          05 FILLER               PIC X(08) VALUE 'MENSAGEM'.
      *
       01  DET001.
          05 FILLER               PIC X(03) VALUE SPACES.
          05 DET001-CODIGO        PIC X(05).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-NOME          PIC X(30).
          05 FILLER               PIC X(05) VALUE SPACES.
          05 DET001-MENSAGEM      PIC X(30).
      *
       01  TOT001.
          05 FILLER               PIC X(36) VALUE
             '*** TOTAL DE REGISTROS LIDOS    ==>'.
          05 TOT001-LIDOS         PIC ZZZ.ZZ9.
      *
       01  TOT002.
          05 FILLER               PIC X(36) VALUE
             '*** TOTAL DE REGISTROS COM ERRO ==>'.
          05 TOT002-ERRO         PIC ZZZ.ZZ9.
      *
       01  TOT003.
          05 FILLER               PIC X(36) VALUE
             '*** TOTAL DE REGISTROS OK       ==>'.
          05 TOT003-OK            PIC ZZZ.ZZ9.
      *
       PROCEDURE      DIVISION.

      *================================================================*
       000-00-INICIO              SECTION.
      *================================================================*
           PERFORM 001-00-ABRIR-ARQUIVOS.
           PERFORM 002-00-OBTER-DATA-HORA.
           PERFORM 003-00-VER-ARQ-VAZIO.
           PERFORM 004-00-TRATAR
               UNTIL FS-CADFUNC  EQUAL '10'.
           PERFORM 005-00-IMPRIMIR-TOTAIS.
           PERFORM 006-00-FECHAR-ARQUIVOS.
           STOP RUN.

      *================================================================*
       001-00-ABRIR-ARQUIVOS      SECTION.
      *================================================================*
           MOVE FS-ABERTURA       TO FS-OPERACAO.
           OPEN INPUT  CADFUNC
                OUTPUT FUNCINCO
                       FUNCOK
                       RELATO.
           PERFORM 001-01-TESTAR-FS.

       001-00-FIM.                EXIT.

      *================================================================*
       001-01-TESTAR-FS           SECTION.
      *================================================================*
           PERFORM 001-02-FS-CADFUNC.
           PERFORM 001-03-FS-FUNCINCO.
           PERFORM 001-04-FS-FUNCOK.
           PERFORM 001-05-FS-RELATO.

       001-01-FIM.                EXIT.

      *================================================================*
       001-02-FS-CADFUNC               SECTION.
      *================================================================*
           MOVE 'CADFUNC'         TO FS-ARQUIVO.
           MOVE FS-CADFUNC        TO FS-COD-STATUS.

           IF FS-CADFUNC NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-02-FIM.                EXIT.

      *================================================================*
       001-03-FS-FUNCINCO         SECTION.
      *================================================================*
           MOVE 'FUNCINCO'        TO FS-ARQUIVO.
           MOVE FS-FUNCINCO       TO FS-COD-STATUS.

           IF FS-FUNCINCO NOT EQUAL '00' AND '10'
               PERFORM 900-00-ERRO.

       001-03-FIM.                EXIT.

      *================================================================*
       001-04-FS-FUNCOK           SECTION.
      *================================================================*
           MOVE 'FUNCOK'          TO FS-ARQUIVO.
           MOVE FS-FUNCOK         TO FS-COD-STATUS.

           IF FS-FUNCOK NOT EQUAL '00' AND '10'
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
           PERFORM 003-01-LER-CADFUNC.

           IF FS-CADFUNC EQUAL '10'
               DISPLAY '* ARQUIVO CADFUNC VAZIO  *'
               DISPLAY '* PROGRAMA ENCERRADO     *'
               PERFORM 006-00-FECHAR-ARQUIVOS
               STOP RUN.

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
       004-00-TRATAR              SECTION.
      *================================================================*
           MOVE FS-GRAVACAO       TO FS-OPERACAO.
           
           MOVE CODIGO-R          TO WS-CODIGO.
           MOVE NOME              TO WS-NOME.
           MOVE "REGISTRO OK"     TO WS-MENSAGEM.
           
           IF CODIGO-R EQUAL SPACES
               MOVE "CODIGO NAO PREENCHIDO" TO WS-MENSAGEM
               PERFORM 004-01-IMPRESSAO.
           
           IF CODIGO-R NOT NUMERIC
               MOVE "CODIGO NAO NUMERICO" TO WS-MENSAGEM
               PERFORM 004-01-IMPRESSAO.

           IF CODIGO-R EQUAL 00000
               MOVE "CODIGO ZERO" TO WS-MENSAGEM
               PERFORM 004-01-IMPRESSAO.
           
           IF NOME EQUAL SPACES
               MOVE "NOME NAO PREENCHIDO" TO WS-MENSAGEM
               PERFORM 004-01-IMPRESSAO.
           
           IF WS-MENSAGEM EQUAL "REGISTRO OK"
               PERFORM 004-01-IMPRESSAO
               PERFORM 004-04-GRAVA-FUNCOK
               ADD 1              TO ACUM-OK
           ELSE
               PERFORM 004-03-GRAVA-FUNCINCO
               ADD 1              TO ACUM-ERRO.
           
           ADD 1                  TO ACUM-LIDOS.
           
           PERFORM 003-01-LER-CADFUNC.

       004-00-FIM.                EXIT.

      *================================================================*
       004-01-IMPRESSAO           SECTION.
      *================================================================*
           IF ACUM-LINHAS GREATER 59
               PERFORM 004-02-CABECALHOS.
             
           MOVE WS-CODIGO         TO DET001-CODIGO.
           MOVE WS-NOME           TO DET001-NOME.
           MOVE WS-MENSAGEM       TO DET001-MENSAGEM.

           WRITE REG-RELATO       FROM DET001 AFTER 1.
           PERFORM 001-05-FS-RELATO.
           
           MOVE SPACES            TO WS-CODIGO
                                     WS-NOME.
                                     
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

           MOVE SPACES            TO REG-RELATO.
           WRITE REG-RELATO       AFTER 1.
           PERFORM 001-05-FS-RELATO.

           MOVE 8                 TO ACUM-LINHAS.

       004-02-FIM.                EXIT.

      *================================================================*
       004-03-GRAVA-FUNCINCO      SECTION.
      *================================================================*

           MOVE CODIGO-R          TO COD-ERRO.
           MOVE NOME              TO NOME-ERRO.

           WRITE REG-FUNCINCO.
           PERFORM 001-03-FS-FUNCINCO.
           
       004-03-FIM.                EXIT.

      *================================================================*
       004-04-GRAVA-FUNCOK        SECTION.
      *================================================================*

           MOVE CODIGO-R          TO COD-OK.
           MOVE NOME              TO NOME-OK.

           WRITE REG-FUNCOK.
           PERFORM 001-04-FS-FUNCOK.
           
       004-04-FIM.                EXIT.

      *================================================================*
       005-00-IMPRIMIR-TOTAIS     SECTION.
      *================================================================*
           MOVE ACUM-LIDOS        TO TOT001-LIDOS.
           MOVE ACUM-ERRO         TO TOT002-ERRO.
           MOVE ACUM-OK           TO TOT003-OK.
       
           WRITE REG-RELATO       FROM TOT001 AFTER 2.
           PERFORM 001-05-FS-RELATO.
       
           WRITE REG-RELATO       FROM TOT002 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           WRITE REG-RELATO       FROM TOT003 AFTER 1.
           PERFORM 001-05-FS-RELATO.

           DISPLAY 'TOTAL DE REGISTROS LIDOS    ==> ' ACUM-LIDOS.
           DISPLAY 'TOTAL DE REGISTROS COM ERRO ==> ' ACUM-ERRO.
           DISPLAY 'TOTAL DE REGISTROS OK       ==> ' ACUM-OK.
           
       005-00-FIM.                EXIT.

      *================================================================*
       006-00-FECHAR-ARQUIVOS     SECTION.
      *================================================================*
           MOVE FS-FECHAMENTO     TO FS-OPERACAO.
           CLOSE CADFUNC
                 FUNCINCO
                 FUNCOK
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
