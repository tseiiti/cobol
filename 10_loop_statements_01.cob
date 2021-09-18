       identification division.
       program-id. loop_statements_01.

       procedure division.
       a-para.
           perform display 'in a-para'
           end-perform.
           perform c-para thru e-para.

       b-para.
           display 'in b-para'.
           stop run.

       c-para.
           display 'in c-para'.

       d-para.
           display 'in d-para'.

       e-para.
           display 'in e-para'.

