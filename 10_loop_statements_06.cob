       identification division.
       program-id. loop_statements_06.

       data division.
       working-storage section.
       01 ws-a pic 9 value 2.

       procedure division.
       a-para.
           display 'in a-para'
           go to b-para.

       b-para.
           display 'in b-para '.
           go to c-para d-para depending on ws-a.

       c-para.
           display 'in c-para '.

       d-para.
           display 'in d-para '.
           stop run.
