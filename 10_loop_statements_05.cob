       identification division.
       program-id. loop_statements_05.

       data division.
       working-storage section.
       01 ws-a pic 9 value 0.

       procedure division.
       a-para.
           perform b-para varying ws-a from 1 by 1 until ws-a=5
           stop run.

       b-para.
           display 'in b-para ' ws-a.
