       identification division.
       program-id. loop_statements_03.

       data division.
       working-storage section.
       01 ws-cnt pic 9 value 0.

       procedure division.
       a-para.
           perform b-para until ws-cnt=3.
           stop run.

       b-para.
           display 'ws-cnt : 'ws-cnt.
           add 1 to ws-cnt.
