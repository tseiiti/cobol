       identification division.
       program-id. condition_statements_05.

       data division.
       working-storage section.
       01 ws-num pic 9(3).
       88 pass values are 041 thru 100.
       88 fail values are 000 thru 40.

       procedure division.
       a000-first-para.
           move 65 to ws-num.

           if pass
             display 'passed with ' ws-num ' marks'.

           if fail
             display 'failed with ' ws-num 'marks'.

           stop run.
