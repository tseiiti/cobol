       identification division.
       program-id. condition_statements_02.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9).
       01 ws-num2 pic 9(9).

       procedure division.
       a000-first-para.
           move 25 to ws-num1.
           move 15 to ws-num2.

           if ws-num1 is greater than or equal to ws-num2 then
             display 'ws-num1 is greater than ws-num2'
           else
             display 'ws-num1 is less than ws-num2'
           end-if.

           stop run.
