       identification division.
       program-id. condition_statements_01.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9).
       01 ws-num2 pic 9(9).
       01 ws-num3 pic 9(5).
       01 ws-num4 pic 9(6).

       procedure division.
       a000-first-para.
           move 25 to ws-num1 ws-num3.
           move 15 to ws-num2 ws-num4.

           if ws-num1 > ws-num2 then
             display 'in loop 1 - if block'

             if ws-num3 = ws-num4 then
               display 'in loop 2 - if block'
             else
               display 'in loop 2 - else block'
             end-if

           else
             display 'in loop 1 - else block'
           end-if.

           stop run.
