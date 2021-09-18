       identification division.
       program-id. condition_statements_06.

       data division.
       working-storage section.
       01 ws-num1 pic 9(2) value 20.
       01 ws-num2 pic 9(9) value 25.

       procedure division.
       a000-first-para.

           if not ws-num1 is less than ws-num2 then
             display 'if-block'
           else
             display 'else-block'
           end-if.

           stop run.
