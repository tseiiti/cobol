       identification division.
       program-id. condition_statements_07.

       data division.
       working-storage section.
       01 ws-num1 pic 9(2) value 20.
       01 ws-num2 pic 9(2) value 25.
       01 ws-num3 pic 9(2) value 20.

       procedure division.
       a000-first-para.

           if ws-num1 is less than ws-num2 and ws-num1=ws-num3 then
             display 'both condition ok'
           else
             display 'error'
           end-if.

           stop run.
