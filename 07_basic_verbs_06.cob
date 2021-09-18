       identification division.
       program-id. basic_verbs_06.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9) value 10.
       01 ws-num2 pic 9(9) value 9.
       01 ws-num3 pic 9(9) value 10.
       01 ws-numa pic 9(9) value 10.
       01 ws-numb pic 9(9) value 10.
       01 ws-numc pic 9(9) value 10.

       procedure division.
           multiply ws-num1 by ws-num2 ws-num3.
           multiply ws-numa by ws-numb giving ws-numc.

           display 'ws-num1     : ' ws-num1
           display 'ws-num2     : ' ws-num2
           display 'ws-num3     : ' ws-num3
           display 'ws-numa     : ' ws-numa
           display 'ws-numb     : ' ws-numb
           display 'ws-numc     : ' ws-numc

           stop run.
