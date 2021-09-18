       identification division.
       program-id. basic_verbs_08.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9) value 10.
       01 ws-num2 pic 9(9) value 10.
       01 ws-num3 pic 9(9) value 10.
       01 ws-numa pic 9(9) value 50.
       01 ws-numb pic 9(9) value 10.
       01 ws-numc pic 9(9) value 10.

       procedure division.
           divide ws-num1 into ws-num2.
           compute ws-numc = (ws-num1 * ws-num2) 
                           - (ws-numa/ws-numb)
                           + ws-num3
           .

           display 'ws-num1     : ' ws-num1
           display 'ws-num2     : ' ws-num2
           display 'ws-num3     : ' ws-num3
           display 'ws-numa     : ' ws-numa
           display 'ws-numb     : ' ws-numb
           display 'ws-numc     : ' ws-numc

           stop run.
