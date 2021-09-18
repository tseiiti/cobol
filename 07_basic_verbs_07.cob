       identification division.
       program-id. basic_verbs_07.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9) value 5.
       01 ws-num2 pic 9(9) value 250.
       01 ws-numa pic 9(9) value 100.
       01 ws-numb pic 9(9) value 15.
       01 ws-numc pic 9(9).
       01 ws-rem pic 9(9).

       procedure division.
           divide ws-num1 into ws-num2.
           divide ws-numa by ws-numb giving ws-numc remainder ws-rem.

           display 'ws-num1     : ' ws-num1
           display 'ws-num2     : ' ws-num2
           display 'ws-numa     : ' ws-numa
           display 'ws-numb     : ' ws-numb
           display 'ws-numc     : ' ws-numc
           display 'ws-rem      : ' ws-rem

           stop run.
