       identification division.
       program-id. basic_verbs_05.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9) value 10.
       01 ws-num2 pic 9(9) value 10.
       01 ws-num3 pic 9(9) value 100.
       01 ws-num4 pic 9(9) value 100.
       01 ws-numa pic 9(9) value 10.
       01 ws-numb pic 9(9) value 10.
       01 ws-numc pic 9(9) value 10.
       01 ws-numd pic 9(9) value 100.
       01 ws-nume pic 9(9) value 10.

       procedure division.
           subtract ws-num1 ws-num2 from ws-num3 ws-num4.
           subtract ws-numa ws-numb ws-numc from ws-numd giving ws-nume.

           display 'ws-num1     : ' ws-num1
           display 'ws-num2     : ' ws-num2
           display 'ws-num3     : ' ws-num3
           display 'ws-num4     : ' ws-num4
           display 'ws-numa     : ' ws-numa
           display 'ws-numb     : ' ws-numb
           display 'ws-numc     : ' ws-numc
           display 'ws-numd     : ' ws-numd
           display 'ws-nume     : ' ws-nume

           stop run.

