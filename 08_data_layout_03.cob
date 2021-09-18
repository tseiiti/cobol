       identification division.
       program-id. data_layout_03.

       data division.
       working-storage section.
       01 ws-num1 pic s9(5)v9(3) usage is display.
       01 ws-num2 pic s9(10) usage is comp.

       procedure division.
           display 'ws-num1 : ' ws-num1.
           display 'ws-num2 : ' ws-num2.

           stop run.
