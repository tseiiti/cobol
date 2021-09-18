       identification division.
       program-id. data_types_01.

       data division.
       working-storage section.
       01 ws-num1 pic s9(3)v9(2).
       01 ws-num2 pic ppp999.
       01 ws-num3 pic s9(3)v9(2) value -123.45.
       01 ws-name pic a(6) value 'abcdef'.
       01 ws-id pic x(5) value 'a121$'.

       procedure division.
           display 'ws-num1 : 'ws-num1.
           display 'ws-num2 : 'ws-num2.
           display 'ws-num3 : 'ws-num3.
           display 'ws-name : 'ws-name.
           display 'ws-id : 'ws-id.
           stop run.

