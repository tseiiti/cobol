       identification division.
       program-id. data_types_02.

       data division.
       working-storage section.
       01 ws-num1 pic 99v9 value is 3.5.
       01 ws-name pic a(6) value 'abcd'.
       01 ws-id pic 99 value zero.

       procedure division.
           display 'ws-num1 : 'ws-num1.
           display 'ws-name : 'ws-name.
           display 'ws-id : 'ws-id.
           stop run.

