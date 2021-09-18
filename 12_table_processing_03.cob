       identification division.
       program-id. table_processing_03.

       data division.
       working-storage section.
       01 ws-table.
       05 ws-a occurs 3 times.
       10 ws-b pic a(2).
       10 ws-c occurs 2 times.
       15 ws-d pic x(3).

       procedure division.
           move '12abcdef34ghijkl56mnopqr' to ws-table.
           display 'ws-table  : ' ws-table.
           display 'ws-a(1)   : ' ws-a(1).
           display 'ws-c(1,1) : ' ws-c(1,1).
           display 'ws-c(1,2) : ' ws-c(1,2).
           display 'ws-a(2)   : ' ws-a(2).
           display 'ws-c(2,1) : ' ws-c(2,1).
           display 'ws-c(2,2) : ' ws-c(2,2).
           display 'ws-a(3)   : ' ws-a(3).
           display 'ws-c(3,1) : ' ws-c(3,1).
           display 'ws-c(3,2) : ' ws-c(3,2).

           stop run.
