       identification division.
       program-id. table_processing_05.

       data division.
       working-storage section.
       01 ws-table.
       05 ws-a occurs 3 times indexed by i.
       10 ws-b pic a(2).
       10 ws-c occurs 2 times indexed by j.
       15 ws-d pic x(3).

       procedure division.
           move '12abcdef34ghijkl56mnopqr' to ws-table.
           set i j to 1.
           display ws-c(i,j).
           set i j up by 1.
           display ws-c(i,j).

           stop run.
