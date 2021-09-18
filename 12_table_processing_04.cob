       identification division.
       program-id. table_processing_04.

       data division.
       working-storage section.
       01 ws-table.
       05 ws-a occurs 3 times indexed by i.
       10 ws-b pic a(2).
       10 ws-c occurs 2 times indexed by j.
       15 ws-d pic x(3).

       procedure division.
           move '12abcdef34ghijkl56mnopqr' to ws-table.
           perform a-para varying i from 1 by 1 until i > 3.
           stop run.

       a-para.
           perform c-para varying j from 1 by 1 until j > 2.

       c-para.
           display ws-c(i, j).
