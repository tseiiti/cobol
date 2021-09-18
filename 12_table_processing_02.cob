       identification division.
       program-id. table_processing_02.

       data division.
       working-storage section.
       01 ws-table.
       05 ws-a occurs 2 times.
       10 ws-b pic a(10) value ' tutorials'.
       10 ws-c occurs 2 times.
       15 ws-d pic x(6) value ' point'.

       procedure division.
           display 'two-d table : ' ws-table.

           stop run.
