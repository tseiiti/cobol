       identification division.
       program-id. table_processing_06.

       data division.
       working-storage section.
       01 ws-table.
         05 ws-a pic x(1) occurs 18 times indexed by i.
       01 ws-srch pic a(1) value 'm'.

       procedure division.
           move 'abcdefghijklmnopqr' to ws-table.
           set i to 1.
           search ws-a
             at end display 'm not found in table'
             when ws-a(i) = ws-srch
               display 'letter m found in table'
           end-search.

           stop run.
