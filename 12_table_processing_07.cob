       identification division.
       program-id. table_processing_07.

       data division.
       working-storage section.
       01 ws-table.
         05 ws-record occurs 10 times 
           ascending key is ws-num indexed by i.
       10 ws-num pic 9(2).
       10 ws-name pic a(3).
           
       procedure division.
           move '12abc56def34ghi78jkl93mno11pqr' to ws-table.
           search all ws-record
             at end display 'record not found'
             when ws-num(i) = 93
               display 'record found '
               display ws-num(i)
               display ws-name(i)
           end-search.

