       identification division.
       program-id. table_processing_01.

       data division.
       working-storage section.
       01 ws-table.
         05 ws-a pic a(10) value 'tutorials' occurs 5 times.
           
       procedure division.
           display 'one-d table : ' ws-table.

           stop run.
