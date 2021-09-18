       identification division.
       program-id. data_layout_04.

       data division.
       working-storage section.
       copy 08_book_04.

       procedure division.
           display 'ws-num : ' ws-num.
           display 'ws-char : ' ws-char.

           stop run.
