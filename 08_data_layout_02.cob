       identification division.
       program-id. data_layout_02.

       data division.
       working-storage section.
       01 ws-description.
       05 ws-num.
       10 ws-num1 pic 9(2) value 20.
       10 ws-num2 pic 9(2) value 56.
       05 ws-char.
       10 ws-char1 pic x(2) value 'aa'.
       10 ws-char2 pic x(2) value 'bb'.
       66 ws-rename renames ws-num2 thru ws-char2.

       procedure division.
           display 'ws-rename : ' ws-rename.

           stop run.
