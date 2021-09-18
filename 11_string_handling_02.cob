       identification division.
       program-id. string_handling_02.

       data division.
       working-storage section.
       01 ws-string pic x(15) value 'abcdacdadeaaaff'.

       procedure division.
           display 'old string : ' ws-string.
           inspect ws-string replacing all 'a' by 'x'.
           display 'new string : ' ws-string.

           stop run.
