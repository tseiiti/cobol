       identification division.
       program-id. string_handling_01.

       data division.
       working-storage section.
       01 ws-cnt1 pic 9(2) value 0.
       01 ws-cnt2 pic 9(2) value 0.
       01 ws-string pic x(15) value 'abcdacdadeaaaff'.

       procedure division.
           inspect ws-string tallying ws-cnt1 for all characters.
           display 'ws-cnt1 : ' ws-cnt1.
           inspect ws-string tallying ws-cnt2 for all 'a'.
           display 'ws-cnt2 : ' ws-cnt2.
