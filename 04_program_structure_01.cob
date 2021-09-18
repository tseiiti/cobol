       identification division.
       program-id. program_structure_01.

       data division.
       working-storage section.
       01 ws-id pic xx.

       procedure division.

       a0000-first-para section.

       first-paragraph.
       accept ws-id
       move '10' to ws-id
           display ws-id
           .

