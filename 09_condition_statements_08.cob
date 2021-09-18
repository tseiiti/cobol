       identification division.
       program-id. condition_statements_08.

       data division.
       working-storage section.
       01 ws-a pic 9 value 0.

       procedure division.
           move 3 to ws-a.

           evaluate true
              when ws-a > 2
                 display 'ws-a greater than 2'

              when ws-a < 0
                 display 'ws-a less than 0'

              when other
                 display 'invalid value of ws-a'
           end-evaluate.

           stop run.
