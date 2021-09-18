       identification division.
       program-id. basic_verbs_01.

       data division.
       working-storage section.
       01 ws-student-name pic x(25).
       01 ws-date pic x(10).

       procedure division.
           accept ws-student-name.
           accept ws-date from date.
           display 'name : 'ws-student-name.
           display 'date : 'ws-date.

           stop run.

