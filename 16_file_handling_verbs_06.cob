       identification division.
       program-id. file_handling_verbs_06.


       environment division.
       input-output section.
       file-control.
           select student assign to out1
             organization is indexed
             access is random
             record key is student-id
             file status is fs.

       data division.
       file section.
       fd student
       01 student-file.
          05 student-id pic 9(4).
          05 name pic a(12).
          05 class pic x(3).
       working-storage section.
       01 ws-student.
          05 ws-student-id pic 9(5).
          05 ws-name pic a(25).
          05 ws-class pic x(3).

       procedure division.
           open i-o student.
           move '1000' to student-id.

           delete student record
             invalid key display 'invalid key'
             not invalid key display 'record deleted'
           end-delete.

           close student.
           stop run.
