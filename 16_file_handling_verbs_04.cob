       identification division.
       program-id. file_handling_verbs_04.

       environment division.
       input-output section.
       file-control.
           select student assign to out1
             organization is sequential
             access is sequential
             file status is fs.

       data division.
       file section.
       fd student
       01 student-file.
          05 student-id pic 9(5).
          05 name pic a(25).
          05 class pic x(3).

       working-storage section.
       01 ws-student.
          05 ws-student-id pic 9(5).
          05 ws-name pic a(25).
          05 ws-class pic x(3).

       procedure division.
           open extend student.
           move 1000 to student-id.
           move 'tim' to name.
           move '10' to class.
           write student-file
           end-write.
           close student.
           stop run.
