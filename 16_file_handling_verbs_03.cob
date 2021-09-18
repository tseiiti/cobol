       identification division.
       program-id. file_handling_verbs_03.

       environment division.
       input-output section.
       file-control.
           select student assign to in1
             organization is indexed
             access is random
             record key is student-id
             file status is fs.

       data division.
       file section.
       fd student.
       01 student-file.
		       05 student-id pic 9(5).
		       05 name pic a(25).

       working-storage section.
       01 ws-student.
		       05 ws-student-id pic 9(5).
		       05 ws-name pic a(25).

       procedure division.
           open input student.
           move 20005 to student-id.

           read student record into ws-student-file
             key is student-id
             invalid key display 'invalid key'
             not invalid key display ws-student-file
           end-read.

           close student.
           stop run.
