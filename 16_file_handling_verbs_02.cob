       identification division.
       program-id. file_handling_verbs_02.

       environment division.
       input-output section.
       file-control.
       copy 16_book_fc_02.


       data division.
       file section.
       copy 16_book_fs_02.

       working-storage section.
       01 ws-eof pic a(1).

       procedure division.
       copy 16_book_pd_02.cob.

      * o eof inicializa vazio
           perform until ws-eof = 'y'
             read student
               at end move 'y' to ws-eof
               not at end display student-id ' ' name
             end-read
           end-perform.

      *fechou o arquivo
           close student.
           stop run.
