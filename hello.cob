       identification division.
       program-id. hello.

       environment division.
       source-computer. xxx-zos.
       object-computer. xxx-zos.

       input-output section.
       file-control.
           select file assign to ddname
             organization is sequential.
           
       data division.

       working-storage section.
       01 ws-id pic 9(5).
       01 ws-a pic 9 value 0.

       procedure division.
           display "Hello World!" with no advancing
           display " Teste".

       a0000-first-para section.
       first-paragraph.
           accept ws-id
           move '10' to ws-id
           display ws-id.

       a-para.
           perform b-para varying ws-a from 1 by 1 until ws-a = 5.
           stop run.

       b-para.
           display 'in b-para ' ws-a.
