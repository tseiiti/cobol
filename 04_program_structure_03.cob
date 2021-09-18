       identification division.
       program-id. program_structure_03.

       data division.
       working-storage section.
       01 ws-name pic a(30).
       01 ws-id pic 9(5) value 12345.

       procedure division.
       a000-first-para.
           display 'Hello World'.
      / teste de comentario
           move 'TutorialsPoint' to ws-name.
           display 'My name is : ' ws-name.
           display 'My Id is : ' ws-id.
           stop run.

