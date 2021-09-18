       identification division.
       program-id. program_structure_02.

       environment division.

       input-output section.
      * file-control.
      *     select filen assign to input.
      *     organization is sequential.
      *     access is sequential.

       data division.
      * file section.
      * fd filen
      * 01 name pic a(25).

       working-storage section.
       01 ws-student pic a(30).
       01 ws-id pic xx.

       local-storage section.
       01 ls-class pic 9(3).

       linkage section.
       01 ls-id pic 9(5).

       procedure division.
           display 'executing cobol program using jcl'.
           stop run.

