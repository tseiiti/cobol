       identification division.
       program-id. string_handling_04.

       data division.
       working-storage section.
       01 ws-string pic a(30) value 'welcome to tutorialspoint'.
       01 ws-str1 pic a(7).
       01 ws-str2 pic a(2).
       01 ws-str3 pic a(15).
       01 ws-count pic 99 value 1.

       procedure division.
           unstring ws-string delimited by space
              into ws-str1, ws-str2, ws-str3
           end-unstring.

           display 'ws-str1 : ' ws-str1.
           display 'ws-str2 : ' ws-str2.
           display 'ws-str3 : ' ws-str3.

           stop run.
