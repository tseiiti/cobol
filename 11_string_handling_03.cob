       identification division.
       program-id. string_handling_03.

       data division.
       working-storage section.
		   01 ws-string pic a(30).
		   01 ws-str1 pic a(15) value 'tutorialspoint'.
		   01 ws-str2 pic a(7) value 'welcome'.
		   01 ws-str3 pic a(7) value 'to and'.
		   01 ws-count pic 99 value 1.

       procedure division.
				   string ws-str2 delimited by size
				      ws-str3 delimited by space
				      ws-str1 delimited by size
				      into ws-string
				      with pointer ws-count
				      on overflow display 'overflow!'
				   end-string.

				   display 'ws-string : ' ws-string.
				   display 'ws-count : ' ws-count.

           stop run.
