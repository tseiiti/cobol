       identification division.
       program-id. condition_statements_04.

       data division.
       working-storage section.
		   01 ws-num1 pic x(9) value 'abcd '.
		   01 ws-num2 pic 9(9) value 123456789.

       procedure division.
       a000-first-para.
				   if ws-num1 is alphabetic then
				     display 'ws-num1 is alphabetic'.

				   if ws-num1 is numeric then
				     display 'ws-num1 is numeric'.

				   if ws-num2 is numeric then
				     display 'ws-num2 is numeric'.

           stop run.
