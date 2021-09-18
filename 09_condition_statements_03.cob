       identification division.
       program-id. condition_statements_03.

       data division.
       working-storage section.
		   01 ws-num1 pic s9(9) value -1234.
		   01 ws-num2 pic s9(9) value 123456.

       procedure division.
       a000-first-para.
				   if ws-num1 is positive then
				     display 'ws-num1 is positive'.

				   if ws-num1 is negative then
				     display 'ws-num1 is negative'.

				   if ws-num1 is zero then
				     display 'ws-num1 is zero'.

				   if ws-num2 is positive then
				     display 'ws-num2 is positive'.

           stop run.
