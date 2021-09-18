       identification division.
       program-id. basic_verbs_03.

       data division.
       working-storage section.
       01 ws-num1 pic 9(9).
       01 ws-num2 pic 9(9).
       01 ws-num3 pic 9(5).
       01 ws-num4 pic 9(6).
       01 ws-address.
       05 ws-house-number pic 9(3).
       05 ws-country pic x(5).
       05 ws-pincode pic 9(6).
       01 ws-address1.
       05 ws-house-number1 pic 9(3).
       05 ws-country1 pic x(5).
       05 ws-pincode1 pic 9(6).

       procedure division.
       a000-first-para.
           move 123456789 to ws-num1.
           move ws-num1 to ws-num3 ws-num2.
           move ws-num1(3:6) to ws-num4.
           move 123 to ws-house-number.
           move 'india' to ws-country.
           move 112233 to ws-pincode.
           move ws-address to ws-address1.

           display 'ws-num1     : 'ws-num1.
           display 'ws-num2     : 'ws-num2.
           display 'ws-num3     : 'ws-num3.
           display 'ws-num4     : 'ws-num4.
           display 'ws-address  : 'ws-address.
           display 'ws-address1 : 'ws-address1.

           stop run.

