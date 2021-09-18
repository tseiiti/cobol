       identification division.
       program-id. basic_verbs_02.

       data division.
       working-storage section.
       01 ws-name pic a(30) value 'abcdef'.
       01 ws-id pic 9(5).
       01 ws-address.
       05 ws-house-number pic 9(3).
       05 ws-country pic x(15).
       05 ws-pincode pic 9(6) value 123456.

       procedure division.
       a000-first-para.
           initialize ws-name, ws-address.
           initialize ws-id replacing numeric data by 12345.
           display 'my name is   : 'ws-name.
           display 'my id is     : 'ws-id.
           display 'address      : 'ws-address.
           display 'house number : 'ws-house-number.
           display 'country      : 'ws-country.
           display 'pincode      : 'ws-pincode.

           stop run.

