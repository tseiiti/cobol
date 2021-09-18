       identification division.
       program-id. data_layout_01.

       data division.
       working-storage section.
       01 ws-description.
       05 ws-date1 value '20140831'.
       10 ws-year pic x(4).
       10 ws-month pic x(2).
       10 ws-date pic x(2).
       05 ws-date2 redefines ws-date1 pic 9(8).

       procedure division.
           display 'ws-date1 : ' ws-date1.
           move '20150405' to ws-date1.
           display 'ws-date2 : ' ws-date2.

           stop run.
