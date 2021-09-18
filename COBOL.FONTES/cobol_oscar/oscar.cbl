      $SET OSVS
      *================================================================*
       IDENTIFICATION DIVISION.
      *================================================================*
      *      
       PROGRAM-ID.     OSCAR.
       AUTHOR.         OSCAR.
       DATE-WRITTEN.   16/11/09.
       DATE-COMPILED.
      *
       REMARKS.
      *
      *
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
      *
       CONFIGURATION   SECTION.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT   IS   COMMA.
      *
       INPUT-OUTPUT    SECTION.
      *
       FILE-CONTROL.
      *
      *      
      *================================================================*
	   DATA DIVISION.
      *================================================================*
      *
       FILE SECTION.
      *
              WORKING-STORAGE SECTION.
      *
      *      
       PROCEDURE       DIVISION.
       PRIMEIRO   SECTION.
      *
           DISPLAY
          '*=========================================================*'.
           DISPLAY    'hello world'
          '*=========================================================*'.
           STOP        RUN.
      *
       PRIMEIRO-EXIT. EXIT.
      *

