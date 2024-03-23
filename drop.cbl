       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLDROP.
       AUTHOR. ALAN BEADLE.
       DATE-WRITTEN 12/31/2022.
       REMARKS. NOT Y2.1K COMPLIANT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  CURROW            PIC 99.
       01  BALLROW           PIC 99.
       01  KEYIN             PIC X
              VALUE SPACE.
           88  QUIT VALUES 'Q' 'q'.
       01  WS-DATE.
           05  YR            PIC 99.
           05  MODA.
           10  MO            PIC 99.
           10  DA            PIC 99.
       01  WS-TIME.
           05 HRMIN.
           10  HR            PIC 99.
           10  MIN           PIC 99.
           05  SEC           PIC 99.
       01  WS-CLOCK.
           05  FILLER        PIC X(11)
              VALUE '  '.
           05  OUTHR         PIC Z9.
           05  FILLER        PIC X
              VALUE ':'.
           05  OUTMIN        PIC 99.
           05  FILLER        PIC X
              VALUE ':'.
           05  OUTSEC        PIC 99.
       01  WS-NEWYEAR.
           05  CENTURY       PIC 9B9B
              VALUE 20.
           05  YEAR-OUT      PIC 9B9.       

       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL QUIT
             ACCEPT WS-DATE FROM DATE
             ACCEPT WS-TIME FROM TIME
             EVALUATE MODA
               WHEN 1231
                 PERFORM 400-COUNTDOWN
               WHEN 0101
                 PERFORM 500-NEWYEAR
               WHEN OTHER
                 MOVE 3 TO BALLROW
             END-EVALUATE
             PERFORM 300-BALL
             DISPLAY ' (PRESS Q TO QUIT)'
             ACCEPT KEYIN WITH TIMEOUT 1
             END-ACCEPT
           END-PERFORM
           STOP RUN.

        300-BALL.
          PERFORM VARYING CURROW FROM 3 BY 1 UNTIL CURROW GREATER 15
            IF CURROW EQUAL BALLROW
              DISPLAY '( )' LINE CURROW COL 15
            ELSE
              DISPLAY ' | ' LINE CURROW COL 15
            END-IF
          END-PERFORM.

        400-COUNTDOWN.
           SUBTRACT HR FROM 23 GIVING OUTHR
           SUBTRACT MIN FROM 59 GIVING OUTMIN
           SUBTRACT SEC FROM 59 GIVING OUTSEC
           IF HRMIN EQUAL 2359
             COMPUTE BALLROW = 3 + (SEC + 1) / 5
           ELSE
             MOVE 3 TO BALLROW
           END-IF
           ADD 1 TO YR GIVING YEAR-OUT.
           DISPLAY WS-NEWYEAR LINE 16 COL 13.
           DISPLAY WS-CLOCK LINE 18.

        500-NEWYEAR.
           MOVE HR TO OUTHR
           MOVE MIN TO OUTMIN
           MOVE SEC TO OUTSEC
           MOVE 15 TO BALLROW
           MOVE YR TO YEAR-OUT.
           DISPLAY WS-NEWYEAR LINE 16 COL 13.
           DISPLAY WS-CLOCK LINE 18
           DISPLAY 'HAPPY NEW YEAR FROM COBOL' LINE 19 COL 4.
