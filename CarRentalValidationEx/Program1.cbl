       IDENTIFICATION DIVISION.
      * CarRentals.cbl
       PROGRAM-ID. RENTALS.
       AUTHOR. Dhanan Thannoo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RENTAL-FILE   ASSIGN TO 'rentals.dat'
                                ORGANIZATION IS LINE SEQUENTIAL.

           SELECT VALID-RENTAL-FILE  ASSIGN TO 'validrentals.dat'
                                ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-FILE
                                ASSIGN TO 'badrentals.dat'
                                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  RENTAL-FILE
           RECORD CONTAINS 56 CHARACTERS
           DATA RECORD IS RENTAL-RECORD.
       01 RENTAL-RECORD.
         05 REN-CONTRACT-NO PIC 9(6).
         05 REN-NAME.
           10 REN-LAST-NAME PIC X(15).
           10 REN-FIRST-NAME PIC X(10).
           10 REN-INITIAL PIC X.
         05 REN-RETURNED-DATE.
           10 REN-RETURNED-YEAR PIC 9(2).
           10 REN-RETURNED-MONTH PIC 9(2).
             88 VALID-MONTHS VALUES 1 THRU 12.
             88 FEBRUARY VALUE 2.
             88 30-DAY-MONTH VALUES 4 6 9 11.
             88 31-DAY-MONTH VALUES 1 3 5 7 8 10 12.
           10 REN-RETURNED-DAY PIC 9(2).
         05 REN-CAR-TYPE PIC X.
           88 VALID-CAR-TYPES VALUES 'E' 'C' 'M' 'F' 'L'.
         05 REN-DAYS-RENTED PIC 99.
           88 ZERO-DAYS-RENTED VALUE 0.
           88 VALID-DAYS-RENTED VALUES 1 THRU 35.
         05 REN-MILEAGE.
           10 REN-MILES-IN PIC 9(6).
           10 REN-MILES-OUT PIC 9(6).
           10 REN-MILEAGE-RATE PIC 99.
             88 VALID-MILEAGE-RATES VALUES 00 THRU 50.
         05 REN-INSURANCE PIC X.
           88 VALID-INSURANCE VALUES 'Y' 'N'.

       FD  VALID-RENTAL-FILE
           RECORD CONTAINS 56 CHARACTERS
           DATA RECORD IS VALID-RENTAL-RECORD.
       01 VALID-RENTAL-RECORD PIC X(56).

       FD  ERROR-FILE
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS ERROR-RECORD.
       01 ERROR-RECORD PIC X(132).

       WORKING-STORAGE SECTION.
       01 PROGRAM-SWITCHES.
         05 W01-DATA-REMAINS-SWITCH PIC X(3) VALUE 'YES'.
         05 W01-VALID-DATA-SWITCH PIC X(3) VALUE SPACES.

       01 VALIDATION-CONSTANTS-AND-CALCS.
         05 MILES-PER-DAY-FACTOR PIC 99 VALUE 10.
         05 EXPECTED-MILES PIC 9(6).
         05 ACTUAL-MILES PIC 9(6).

       01 ERROR-REASONS.
         05 NON-NUMERIC-CONTRACT-MSG PIC X(40) VALUE
                                     'NON-NUMERIC CONTRACT NUMBER'.
         05 LAST-NAME-MSG PIC X(40) VALUE 'MISSING LAST NAME'.
         05 FIRST-NAME-MSG PIC X(40) VALUE 'MISSING FIRST NAME'.
         05 INITIAL-MSG PIC X(40) VALUE 'NON ALPHABETIC INITIAL'.
         05 CAR-TYPE-MSG PIC X(40) VALUE
                         'CAR TYPE MUST BE:  E, C, M, F, OR L'.
         05 MONTH-MSG PIC X(40) VALUE 'MONTH MUST BE BETWEEN 1 AND 12'.
         05 DAY-MSG PIC X(40) VALUE 'INVALID DAY'.
         05 FUTURE-DATE-MSG PIC X(40) VALUE 'DATE HAS NOT YET OCCURRED'.
         05 NON-NUM-DAYS-RENTED-MSG PIC X(40) VALUE
                                    'DAYS RENTED MUST BE NUMERIC'.
         05 ZERO-DAYS-MSG PIC X(40) VALUE 'DAYS RENTED MUST BE > ZERO'.
         05 LEASING-MSG PIC X(40) VALUE
                        'DAYS RENTED MUST BE 1 THROUGH 35'.
         05 NON-NUM-MILES-IN-MSG PIC X(40) VALUE 'NON-NUMERIC MILES IN'.
         05 NON-NUM-MILES-OUT-MSG PIC X(40) VALUE
                                  'NON-NUMERIC MILES OUT'.
         05 LESS-THAN-MILES-MSG PIC X(40) VALUE
                                'MILEAGE IN LESS THAN MILEAGE OUT'.
         05 INVALID-MILES-MSG PIC X(40) VALUE
                             'ACTUAL MILES IS LESS THAN EXPECTED MILES'.
         05 NON-NUM-RATE-MSG PIC X(40) VALUE 'NON-NUMERIC MILEAGE RATE'.
         05 MILEAGE-RATE-MSG PIC X(40) VALUE
                             'MILEAGE RATE OUT OF RANGE'.
         05 INSURANCE-MSG PIC X(40) VALUE
                          'INSURANCE CODE MUST BE Y OR N'.

       01 TODAYS-DATE.
         05 TODAYS-YEAR PIC 99.
         05 TODAYS-MONTH PIC 99.
         05 TODAYS-DAY PIC 99.

       01 HEADING-ERROR-LINE-ONE.
         05 PIC X(26) VALUE SPACES.
         05 PIC X(19) VALUE 'ERROR REPORT AS OF '.
         05 HDG-DATE.
           10 HDG-MONTH PIC 99.
           10 PIC X VALUE '/'.
           10 HDG-DAY PIC 99.
           10 PIC X VALUE '/'.
           10 HDG-YEAR PIC 99.
         05 PIC X(79) VALUE SPACES.
    10
       01 HEADING-ERROR-LINE-TWO.
         05 FILLER PIC X(10) VALUE 'CONTRACT #'.
         05 FILLER PIC XX VALUE SPACES.
         05 FILLER PIC X(9) VALUE 'LAST NAME'.
         05 FILLER PIC X(8) VALUE SPACES.
         05 FILLER PIC X(21) VALUE 'ERROR MESSAGE & FIELD'.
         05 FILLER PIC X(21) VALUE SPACES.
         05 FILLER PIC X(8) VALUE 'CONTENTS'.
         05 FILLER PIC X(46) VALUE SPACES.

       01 ERROR-LINE.
         05 FILLER PIC XX VALUE SPACES.
         05 ERR-CONTRACT-NO PIC 9(6).
         05 FILLER PIC X(4) VALUE SPACES.
         05 ERR-LAST-NAME PIC X(15).
         05 FILLER PIC XX VALUE SPACES.
         05 ERR-MESSAGE PIC X(40).
         05 FILLER PIC XX VALUE SPACES.
         05 ERR-CONTENTS PIC X(23).
         05 FILLER PIC X(38) VALUE SPACES.

       01 ERROR-DETAILS.
         05 ERR-MILES-IN-OUT.
           10 FILLER PIC X(4) VALUE 'IN: '.
           10 ERR-MILES-IN PIC 9(6).
           10 FILLER PIC X(6) VALUE ' OUT: '.
           10 ERR-MILES-OUT PIC 9(6).
         05 ERR-RETURNED-DATE.
           10 ERR-RETURNED-MONTH-DAY.
             15 ERR-RETURNED-MONTH PIC 99.
             15 ERR-RETURNED-DAY PIC /99.
           10 ERR-RETURNED-YEAR PIC /99.
         05 ERR-EXPECTED-MILES.
           10 FILLER PIC X(6) VALUE 'DAYS: '.
           10 ERR-DAYS-RENTED PIC 99.
           10 FILLER PIC X(9) VALUE '  MILES: '.
           10 ERR-MILES PIC 9(6).
     
       PROCEDURE DIVISION.
           OPEN INPUT RENTAL-FILE
           OPEN OUTPUT VALID-RENTAL-FILE
           OPEN OUTPUT ERROR-FILE
           PERFORM 100-GET-TODAYS-DATE
           PERFORM 200-WRITE-ERROR-HEADINGS
           PERFORM 300-READ-RENTAL-RECORD
           PERFORM 400-PROCESS-RENTAL-RECORDS
             UNTIL W01-DATA-REMAINS-SWITCH = 'NO'
           CLOSE RENTAL-FILE
           CLOSE VALID-RENTAL-FILE
           CLOSE ERROR-FILE
           STOP RUN.

       100-GET-TODAYS-DATE.
      * Retrieve the system date and store it in TODAYS-DATE.
           ACCEPT TODAYS-DATE FROM DATE
           MOVE TODAYS-MONTH TO HDG-MONTH
           MOVE TODAYS-DAY TO HDG-DAY
           MOVE TODAYS-YEAR TO HDG-YEAR.

       200-WRITE-ERROR-HEADINGS.
           MOVE HEADING-ERROR-LINE-ONE TO ERROR-RECORD
           WRITE ERROR-RECORD
           MOVE SPACES TO ERROR-RECORD
           WRITE ERROR-RECORD
           MOVE HEADING-ERROR-LINE-TWO TO ERROR-RECORD
           WRITE ERROR-RECORD
           MOVE SPACES TO ERROR-RECORD
           WRITE ERROR-RECORD.

       300-READ-RENTAL-RECORD.
           READ RENTAL-FILE
               AT END
                   MOVE 'NO' TO W01-DATA-REMAINS-SWITCH
           END-READ.
       400-PROCESS-RENTAL-RECORDS.
      * Incoming records are assumed to be valid.
      * Needs to be reset for every record.
           MOVE 'YES' TO W01-VALID-DATA-SWITCH
           PERFORM 500-VALIDATE-RENTAL-RECORD
           PERFORM 600-WRITE-VALID-RECORD
           PERFORM 300-READ-RENTAL-RECORD.

       500-VALIDATE-RENTAL-RECORD.
           PERFORM 510-VALIDATE-CONTRACT-NO
           PERFORM 520-VALIDATE-NAME
           PERFORM 530-VALIDATE-CAR-TYPE
           PERFORM 540-VALIDATE-DATE-RETURNED
           PERFORM 550-VALIDATE-DAYS-RENTED
           PERFORM 560-VALIDATE-MILES-DRIVEN
           PERFORM 570-VALIDATE-MILEAGE-RATE
           PERFORM 580-VALIDATE-INSURANCE.

       510-VALIDATE-CONTRACT-NO.
           IF REN-CONTRACT-NO NOT NUMERIC
               MOVE NON-NUMERIC-CONTRACT-MSG TO ERR-MESSAGE
               MOVE REN-CONTRACT-NO TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
           END-IF.

       520-VALIDATE-NAME.
           IF REN-LAST-NAME = SPACES
               MOVE LAST-NAME-MSG TO ERR-MESSAGE
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           ELSE
      * Do a presence check on first name
               IF REN-FIRST-NAME = SPACES
                   MOVE FIRST-NAME-MSG TO ERR-MESSAGE
                   PERFORM 599-WRITE-ERROR-LINE
                   MOVE SPACES TO ERR-CONTENTS
               END-IF
           END-IF
           .

       530-VALIDATE-CAR-TYPE.
      * Code this paragraph
           IF NOT VALID-CAR-TYPES
               MOVE CAR-TYPE-MSG TO ERR-MESSAGE
               MOVE REN-CAR-TYPE TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           END-IF
           .

       540-VALIDATE-DATE-RETURNED.
      * Validate the month
      * Validate the day of the month. For February, validate to be <= 29. You do not have to account for leap years.
      * Validate returned date. It cannot be after today
           IF NOT VALID-MONTHS
               MOVE MONTH-MSG TO ERR-MESSAGE
               MOVE REN-RETURNED-MONTH TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           ELSE
              
               IF FEBRUARY AND REN-RETURNED-DAY > 29
                   MOVE DAY-MSG TO ERR-MESSAGE
                   MOVE REN-RETURNED-DAY TO ERR-RETURNED-DAY
                   MOVE REN-RETURNED-MONTH TO ERR-RETURNED-MONTH
                   MOVE ERR-RETURNED-MONTH-DAY TO ERR-CONTENTS
                   PERFORM 599-WRITE-ERROR-LINE
                   MOVE SPACES TO ERR-CONTENTS
               ELSE
                   
                   IF 30-DAY-MONTH AND REN-RETURNED-DAY > 30
                       MOVE DAY-MSG TO ERR-MESSAGE
                       MOVE REN-RETURNED-DAY TO ERR-RETURNED-DAY
                       MOVE REN-RETURNED-MONTH TO ERR-RETURNED-MONTH
                       MOVE ERR-RETURNED-MONTH-DAY TO ERR-CONTENTS
                       PERFORM 599-WRITE-ERROR-LINE
                       MOVE SPACES TO ERR-CONTENTS
                   ELSE
                       IF 31-DAY-MONTH AND REN-RETURNED-DAY > 31
                           MOVE DAY-MSG TO ERR-MESSAGE
                           MOVE REN-RETURNED-DAY TO ERR-RETURNED-DAY
                           MOVE REN-RETURNED-MONTH TO ERR-RETURNED-MONTH
                           MOVE ERR-RETURNED-MONTH-DAY TO ERR-CONTENTS
                           PERFORM 599-WRITE-ERROR-LINE
                           MOVE SPACES TO ERR-CONTENTS
                       ELSE
                           IF REN-RETURNED-DATE > TODAYS-DATE
                               MOVE FUTURE-DATE-MSG TO ERR-MESSAGE
                               MOVE REN-RETURNED-DAY TO ERR-RETURNED-DAY
                               MOVE REN-RETURNED-MONTH TO
                                 ERR-RETURNED-MONTH
                               MOVE REN-RETURNED-YEAR TO 
                               ERR-RETURNED-YEAR
                               MOVE ERR-RETURNED-DATE TO ERR-CONTENTS
                               PERFORM 599-WRITE-ERROR-LINE
                               MOVE SPACES TO ERR-CONTENTS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       550-VALIDATE-DAYS-RENTED.
      * Code a numeric check on days rented
      * Validate zero days rented and valid days rented
           IF REN-DAYS-RENTED NOT NUMERIC
               MOVE NON-NUM-DAYS-RENTED-MSG TO ERR-MESSAGE
               MOVE REN-DAYS-RENTED TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           ELSE
               IF ZERO-DAYS-RENTED
                   MOVE ZERO-DAYS-MSG TO ERR-MESSAGE
                   MOVE REN-DAYS-RENTED TO ERR-CONTENTS
                   PERFORM 599-WRITE-ERROR-LINE
                   MOVE SPACES TO ERR-CONTENTS
               ELSE
                   IF NOT VALID-DAYS-RENTED
                       MOVE LEASING-MSG TO ERR-MESSAGE
                       MOVE REN-DAYS-RENTED TO ERR-CONTENTS
                       PERFORM 599-WRITE-ERROR-LINE
                       MOVE SPACES TO ERR-CONTENTS
                   END-IF
               END-IF
           END-IF
           .

       560-VALIDATE-MILES-DRIVEN.
      * Code numeric checks on miles in and miles out
      * Code validation for:
      *      miles in must be greater than or equal to miles out
      *      actual miles must be greater than or equal to expected miles
      * Calculations
      *    COMPUTE EXPECTED-MILES ROUNDED = MILES-PER-DAY-FACTOR * REN-DAYS-RENTED
      *    COMPUTE ACTUAL-MILES ROUNDED = REN-MILES-IN - REN-MILES-OUT
           IF REN-MILES-IN IS NOT NUMERIC
               MOVE NON-NUM-MILES-IN-MSG TO ERR-MESSAGE
               MOVE REN-MILES-IN TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           ELSE 
               IF REN-MILES-OUT IS NOT NUMERIC
                   MOVE NON-NUM-MILES-OUT-MSG TO ERR-MESSAGE
                   MOVE REN-MILES-OUT TO ERR-CONTENTS
                   PERFORM 599-WRITE-ERROR-LINE
                   MOVE SPACES TO ERR-CONTENTS
               ELSE
                   IF REN-MILES-OUT > REN-MILES-IN
                       MOVE LESS-THAN-MILES-MSG TO ERR-MESSAGE
                       MOVE REN-MILES-IN TO ERR-MILES-IN
                       MOVE REN-MILES-OUT TO ERR-MILES-OUT
                       MOVE ERR-MILES-IN-OUT TO ERR-CONTENTS
                       PERFORM 599-WRITE-ERROR-LINE
                       MOVE SPACES TO ERR-CONTENTS
                   ELSE
                       COMPUTE EXPECTED-MILES ROUNDED =
                         MILES-PER-DAY-FACTOR * REN-DAYS-RENTED
                       COMPUTE ACTUAL-MILES ROUNDED = REN-MILES-IN -
                         REN-MILES-OUT
                       IF ACTUAL-MILES < EXPECTED-MILES
                           MOVE REN-DAYS-RENTED TO ERR-DAYS-RENTED
                           MOVE ACTUAL-MILES TO ERR-MILES
                           MOVE ERR-EXPECTED-MILES TO ERR-CONTENTS
                           PERFORM 599-WRITE-ERROR-LINE
                           MOVE SPACES TO ERR-CONTENTS
                       END-IF
                   END-IF
               END-IF
           END-IF
           .

       570-VALIDATE-MILEAGE-RATE.
      * Code a numeric check on mileage rate. Validate valid mileage rates.
           IF REN-MILEAGE-RATE IS NOT NUMERIC
               MOVE NON-NUM-RATE-MSG TO ERR-MESSAGE
               MOVE REN-MILEAGE-RATE TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           ELSE
               IF NOT VALID-MILEAGE-RATES
                   MOVE MILEAGE-RATE-MSG TO ERR-MESSAGE
                   MOVE REN-MILEAGE-RATE TO ERR-CONTENTS
                   PERFORM 599-WRITE-ERROR-LINE
                   MOVE SPACES TO ERR-CONTENTS
               END-IF
           END-IF
           .

       580-VALIDATE-INSURANCE.
      * Code this paragraph
           IF NOT VALID-INSURANCE
               MOVE INSURANCE-MSG TO ERR-MESSAGE
               MOVE REN-INSURANCE TO ERR-CONTENTS
               PERFORM 599-WRITE-ERROR-LINE
               MOVE SPACES TO ERR-CONTENTS
           END-IF
           .

       599-WRITE-ERROR-LINE.
           MOVE REN-CONTRACT-NO TO ERR-CONTRACT-NO
           MOVE REN-LAST-NAME TO ERR-LAST-NAME
           MOVE ERROR-LINE TO ERROR-RECORD
           WRITE ERROR-RECORD
           MOVE 'NO' TO W01-VALID-DATA-SWITCH.

       600-WRITE-VALID-RECORD.
           IF W01-VALID-DATA-SWITCH = 'YES'
               MOVE RENTAL-RECORD TO VALID-RENTAL-RECORD
               WRITE VALID-RENTAL-RECORD
           ELSE
      * The 2 lines below clear the error message line.  It is needed so that the error message field clears.
      * It is especially important when the current error is shorter in length than the previous one.
               MOVE SPACES TO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF.
