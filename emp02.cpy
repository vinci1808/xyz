       FD EMPLOYEE-FILE
          LABEL RECORDS STANDARD
          RECORD CONTAINS 80 CHARACTERS
          BLOCK CONTAINS 0 RECORDS.
       01 EMPLOYEE-REC.
            05  EMP-CODE            PIC 9(06).
            05  EMP-NAME            PIC X(15).
            05  EMP-GROUP           PIC A(10).
            05  EMP-DESG            PIC A(15).
            05  EMP-LOC             PIC A(03).
            05  FILLER              PIC X(31).  
