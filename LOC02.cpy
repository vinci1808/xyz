       FD LOCATION-FILE
          LABEL RECORDS STANDARD
          RECORD CONTAINS 80 CHARACTERS
          BLOCK CONTAINS  0  RECORDS.
       01 LOCATION-REC.
           05   LOC-CODE          PIC A(03).
           05   LOC-ADDRESS1      PIC X(30).
           05   LOC-ADDRESS2      PIC X(30).
           05   LOC-CITY          PIC A(15).
           05   FILLER            PIC X(02).
