 IDENTIFICATION DIVISION.                               
 PROGRAM-ID. MODEMP.                                     
*                                                       
 ENVIRONMENT DIVISION.                                  
 INPUT-OUTPUT SECTION.                                  
 FILE-CONTROL.                                          
     SELECT EMP-FILE-IN ASSIGN TO 'EMP.DAT'                      
     ORGANIZATION IS SEQUENTIAL                         
     ACCESS MODE  IS SEQUENTIAL 
     FILE STATUS  IS WS-EMPIN-STAT.       
     SELECT WORK-FILE ASSIGN TO 'WRKFILE.DAT'.    
     SELECT EMPLOYEE-FILE ASSIGN TO 'EMPFILE.DAT'            
     ORGANIZATION IS SEQUENTIAL                         
     ACCESS MODE  IS SEQUENTIAL 
*     LABEL RECORDS STANDARD
*     BLOCK CONTAINS 0 RECORDS                         
      FILE STATUS  IS WS-EMP-STAT.   
     SELECT LOCATION-FILE ASSIGN TO 'LOC.DAT'            
     ORGANIZATION IS INDEXED                         
     ACCESS MODE  IS DYNAMIC
     RECORD KEY IS LOC-CODE
     ALTERNATE RECORD KEY IS LOC-CITY                         
     FILE STATUS  IS WS-LOC-STAT.      
     SELECT REPORT-FILE ASSIGN TO 'REPFILE.REP'         
     FILE STATUS  IS WS-REP-STAT.                 
*                                                     
 DATA DIVISION.                                       
 FILE SECTION.                                        
 FD  EMP-FILE-IN.                                     
 01  EMP-FILE-RECORD     PIC X(80).                      
 SD  WORK-FILE.                                       
 01  WORK-RECORD.  
     03  FILLER          PIC X(21).                                    
     03  WS-EMP-GROUP    PIC A(10).  
     03  FILLER          PIC X(15).
     03  WS-EMP-LOC      PIC A(03).                     
     03  FILLER          PIC X(45).                       
 FD  EMPLOYEE-FILE.
*     LABEL RECORDS STANDARD.                                    
*     RECORD CONTAINS 80 CHARACTERS. 
*     BLOCK CONTAINS 0 RECORDS.                  
 01 EMPLOYEE-RECORD.          
    03 EMP-CODE    PIC 9(06).  
    03 EMP-NAME    PIC X(15).   
    03 EMP-GROUP   PIC X(10).   
    03 EMP-DESG    PIC X(15).   
    03 EMP-LOC     PIC X(03).
    03 FILLER      PIC X(30) VALUE SPACES. 
 FD LOCATION-FILE.
*   LABEL RECORDS STANDARD
*    RECORD CONTAINS 80 CHARACTERS
*    BLOCK CONTAINS  0  RECORDS.
 01 LOCATION-REC.
    05   LOC-CODE          PIC A(03).
    05   LOC-ADDRESS1      PIC X(30).
    05   LOC-ADDRESS2      PIC X(30).
    05   LOC-CITY          PIC A(15).
    05   FILLER            PIC X(02) VALUE SPACES.
 FD REPORT-FILE.                                     
*    RECORD CONTAINS 80 CHARACTERS                   
*    RECORDING MODE IS F.                            
 01 REPORT-RECORD PIC X(80).
*                        
 WORKING-STORAGE SECTION.                            
 01  HEADING-LINE1.                                  
     03  FILLER      PIC X(27) VALUE SPACES.       
     03  FILLER      PIC X(26) VALUE 'CAPGEMINI GLOBAL SOLUTIONS'.       
     03  FILLER      PIC X(27) VALUE SPACES.             
 01  HEADING-LINE2.                                             
     03  FILLER       PIC X(18) VALUE SPACES.     
     03  FILLER       PIC X(18) VALUE 'GROUP WISE LIST OF'. 
     03  FILLER       PIC X(22) VALUE ' EMPLOYEES WORKING AT'.
     03  P-LOC-CODE   PIC X(03) VALUE SPACES.                   
     03  FILLER       PIC X(19) VALUE SPACES.  
 01  HEADING-LINE3.                                             
     03  FILLER       PIC X(07) VALUE 'GROUP: '.     
     03  P-GROUP      PIC A(10) VALUE SPACES.                    
     03  FILLER       PIC X(63) VALUE SPACES.                   
 01  HEADING-LINE4.                                            
     03  FILLER       PIC X(08) VALUE 'EMP CODE'. 
     03  FILLER       PIC X(14) VALUE SPACES.                   
     03  FILLER       PIC X(15) VALUE '     NAME      '. 
     03  FILLER       PIC X(14) VALUE SPACES.                 
     03  FILLER       PIC X(15) VALUE '   EMP DESG    '.
     03  FILLER       PIC X(14) VALUE SPACES.                  
 01  DETAIL-LINE.                                               
     03  P-EMP-CODE   PIC 9(06) VALUE ZERO.                   
     03  FILLER       PIC X(16) VALUE SPACES.                   
     03  P-NAME       PIC X(15) VALUE SPACES.                   
     03  FILLER       PIC X(14) VALUE SPACES.                   
     03  P-DESG       PIC A(15) VALUE SPACES.                   
     03  FILLER       PIC X(14) VALUE SPACES.                   
 01  WS-VARIABLES.                                             
     03  PAGE-COUNT       PIC 9(02)   VALUE ZERO.               
     03  LINE-COUNT       PIC 9(01)   VALUE 5.                  
     03  TEMP-LOC-CODE    PIC A(03)   VALUE SPACES.
     03  TEMP-GROUP       PIC A(05)   VALUE SPACES.
     03  GROUP-TOTALS     PIC 9(01)   VALUE ZERO.             
     03  EMP-COUNT1       PIC 9(01)   VALUE ZERO.
     03  LOC-TOTALS       PIC 9(01)   VALUE ZERO.                   
     03  EMP-COUNT2       PIC 9(01)   VALUE ZERO.
 01  WS-EMP-STAT          PIC X(02).                           
     88  OPEN-SUCCESS                 VALUE '00'.            
 01  WS-EMPIN-STAT        PIC X(02).                         
     88  OPEN-SUCCESS                 VALUE '00'. 
 01  WS-LOC-STAT          PIC X(02).                         
     88  OPEN-SUCCESS                 VALUE '00'.           
 01  WS-REP-STAT          PIC X(02).                           
     88  OPEN-SUCCESS                 VALUE '00'.            
 01  WS-FILE-FLAG         PIC X(01)   VALUE 'N'.             
     88  END-OF-FILE                  VALUE 'Y'. 
 01  WS-FILE-FLAG2        PIC X(01)   VALUE 'N'.             
     88  END-OF-FILE2                 VALUE 'Y'.             
 01  WS-REP-FLAG          PIC X(01)   VALUE 'N'.             
     88  END-OF-REPORT                VALUE 'Y'.             
 01  WS-TOT-EMP           PIC 9(02).  
 01  CITY-NAME            PIC A(15)   VALUE SPACES.
*                          
 PROCEDURE DIVISION.                                      
 MAIN-PARA.                                          
      PERFORM INIT-PARA                               
      PERFORM PROCESS-PARA  UNTIL  END-OF-FILE        
      PERFORM END-PARA                                
      STOP RUN.                                            
 INIT-PARA.                                          
      PERFORM SORT-PARA    
      OPEN INPUT EMP-FILE-IN                           
      OPEN INPUT EMPLOYEE-FILE  
      OPEN INPUT LOCATION-FILE                            
      OPEN OUTPUT REPORT-FILE                              
      DISPLAY WS-EMP-STAT                                  
      DISPLAY WS-EMPIN-STAT
      DISPLAY WS-LOC-STAT                                               
      PERFORM HEADING-PARA.
      PERFORM READ-PARA. 
 SORT-PARA.                             
       SORT WORK-FILE                          
         ON ASCENDING KEY WS-EMP-LOC         
          USING EMP-FILE-IN                   
         GIVING EMPLOYEE-FILE.               
 READ-PARA.                             
     ACCEPT CITY-NAME
     READ LOCATION-FILE NEXT                       
      AT END                                  
       MOVE 'Y' TO WS-FILE-FLAG2                  
      NOT AT END                              
       MOVE LOC-CODE TO TEMP-LOC-CODE                       
       ADD 1 TO LOC-TOTALS 
     END-READ.
     PERFORM READ-EMP-PARA.
 READ-EMP-PARA.
     READ EMPLOYEE-FILE
     AT END                                  
     MOVE 'Y' TO WS-FILE-FLAG                  
     NOT AT END  
     IF EMP-LOC = TEMP-LOC-CODE
        NEXT SENTENCE
     ELSE 
          PERFORM GROUP-CHANGE-PARA
          PERFORM LOC-CHANGE-PARA
     END-IF. 
     MOVE EMP-GROUP TO  P-GROUP 
     PERFORM PROCESS-PARA
     PERFORM READ-PARA.
 PROCESS-PARA.                            
     PERFORM MOVE-PARA
     PERFORM READ-EMP-PARA.                    
*     
 HEADING-PARA.                                                            
     WRITE REPORT-RECORD FROM HEADING-LINE1 AFTER PAGE    
     WRITE REPORT-RECORD FROM HEADING-LINE2               
     WRITE REPORT-RECORD FROM HEADING-LINE3.       
*      
 MOVE-PARA.                                           
     MOVE EMP-CODE  TO P-EMP-CODE                          
     MOVE EMP-NAME  TO P-NAME                        
     MOVE EMP-DESG  TO P-DESG                         
     WRITE REPORT-RECORD.
*             
 GROUP-CHANGE-PARA.    
     INITIALIZE DETAIL-LINE.
     MOVE EMP-GROUP TO TEMP-GROUP, P-GROUP.
 LOC-CHANGE-PARA.    
     INITIALIZE DETAIL-LINE.
     MOVE EMP-LOC    TO TEMP-LOC-CODE, P-LOC-CODE.
 END-PARA.                                
     CLOSE EMP-FILE-IN.
     CLOSE EMPLOYEE-FILE.                      
     CLOSE LOCATION-FILE.
     CLOSE REPORT-FILE.  
     DISPLAY 'REP WRITE FS=> ', WS-REP-STAT.
    
    
