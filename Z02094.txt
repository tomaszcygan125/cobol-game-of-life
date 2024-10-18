       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02094.                                              
      ******************************************************************
      *                                                                 
      *                                                                 
      *   PROGRAM WILL ALLOW USER TO DEFINE SHAPES FOR GAME OF LIFE     
      *  PROGRAM                                                        
      *                                                                 
      *                                                                 
      ******************************************************************
      *                         CHANGE  LOG                             
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT E1DQ0010 ASSIGN TO E1DQ0010                           
            ORGANIZATION IS SEQUENTIAL                                  
            ACCESS MODE IS SEQUENTIAL                                   
            FILE STATUS IS WS-FS-E1DQ0010.                              
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
      * FILE DESCRIPTION                                                
       FD E1DQ0010                                                      
           RECORD CONTAINS 20 CHARACTERS                                
           RECORDING MODE F                                             
           DATA RECORD IS WS-E1DQ0010-REC.                              
       01 WS-E1DQ0010-REC.                                              
           05 WS-E1DQ0010-LINE          PIC X(20).                      
                                                                        
       WORKING-STORAGE SECTION.                                         
      *******************                                               
      *  ARRAY THAT STORES MOST IMPORTANT DATA                          
      *******************                                               
       01 WS-TABLE.    
            05 WS-LINE-OF-DATA PIC X(20) OCCURS 22 TIMES.               
                                                                        
      * SQLCA                                                           
           EXEC SQL INCLUDE SQLCA  END-EXEC.                            
      * SHAPER_TABLE'S DCLGEN                                           
           EXEC SQL INCLUDE SHAPER END-EXEC.                            
      ***************************************************************   
      *   ERROR HANDLING VARIABLES                                      
      ***************************************************************   
       01 SW-FILE-ERROR.                                                
           10 SW-FS-CURRENT                   PIC 99.                   
               88 SO-FILE-STATUS-OK           VALUE  00 10.             
           10 SW-FILE-NAME                    PIC X(8).                 
               88 SO-FILE-E1DQ0010            VALUE 'E1DQ0010'.         
           10 SW-FILE-OPERATION               PIC X(5).                 
               88 SO-FILE-OPERATION-OPEN      VALUE 'OPEN'.             
               88 SO-FILE-OPERATION-CLOSE     VALUE 'CLOSE'.            
               88 SO-FILE-OPERATION-READ      VALUE 'READ'.             
               88 SO-FILE-OPERATION-WRITE     VALUE 'WRITE'.            
      **********************************                                
      *      DB2 ERROR HANDLING VARIABLES*                              
      **********************************                                
       01 WS-DB2-ERROR.                                                 
           10 SW-SQLCODE                    PIC S9(5).                  
               88 SO-SQLCODE-OK             VALUE  000   100.           
           10 WS-SQLERRMC                   PIC X(70).                  
           10 WS-SQLCODE-FORMAT             PIC -(5).                   
           10 SW-ST-IDENTIFICATOR           PIC X(4).                   
               88 SO-7100-PARA              VALUE '7100'.               
      * FILE STATUS                                                     
       01 WS-FILE-STATUS.                                               
           05 WS-FS-E1DQ0010                  PIC 99.                   
      * VARIABLES.                                                      
       01 PROGRAM-VARIABLES.                                            
           05 WS-MAX-X                        PIC S9(9) COMP.           
           05 WS-MAX-Y                        PIC S9(9) COMP.    
      ********************                                              
      *   SWITCHES                                                      
      *******************                                               
       01 SW-SWITCHES.                                                  
           05 SW-E1DQ0010-END-OF-FILE         PIC X.                    
               88 SO-E1DQ0010-END-OF-FILE     VALUE 'Y'.                
               88 SO-E1DQ0010-NOT-END-OF-FILE VALUE 'N'.                
           05 SW-SO-22-RECORDS-READ           PIC X.                    
               88 SO-22-RECORDS-READ          VALUE 'Y'.                
               88 SO-NOT-22-RECORDS-READ      VALUE 'N'.                
           05 SW-SO-DATA-VALID                PIC X.                    
               88 SO-DATA-VALID               VALUE 'Y'.                
               88 SO-INVALID-DATA             VALUE 'N'.                
       01 WS-ITER     PIC S9(4) COMP VALUE 0.                           
       01 WS-ITER2    PIC S9(4) COMP VALUE 0.                           
       01 WS-TEMP-VAR PIC S9(4) COMP VALUE 0.                           
      ******************************************************************
      * PROCEDURE DIVISION                                              
      ******************************************************************
       PROCEDURE DIVISION.                                              
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                          1000-INIT                              
      ******************************************************************
       1000-INIT.                                                       
           PERFORM 1010-OPEN-FILE                                       
           PERFORM 1015-INITIALZIE-RECORD                               
           PERFORM 1020-SET-STARTING-FLAGS                              
           .                                                            
                                                                        
      ******************************************************************
      *                       1010-OPEN-FILE                            
      ******************************************************************
       1010-OPEN-FILE.                                                  
           OPEN INPUT E1DQ0010                                          
           MOVE WS-FS-E1DQ0010            TO SW-FS-CURRENT              
           SET SO-FILE-OPERATION-OPEN     TO TRUE                       
           SET SO-FILE-E1DQ0010           TO TRUE                       
           SET SO-INVALID-DATA            TO TRUE                       
           PERFORM   4000-CHECK-FOR-FILE-ERROR                          
           .                                                            
      ******************************************************************
      *                 1015-INITIALZIE-RECORD                          
      ******************************************************************
       1015-INITIALZIE-RECORD.                                          
           INITIALIZE WS-E1DQ0010-REC                                   
           .                                                            
      ******************************************************************
      *                1020-SET-STARTING-FLAGS                          
      ******************************************************************
       1020-SET-STARTING-FLAGS.                                         
           SET SO-E1DQ0010-NOT-END-OF-FILE TO TRUE                      
           SET SO-NOT-22-RECORDS-READ      TO TRUE                      
           SET SO-INVALID-DATA             TO TRUE                      
           .                                                            
      ******************************************************************
      *                       2000-PROCESS                              
      ******************************************************************
       2000-PROCESS.                                                    
                                                                        
           PERFORM 2100-READ-INPUT-22-TIMES                             
                                                                        
           PERFORM UNTIL SO-E1DQ0010-END-OF-FILE                        
                                                                        
             IF SO-22-RECORDS-READ THEN                                 
                                                                        
               PERFORM 2200-CHECK-IF-DATA-VALID                         
                                                                        
               IF SO-DATA-VALID   THEN                                  
                  DISPLAY 'DATA VALID'                                  
                  PERFORM 7100-PREPARE-AND-WRITE-RECORD                 
                                                                        
               ELSE                                                     
      * INVALID DATA                                                    
                  DISPLAY 'END DUE TO INVALID DATA'                     
                  PERFORM 3100-FINAL-WITH-ERROR                         
               END-IF                                                   
               PERFORM 2050-INITIALIZE-DATA                             
               PERFORM 2100-READ-INPUT-22-TIMES                         
                                                                        
             END-IF                                                     
           END-PERFORM                                                  
           .                                                            
       2050-INITIALIZE-DATA.                                            
           INITIALIZE DCLSHAPE-TABLE2                                   
           INITIALIZE WS-TABLE                                          
           .                                                            
      ******************************************************************
      *                 2100-READ-INPUT-22-TIMES                        
      ******************************************************************
       2100-READ-INPUT-22-TIMES.                                        
           SET SO-NOT-22-RECORDS-READ  TO TRUE                          
                                                                        
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 22       
           OR SO-E1DQ0010-END-OF-FILE                                   
                                                                        
             PERFORM 2110-READ-INPUT-FILE                               
                                                                        
             IF WS-ITER = 22 THEN                                       
                 SET SO-22-RECORDS-READ  TO TRUE                        
                 DISPLAY 'PRZECZYTANO 22 RAZY '                         
             END-IF                                                     
                                                                        
           END-PERFORM                                                  
           .                    
      ******************************************************************
      *                   2110-READ-INPUT-FILE                          
      ******************************************************************
       2110-READ-INPUT-FILE.                                            
                                                                        
           READ E1DQ0010                                                
           AT END                                                       
              SET SO-E1DQ0010-END-OF-FILE TO TRUE                       
           NOT AT END                                                   
      * SAVING RECORD WE JUST READ TO ARRAY                             
      * THIS ARRAY WILL BE LATER STORED IN SHAPE_TABLE TABLE            
                                                                        
              MOVE WS-E1DQ0010-LINE TO WS-LINE-OF-DATA(WS-ITER)         
           END-READ                                                     
                                                                        
           MOVE WS-FS-E1DQ0010            TO SW-FS-CURRENT              
           SET SO-FILE-OPERATION-READ     TO TRUE                       
           SET SO-FILE-E1DQ0010           TO TRUE                       
           PERFORM 4000-CHECK-FOR-FILE-ERROR                            
           .                                                            
      ******************************************************************
      *                     2200-CHECK-IF-DATA-VALID                    
      * WE NEED TO CHECK IF SHAPE_ID IS VALID                           
      * IF SHAPE_NAME IS NOT EMPTY                                      
      *                                                                 
      ******************************************************************
       2200-CHECK-IF-DATA-VALID.                                        
      * INITIALIZATION                                                  
           SET SO-INVALID-DATA TO TRUE                                  
                                                                        
                                                                        
      * ID OF SHAPE                                                     
           IF FUNCTION TEST-NUMVAL(WS-LINE-OF-DATA(1)) NOT  = 0 THEN    
              DISPLAY 'NON NUMERIC SHAPE ID '                           
              PERFORM 3100-FINAL-WITH-ERROR                             
           END-IF   
                                                                        
      * SHAPE NAME                                                      
           IF WS-LINE-OF-DATA(2) = SPACE OR LOW-VALUES                  
           THEN                                                         
              DISPLAY 'SHAPE_NAME IS EMPTY '                            
              PERFORM 3100-FINAL-WITH-ERROR                             
           END-IF                                                       
      *  IN OTHER CASE DATA IS VALID                                    
           SET SO-DATA-VALID TO TRUE                                    
           .                                                            
      ***************************************************************   
      *                2300-GET-DATA-AND-WRITE                          
      ***************************************************************   
       2300-GET-DATA-AND-WRITE.                                         
      * OUTSIDE LOOP GOES THRU THE LINES (Y)                            
      * INSIDE LOOP GOES THRU THE COLUMNS(X)                            
           PERFORM VARYING WS-ITER FROM 3 BY 1 UNTIL WS-ITER > 22       
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 20   
                  IF WS-LINE-OF-DATA(WS-ITER)(WS-ITER2:1) = 'X' OR      
                   'x'                                                  
                  THEN                                                  
                     MOVE WS-ITER TO WS-TEMP-VAR                        
                     SUBTRACT 2 FROM WS-TEMP-VAR                        
                                                                        
                     MOVE WS-TEMP-VAR TO POSITION-Y                     
                     MOVE WS-ITER2    TO POSITION-X                     
                     PERFORM 7200-WRITE-DB-RECORD                       
                     INITIALIZE WS-TEMP-VAR                             
                  END-IF                                                
             END-PERFORM                                                
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                      3000-FINAL                                 
      ******************************************************************
       3000-FINAL.
           DISPLAY '300-FINAL'                                          
           STOP RUN                                                     
           .                                                            
      ******************************************************************
      *                 3100-FINAL-WITH-ERROR                           
      ******************************************************************
       3100-FINAL-WITH-ERROR.                                           
           DISPLAY 'FINAL WITH ERROR'                                   
           STOP RUN                                                     
           .                                                            
      ******************************************************************
      *                 3500-DB2-ERROR-FINAL                            
      ******************************************************************
       3500-DB2-ERROR-FINAL.                                            
           DISPLAY ' END DUE TO DB2 ERROR'                              
           STOP RUN                                                     
           .                                                            
      ******************************************************************
      *                 3600-FILE-ERROR-EXIT                            
      ******************************************************************
       3600-FILE-ERROR-EXIT.                                            
           DISPLAY '3500 FILE ERROR FIANL'                              
           STOP RUN                                                     
           .                                                            
      *****************************************************             
      *                4000-CHECK-FOR-FILE-ERROR          *             
      *****************************************************             
       4000-CHECK-FOR-FILE-ERROR.                                       
           IF SO-FILE-STATUS-OK                                         
               CONTINUE                                                 
           ELSE                                                         
               DISPLAY 'FILE ERROR'                                     
               DISPLAY 'IN FILE: '     SW-FILE-NAME                     
               DISPLAY 'OPERATION: '   SW-FILE-OPERATION                
               DISPLAY 'FILE STATUS: ' SW-FS-CURRENT                    
               MOVE 8  TO RETURN-CODE         
               PERFORM 3600-FILE-ERROR-EXIT                             
           END-IF                                                       
           .                                                            
      ***************************************************************** 
      *                   7100-PREPARE-AND-WRITE-RECORD                 
      ***************************************************************** 
       7100-PREPARE-AND-WRITE-RECORD.                                   
      * SHAPE ID                                                        
           COMPUTE SHAPE-ID  = FUNCTION NUMVAL(WS-LINE-OF-DATA(1))      
      * SHAPE NAME                                                      
           MOVE WS-LINE-OF-DATA(2) TO SHAPE-NAME                        
                                                                        
           DISPLAY 'PRZYGOTOWANO ZACZNAM 2300 GET AND WRITE'            
                                                                        
           PERFORM 2300-GET-DATA-AND-WRITE                              
           .                                                            
      ***************************************************************** 
      *                     7200-WRITE-DB-RECORD                        
      ***************************************************************** 
       7200-WRITE-DB-RECORD.                                            
           EXEC SQL                                                     
           INSERT INTO                                                  
           SHAPE_TABLE2(SHAPE_ID, SHAPE_NAME, POSITION_X, POSITION_Y)   
           VALUES(                                                      
           :SHAPE-ID,                                                   
           :SHAPE-NAME,                                                 
           :POSITION-X,                                                 
           :POSITION-Y)                                                 
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF SO-SQLCODE-OK THEN                                        
             CONTINUE                                                   
           ELSE                                                         
             PERFORM 9000-DB2-ERROR                                     
           END-IF   
           .                                                            
      ***************************************************************** 
      *                                                                 
      *                    9000-DB2-ERROR                               
      *                                                                 
      ***************************************************************** 
      *                                                                 
       9000-DB2-ERROR.                                                  
           DISPLAY 'DB2 ERROR'                                          
           MOVE      SW-SQLCODE TO WS-SQLCODE-FORMAT                    
           DISPLAY 'SQLCODE '      WS-SQLCODE-FORMAT                    
           DISPLAY 'SQLERRMC '     SQLERRMC                             
           DISPLAY 'ST. IDENTIFICATOR ' SW-ST-IDENTIFICATOR             
           PERFORM 9100-ROLLBACK                                        
           MOVE 12 TO RETURN-CODE                                       
           PERFORM 3500-DB2-ERROR-FINAL                                 
           .                                                            
      ***************************************************************** 
      *                                                                 
      *                    9100-ROLLBACK                                
      *                                                                 
      ***************************************************************** 
       9100-ROLLBACK.                                                   
           EXEC SQL                                                     
           ROLLBACK                                                     
           END-EXEC                                                     
           .                                                                                                                
                          
                                                      
                                                    
                                        


       
                                                 
