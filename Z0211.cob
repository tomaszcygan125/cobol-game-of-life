       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02112T.                                             
      ******************************************************************
      *                                                                 
      * THIS PROGRAM IS A ROUTINE TO Z02101 (0206)                      
      * AND WILL PERFORM FEW TASKS DEPENDING ON USER CHOICE             
      * 1. DISPLAYING A MAP                                             
      * 2. ADDING A SHAPE TO MAP                                        
      * 3. RESETING THE MAP                                             
      * 4. PLAYING THE GAME (1000 ITERATIONS 100 MS DELEAY)             
      *                                                                 
      ******************************************************************
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
      * CONSTATNS.                                                      
       01 CT-CONTSTANT.                                                 
           05 CT-MAXIMUM-WIDTH-OF-SCREEN  PIC S9(4) COMP VALUE 80.      
           05 CT-MAXIMUM-HEIGHT-OF-SCRREN PIC S9(4) COMP VALUE 25.      
           05 CT-LOCK-INDICATOR           PIC X VALUE 'Y'.              
           05 CT-UNLOCK-INDICATOR         PIC X VALUE 'N'.              
           05 CT-NUMBER-OF-ITERATIONS     PIC S9(4) COMP VALUE 100.     
      * SWITCHES                                                        
       01 SW-SWITCHES.                                                  
           05 SW-SHAPE-FOUND                       PIC X.               
               88 SO-SHAPE-EXISTS                  VALUE 'Y'.           
               88 SO-SHAPE-DONT-EXISTS             VALUE 'N'.           
           05 SW-IF-SHAPE-CAN-BE-PLACED            PIC X.               
               88 SO-CAN-BE-PLACED                 VALUE 'Y'.           
               88 SO-CANT-BE-PLACED                VALUE 'N'.           
           05 SW-END-OF-DATA-SHAPE-TABLE           PIC X.               
               88 SO-END-OF-DATA-SHAPE-TABLE       VALUE 'Y'.           
               88 SO-NOT-END-OF-DATA-SHAPE-TABLE   VALUE 'N'.           
           05 SW-WHAT-TO-DO-WITH-CELL              PIC X.               
               88 SO-KILL-THE-CELL                 VALUE 'A'.           
               88 SO-CREATE-NEW-CELL               VALUE 'B'.           
               88 SO-DO-NOTHING                    VALUE 'C'.           
           05 SW-FIRST-CHANGE                      PIC X.               
               88 SO-FIRST-CHANGE                  VALUE '1'.           
               88 SO-SECOND-CHANGE                 VALUE '2'.           
               88 SO-THIRD-CHANGE                  VALUE '3'.           
               88 SO-LAST-CHANGE                   VALUE '4'.           
               88 SO-NOT-FIRST-CHANGE              VALUE 'N'.           
           05 SW-NO-ACTIVE-CELLS                   PIC X.               
               88 SO-NO-ACTIVE-CELLS               VALUE 'Y'.           
               88  SO-THERE-ARE-ACTIVE-CELLS       VALUE 'N'.           
           05 SW-IF-RESOURCES-LOCKED               PIC X.               
               88 SO-OTHERS-CAN-PLAY               VALUE 'N'.           
               88 SO-RESOURCES-ARE-LOCKED          VALUE 'Y'.           
           05 SW-IF-INPUT-DATA-IS-VALID            PIC X.               
               88 SO-VALID-INPUT-DATA              VALUE 'Y'.           
               88 SO-INVALID-INPUT-DATA            VALUE 'N'.           
      * COMMAREA                                                        
           COPY ZZEC0210.                                               
                                                                        
      **********************************                                
      *      DB2 ERROR HANDLING VARIABLES*                              
      **********************************                                
       01 WS-DB2-ERROR.                                                 
           10 SW-SQLCODE                    PIC S9(5).                  
               88 SO-SQLCODE-OK             VALUE  000   100.           
               88 SO-SQLCODE-NORMAL         VALUE 000.                  
               88 SO-SQLCODE-NOT-FOUND      VALUE 100.                  
           10 WS-SQLERRMC                   PIC X(70).                  
           10 WS-SQLCODE-FORMAT             PIC -(5).                   
           10 SW-ST-IDENTIFICATOR           PIC X(4).                   
               88 SO-7100-PARA              VALUE '7100'.               
               88 SO-7200-PARA              VALUE '7200'.               
               88 SO-7300-PARA              VALUE '7300'.               
               88 SO-7400-PARA              VALUE '7400'.               
               88 SO-7500-PARA              VALUE '7500'.               
               88 SO-7600-PARA              VALUE '7600'.               
               88 SO-7700-PARA              VALUE '7700'.               
               88 SO-7800-PARA              VALUE '7800'.          
               88 SO-7900-PARA              VALUE '7900'.          
                                                                   
      * PROGRAM VARIABLES.                                         
       01 PROGRAM-VARIABLES.                                       
           05 WS-MAP.                                              
               10 WS-MAP-LINE PIC X(79) OCCURS 24 TIMES.           
           05 WS-ITER             PIC S9(4) COMP.                  
           05 WS-ITER2            PIC S9(4) COMP.                  
           05 WS-ITER3            PIC S9(4) COMP.                  
           05 WS-ITER4            PIC S9(4) COMP.                  
           05 WS-ITER5            PIC S9(4) COMP.                  
           05 WS-Y-VALUE          PIC S9(4) COMP.                  
           05 WS-X-VALUE          PIC S9(4) COMP.                  
           05 WS-MAX-FROM-SHAPE.                                   
               10 WS-MAX-POS-OF-X PIC S9(9) COMP.                  
               10 WS-MAX-POS-OF-Y PIC S9(9) COMP.                  
           05 WS-HOW-MANY-SECONDS PIC S9(8) COMP VALUE 1000.       
           05 WS-COUNT-NEIGHBORS  PIC S9(4) COMP VALUE 0.          
           05 WS-NEIGHBORS-TOTAL  PIC S9(4) COMP VALUE 0.          
                                                                   
      * MAP COPYBOOK                                               
           COPY ZZMP0211.                                          
                                                                   
      * SQLCA AND DCLGENS                                          
           EXEC SQL INCLUDE SQLCA END-EXEC.                        
           EXEC SQL INCLUDE Z1EC0211 END-EXEC.                     
           EXEC SQL INCLUDE Z2EC0211 END-EXEC.                     
           EXEC SQL INCLUDE Z3EC0211 END-EXEC.                     
           EXEC SQL INCLUDE Z4EC0211 END-EXEC.                     
      * CURSOR DECLARATIONS                                        
           EXEC SQL DECLARE C-NAME CURSOR FOR                      
           SELECT                                                  
           POSITION_X, POSITION_Y                                  
           FROM SHAPE_TABLE3                                       
           WHERE SHAPE_ID = :SHAPE-ID                              
           END-EXEC.                                                   
       LINKAGE SECTION.                                                
       01 DFHCOMMAREA PIC X(108).                                      
      *****************************************************************
      *                   PROCEDURE DIVISION                           
      *****************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA.                           
      *TEST                                                            
           DISPLAY '-----------------------Z02112T--START---------'    
           PERFORM 1000-INIT                                           
           PERFORM 2000-PROCESS                                        
           DISPLAY '-----------------------Z02112T----END-------------'
           PERFORM 3000-FINAL                                          
           .                                                           
      *****************************************************************
      *                      1000-INIT                                 
      *****************************************************************
       1000-INIT.                                                      
           PERFORM 1005-IGNORE-CONDITION                               
           PERFORM 7700-CHECK-CONCURRENCY                              
                                                                       
           IF SO-OTHERS-CAN-PLAY THEN                                  
                                                                       
      * IF NO ONE USES ROUTINE  RIGHT NOW  WE ARE LOCKING ACCESS       
      * ONLY FOR US                                                    
             PERFORM 7800-LOCK-THE-RESOURCES                           
           ELSE                                                        
      * SOMEONE USES TRANASACTION DATA RIGHT NOW                       
      * SO WE CANNOT CHANGE ANYTHING                                   
      *                                                                
      * ROUTINE WILL SET PARTICULAR FLAG AND CONTROL WILL BE RETURED   
      * TO CALLING PROGRAM                                             
             PERFORM 3020-FINAL-NO-CONCURRENCY                         
                                                                       
           END-IF
      * SETTING OTHER ERROR TO TRUE                                     
                                                                        
           MOVE DFHCOMMAREA TO WS-ZZEC0210                              
           SET  ZZEC0210-O-RC-OTHER-ERROR TO TRUE                       
           MOVE 'UNKNOWN ERROR ' TO  ZZEC0210-O-ERROR-MESSAGE           
           SET SO-THERE-ARE-ACTIVE-CELLS TO TRUE                        
           .                                                            
      ******************************************************************
      *                 1005-IGNORE-CONDITION                           
      ******************************************************************
       1005-IGNORE-CONDITION.                                           
      *TEST                                                             
      *    DISPLAY ' 1005-IGNORE-CONDITION '                            
      */TEST                                                            
           EXEC CICS                                                    
           IGNORE CONDITION ERROR                                       
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                       2000-PROCESS                              
      ******************************************************************
       2000-PROCESS.                                                    
      *TEST                                                             
      *    DISPLAY ' 2000-PROCESS '                                     
      *                                                                 
      */TEST                                                            
           EVALUATE TRUE                                                
            WHEN ZZEC0210-M-DISPLAY-MAP                                 
                 PERFORM 2110-DISPLAY-MAP                               
            WHEN ZZEC0210-M-ADD-A-SHAPE                                 
                 PERFORM 2120-ADD-A-SHAPE                               
            WHEN ZZEC0210-M-RESET-THE-GAME                              
                 PERFORM 2130-RESET-THE-GAME                            
            WHEN ZZEC0210-M-START-THE-GAME                              
                 PERFORM 2140-START-THE-GAME    
            WHEN OTHER                                                  
      *TEST                                                             
      *    DISPLAY ' INVALID MODE '                                     
      *    DISPLAY ' SET ZZEC0210-O-RC-INVALID-MODE TO TRUE   '         
      */TEST                                                            
                 MOVE 'INVALID MODE ' TO ZZEC0210-O-ERROR-MESSAGE       
                 SET ZZEC0210-O-RC-INVALID-MODE TO TRUE                 
           END-EVALUATE                                                 
           MOVE WS-ZZEC0210 TO DFHCOMMAREA                              
      * ROUTINE ENDS PROCESSING OF THE RESOURCES                        
      * SO AT THE END WE CAN RELEASE THE LOCKS WE MADE                  
                                                                        
           PERFORM 7900-UNLOCK-RESOURCES                                
           .                                                            
      ******************************************************************
      *                   2110-DISPLAY-MAP                              
      * PARAGRAPH NEEDS TO:                                             
      * TAKE LAST MAP IMAGE FROM DATABSE                                
      *  MOVE THIS DATA TO MAP VARIABLES                                
      * DISPLAY THE MAP                                                 
      * RETURN CONTROL TO MAIN PROGRAM                                  
      ******************************************************************
       2110-DISPLAY-MAP.                                                
      * THIS PARAGRAPH WILL STORE DATA FROM DATABASE IN PROGRAM VARIABLE
      * NOW WE NEED TO MOVE IT TO MAP VARIABLES                         
      *TEST                                                             
      *    DISPLAY ' 2110-DISPLAY-MAP '                                 
      */TEST                                                            
           PERFORM 7200-SELECT-MAP-FROM-DB                              
                                                                        
           PERFORM 2129-MOVE-DATA-TO-SYBMOLIC-MAP                       
                                                                        
                                                                        
           PERFORM 2111-SEND-THE-MAP     
      *TEST                                                             
      *    DISPLAY 'FLAG ZZEC0210-O-RC-NORMAL SET TRUE '                
      */TEST                                                            
           SET ZZEC0210-O-RC-NORMAL TO TRUE                             
           MOVE ' ' TO ZZEC0210-O-ERROR-MESSAGE                         
           DISPLAY 'STATUS SUCCESS DISPLAY '                            
           .                                                            
      ******************************************************************
      *                       2111-SEND-THE-MAP                         
      ******************************************************************
       2111-SEND-THE-MAP.                                               
      *TEST                                                             
      *    DISPLAY '2111-SEND-THE-MAP '                                 
      */TEST                                                            
           EXEC CICS                                                    
           SEND MAP('MP0211') MAPSET('MP0211')                          
           FROM(MP0211O)                                                
           ERASE                                                        
           END-EXEC                                                     
      *    MOVE DFHRESP(ITEMERR) TO EIBRESP                             
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                       2120-ADD-A-SHAPE                          
      *   PROGRAM GET THRU COMMAREA :                                   
      *   1. NAME OF THE SHAPE                                          
      *   2. POSITION OF X ( WHERE SHAPE SHOULD BE PLACED )             
      *   3. POSITION OF Y ( WHERE SHAPE SHOULD BE PLACED )             
      *                                                                 
      * THIS PARAGRAPH HAVE TO CHECK :                                  
      *   1. IF WE GOT SHAPE WITH THAT NAME IN DATABASE                 
      *                                                                 
      *   2. IF THIS SHAPE CAN BE PLACED ON THE MAP ON THIS PLACE       
      *   ( WHETHER ALL ALIVE CELLS WILL BE PLACED ON MAP )             
      *                                                                 
      * IF THAT CONDITIONS ARE TRUE THEN PROGRAM WILL ADD THIS SHAPE    
      *                                                                 
      * IN THIS PARAGRAPH THERE IS ALSO SPECIAL LOGIC OF DISPALYING     
      * THE SHAPE TO MAKE IT EASY FOR THE USER WHAT HE ADDED            
      *                                                                 
      ******************************************************************
       2120-ADD-A-SHAPE.                                                
      *TEST                                                             
      *    DISPLAY '2120-ADD-A-SHAPE  '                                 
      */TEST                                                            
           PERFORM 2160-CHECK-IF-INPUT-IS-VALID                         
                                                                        
           IF SO-VALID-INPUT-DATA THEN                                  
             PERFORM 7300-CHECK-IF-SHAPE-EXIST                          
                                                                        
             IF SO-SHAPE-EXISTS THEN                                    
                                                                        
      *TEST                                                             
      *      DISPLAY 'SO-SHAPE-EXISTS        '                          
      */TEST                                                            
                PERFORM 2121-CHECK-IF-CAN-BE-PLACED                     
                                                                        
                IF SO-CAN-BE-PLACED THEN                                
                                                                        
      *TEST                                                             
      *    DISPLAY 'SO-CAN-BE-PLACED TRUE  '                            
      */TEST                                                            
                 PERFORM 2122-PREPARE-THE-MAP                           
                 PERFORM 2124-PLACE-THE-SHAPE                           
                 PERFORM 2128-FLASH-THE-SHAPE                           
                                                                        
      *        WE NEED TO SAVE THE MAP TO THE DATABSE                   
                                                                        
                 PERFORM 2125-PREPARE-AND-SAVE-THE-MAP                  
                                                                        
      *TEST                                                             
      *    DISPLAY 'FLAG ZZEC0210-O-RC-NORMAL SET TRUE '     
      */TEST                                                            
                 SET ZZEC0210-O-RC-NORMAL TO TRUE                       
                                                                        
                ELSE                                                    
      *TEST                                                             
      *    DISPLAY '2120 SHAPE CANT BE PLACED HERE '                    
      *    DISPLAY '2120 SET ZZEC0210-O-RC-INVALID-POSTION TO TRUE  '   
      */TEST                                                            
                 SET ZZEC0210-O-RC-INVALID-POSTION TO TRUE              
                 MOVE 'SHAPE CANNOT BE PLACED IN THIS POSITION ' TO     
                                                ZZEC0210-O-ERROR-MESSAGE
                END-IF                                                  
             ELSE                                                       
      *TEST                                                             
      *     DISPLAY '2120 INVALID SHAPE  '                              
      *    DISPLAY '2120 SET   ZZEC0210-O-RC-INVALID-SHAPE TO TRUE   '  
      */TEST                                                            
                SET ZZEC0210-O-RC-INVALID-SHAPE TO TRUE                 
                MOVE 'SHAPE DONT EXIST ' TO ZZEC0210-O-ERROR-MESSAGE    
             END-IF                                                     
           ELSE                                                         
      * NOT VALID DATA                                                  
             MOVE 'POSITIONS WERENT VALID NUMERIC ' TO                  
                                                ZZEC0210-O-ERROR-MESSAGE
             SET ZZEC0210-O-RC-INVALID-INPUT TO TRUE                    
                                                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2121-CHECK-IF-CAN-BE-PLACED                   
      * PROGRAM USES THIS VARIABLES:                                    
      *                                                                 
      *      THIS VARIABLES INDICATES WERE USER WANTS TO PUT A SHAPE    
      *          ZZEC0210-I-POSITION-X                                  
      *          ZZEC0210-I-POSITION-Y                                  
      *     THIS IS MAXIMUM OF SHAPE ( MAX X AND MAX Y )    
      *          WS-MAX-POS-OF-X                                        
      *          WS-MAX-POS-OF-Y                                        
      ******************************************************************
       2121-CHECK-IF-CAN-BE-PLACED.                                     
      *TEST                                                             
      *    DISPLAY ' 2121-CHECK-IF-CAN-BE-PLACED '                      
      */TEST                                                            
           IF ZZEC0210-I-POSITION-X + WS-MAX-POS-OF-X <=                
              CT-MAXIMUM-WIDTH-OF-SCREEN  AND                           
              ZZEC0210-I-POSITION-Y + WS-MAX-POS-OF-Y <=                
              CT-MAXIMUM-HEIGHT-OF-SCRREN THEN                          
                                                                        
      *TEST                                                             
      *    DISPLAY ' 2112 CONDITION TRUE  '                             
      *    DISPLAY ' 2121 SO-CAN-BE-PLACED   TO TRUE  '                 
      */TEST                                                            
             SET SO-CAN-BE-PLACED   TO TRUE                             
           ELSE                                                         
      *TEST                                                             
      *    DISPLAY ' 2112 CONDITION FALSE '                             
      *    DISPLAY ' 2121 SO-CANT-BE-PLACED  TO TRUE  '                 
      */TEST                                                            
             SET SO-CANT-BE-PLACED  TO TRUE                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2122-PREPARE-THE-MAP                        
      * PARAGRAPH WILL TAKE MAP FROM DATABASE                           
      * AND WILL PUT IT IN SYMBOLIC MAP VARIABLES                       
      ******************************************************************
       2122-PREPARE-THE-MAP.                                            
      *TEST                                                             
      *    DISPLAY '2122-PREPARE-THE-MAP         '                      
      */TEST                                                            
                                                                        
           PERFORM 7200-SELECT-MAP-FROM-DB 
      * PARAGRAPH WILL MOVE SCREEN DATA FROM PROGRAM VARIABLES          
      * TO MAP VARIABLES                                                
                                                                        
           PERFORM 2129-MOVE-DATA-TO-SYBMOLIC-MAP                       
           .                                                            
      ******************************************************************
      *                   2124-PLACE-THE-SHAPE                          
      ******************************************************************
       2124-PLACE-THE-SHAPE.                                            
      *TEST                                                             
      *    DISPLAY '2124-PLACE-THE-SHAPE         '                      
      */TEST                                                            
           PERFORM 7400-OPEN-CURSOR                                     
           SET SO-NOT-END-OF-DATA-SHAPE-TABLE TO TRUE                   
                                                                        
           PERFORM 7600-FETCH-SHAPE-TABLE-RECORD                        
                                                                        
           PERFORM UNTIL SO-END-OF-DATA-SHAPE-TABLE                     
                                                                        
      * FOR EVERY RECORD FROM SHAPE_TABLE2                              
      * PROGRAM WILL GET  POSITION OF X  FROM TABLE (SAME FOR Y)        
      * WILL CALCULATE THIS POSISITON + WHERE USER WANTS TO PLACE IT   I
      * AND WILL PUT '/' SYMBOL WHERE THIS PLACE SHOULD BE              
      * WE ARE PLACING '/' TO MAKE IT MORE OBVOIUS WHAT USER JUST PLACED
                                                                        
             COMPUTE POSITION-X = POSITION-X + ZZEC0210-I-POSITION-X - 1
             COMPUTE POSITION-Y = POSITION-Y + ZZEC0210-I-POSITION-Y - 1
                                                                        
             MOVE '/' TO POLEO(POSITION-Y)(POSITION-X:1)                
                                                                        
             PERFORM 7600-FETCH-SHAPE-TABLE-RECORD                      
           END-PERFORM                                                  
                                                                        
           PERFORM 7500-CLOSE-CURSOR                                    
           .                                                            
      ******************************************************************
      *                2125-PREPARE-AND-SAVE-THE-MAP                    
      * PROGRAM WILL PREPARE DATABASE RECORD                            
      * CONTAINING OUR MAP THAT WILL BE SAVED TO DATABASE               
      ******************************************************************
       2125-PREPARE-AND-SAVE-THE-MAP.                                   
      *TEST                                                             
      *    DISPLAY '2125-PREPARE-AND-SAVE-THE-MAP'                      
      */TEST                                                            
      * WE NEED TO MOVE DATA FROM MAP COPYBOOK AND PLACE IT IN          
      * DATABASE VARIABLES                                              
      * SYMBOLIC VARS -> WS-MAP-LINE                                    
      * WS-MAP TO MAP-DATA                                              
                                                                        
           INITIALIZE WS-MAP                                            
           INITIALIZE MAP-DATA                                          
                                                                        
      * MOVE SCRREN DATA FROM SYMBOLIC MAP TO PROGRAM VARIABLES         
           PERFORM 2148-MOVE-SCREEN-DATA-TO-PROG                        
                                                                        
      *TEST                                                             
      *    DISPLAY 'PROGRAM DATA MOVED TO DATABASE RECORD DATA'         
      */TEST                                                            
           MOVE WS-MAP  TO MAP-DATA-TEXT                                
                                                                        
           PERFORM 7100-SAVE-THE-MAP                                    
                                                                        
           .                                                            
      ******************************************************************
      *                   2126-DELAY-600MS                              
      ******************************************************************
       2126-DELAY-600MS.                                                
      *TEST                                                             
      *    DISPLAY '2126-DELAY-600MS    '                               
      */TEST                                                            
           EXEC CICS                                                    
           DELAY FOR MILLISECS(600)   
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *               2127-CHANGE-SYMBOLS                               
      * THIS PARAGRAPH WILL CHANGE SYMBOLS ON OUR MAP                   
      * TO DIFFRERENT EVERY TIME IT IS CALLED                           
      * THIS WILL CREATE 'ANIMATION' TO INDICATE WHICH SHAPE WAS JUST   
      * ADDED                                                           
      ******************************************************************
       2127-CHANGE-SYMBOLS.                                             
      * FROM / TO \ OR FROM \ TO 'X'                                    
      *TEST                                                             
      *    DISPLAY '2127-CHANGE-SYMBOLS '                               
      */TEST                                                            
           EVALUATE TRUE                                                
           WHEN  SO-FIRST-CHANGE                                        
      *TEST                                                             
      *    DISPLAY '2127-CHANGE-SYMBOLS  SO FIRST CHANGE'               
      *    DISPLAY '2127 CHANGE SYMBOLS FROM "/" TO "\" '               
      *    DISPLAY '2127 SET SO-SECOND-CHANGE TO TRUE '                 
      */TEST                                                            
                                                                        
             SET SO-SECOND-CHANGE TO TRUE                               
                                                                        
             PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 24     
                 INSPECT POLEO(WS-ITER) REPLACING ALL '/' BY '\'        
             END-PERFORM                                                
                                                                        
           WHEN SO-SECOND-CHANGE                                        
      *TEST                                                             
      *    DISPLAY '2127-CHANGE-SYMBOLS  SO SEC CHANGE'                 
      *    DISPLAY '2127 CHANGE SYMBOLS FROM "\" TO "+" '               
      *    DISPLAY '2127 SET SO-THIRD-CHANGE  TO TRUE '                 
      */TEST         
             SET SO-THIRD-CHANGE TO TRUE                                
                                                                        
             PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 24     
                 INSPECT POLEO(WS-ITER) REPLACING ALL '\' BY '+'        
             END-PERFORM                                                
                                                                        
           WHEN SO-THIRD-CHANGE                                         
      *TEST                                                             
      *    DISPLAY '2127-CHANGE-SYMBOLS  SO THRD CHANGE'                
      *    DISPLAY '2127 CHANGE SYMBOLS FROM "+" TO "X" '               
      *    DISPLAY '2127 SET SO-LAST-CHANGE   TO TRUE '                 
      */TEST                                                            
                                                                        
             SET SO-LAST-CHANGE TO TRUE                                 
                                                                        
             PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 24     
                 INSPECT POLEO(WS-ITER) REPLACING ALL '+' BY 'X'        
             END-PERFORM                                                
                                                                        
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2128-FLASH-THE-SHAPE                          
      * THIS PARAGRAPH WILL CREATE 'ANIMATION '                         
      * THAT WILL SHOW USER WHAT SHAPE HE JUST PLACED ON MAP            
      *                                                                 
      * THIS PARAGRAPH USES 2127-CHANGE-SYMBOLS PARAGRAPH THAT WILL     
      * CHANGE LETTER OR SYMOBL THAT REPRESENT OUR SHAPE                
      * EVERY TIME THIS PARAGRAPH WILL BE CALLED DIFFERENT SYMBOL       
      * WILL BE USED                                                    
      * AFTER 3 CALLS SYMBOL WILL BE JUST 'X' LIKE EVERY OTHER CELL     
      *                                                                 
      ******************************************************************
       2128-FLASH-THE-SHAPE.                                            
      *TEST                                                             
      *    DISPLAY '2128-FLASH-THE-SHAPE'      
      */TEST                                                            
           PERFORM 2111-SEND-THE-MAP                                    
           PERFORM 2126-DELAY-600MS                                     
           SET SO-FIRST-CHANGE TO TRUE                                  
                                                                        
           PERFORM 3 TIMES                                              
             PERFORM 2127-CHANGE-SYMBOLS                                
             PERFORM 2111-SEND-THE-MAP                                  
             PERFORM 2126-DELAY-600MS                                   
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2129-MOVE-DATA-TO-SYBMOLIC-MAP                  
      ******************************************************************
       2129-MOVE-DATA-TO-SYBMOLIC-MAP.                                  
      *TEST                                                             
      *    DISPLAY '2129-MOVE-DATA-TO-SYBMOLIC-MAP '                    
      *    DISPLAY '2129 DATA WAS MOVED '                               
      */TEST                                                            
                                                                        
           PERFORM VARYING WS-ITER4 FROM 1 BY 1 UNTIL WS-ITER4 > 24     
                                                                        
                MOVE WS-MAP-LINE(WS-ITER4) TO POLEO(WS-ITER4)           
                                                                        
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2130-RESET-THE-GAME                           
      * PARAGRAPH WILL 'KILL ' ALL ALIVE CELLS                          
      * SO  AND THE END WE WILL GET CLEAR MAP                           
      ******************************************************************
       2130-RESET-THE-GAME.                                             
      *TEST                                                             
      *    DISPLAY '2130-RESET-THE-GAME '                               
      *    DISPLAY '2130 MAP DATA INITIALIZE '                          
      */TEST     
      * INITIALIZATION                                                  
           MOVE SPACE TO WS-MAP                                         
                                                                        
           MOVE WS-MAP TO MAP-DATA-TEXT                                 
                                                                        
           PERFORM 7100-SAVE-THE-MAP                                    
                                                                        
      *TEST                                                             
      *    DISPLAY 'ZZEC0210-O-RC-SUCCESS-RESET TO TRUE '               
      */TEST                                                            
           SET ZZEC0210-O-RC-SUCCESS-RESET TO TRUE                      
           MOVE ' ' TO ZZEC0210-O-ERROR-MESSAGE                         
           .                                                            
      ******************************************************************
      *                  2140-START-THE-GAME                            
      * PROGRAM WILL TAKE MAP FROM THE DATABASE                         
      * IN LOOP:                                                        
      * PROGRAM WILL BE MODIFING MAP BY LOOKING ON THE PROGRAM VARIABLES
      * (SYMBOLIC MAP DATA WILL BE MODIFIED)                            
      *  DISPLAYING OF THE MODIFIED MAP                                 
      * MOVING SYMBOLIC MAP TO PROGRAM VARIABLES                        
      * END-LOOP                                                        
      ******************************************************************
       2140-START-THE-GAME.                                             
      *TEST                                                             
      *    DISPLAY '2140-START-THE-GAME '                               
      */TEST                                                            
           INITIALIZE WS-MAP                                            
           INITIALIZE MAP-DATA-TEXT                                     
                                                                        
           PERFORM 7200-SELECT-MAP-FROM-DB                              
                                                                        
      * MOVE DATA FROM PROGRAM VARIABLES TO SYMBOLIC MAP VARIABLES      
                                                                        
           PERFORM  2129-MOVE-DATA-TO-SYBMOLIC-MAP    
      * TUTAJ TEN SEND DZIAL DOBRZE                                     
      *      PERFORM 2111-SEND-THE-MAP                                  
                                                                        
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 >        
           CT-NUMBER-OF-ITERATIONS OR  SO-NO-ACTIVE-CELLS               
                                                                        
                                                                        
             PERFORM 2141-ITERATE-THE-GAME                              
             PERFORM 2111-SEND-THE-MAP                                  
             PERFORM 2146-DELAY-100MS                                   
                                                                        
      *  MOVE SYMBOLIC MAP VARIABLES TO PROGRAM VARIABLES               
             PERFORM 2148-MOVE-SCREEN-DATA-TO-PROG                      
                                                                        
                                                                        
             PERFORM 2147-CHECK-FOR-ACTIVE-CELLS                        
           END-PERFORM                                                  
                                                                        
      *  AFTER THIS GAME ENDS WE NEED TO SAVE THE LAST IMAGE OF         
      *  MAP TO THE DATABASE TO USE IT LATER                            
                                                                        
           MOVE WS-MAP TO MAP-DATA-TEXT                                 
           PERFORM 7100-SAVE-THE-MAP                                    
      *TEST                                                             
      *    DISPLAY 'FLAG ZZEC0210-O-RC-NORMAL SET TRUE '                
      */TEST                                                            
           SET ZZEC0210-O-RC-NORMAL TO TRUE                             
           MOVE ' ' TO ZZEC0210-O-ERROR-MESSAGE                         
           .                                                            
      ***************************************************************** 
      *                2141-ITERATE-THE-GAME                            
      * AT EACH ITERATION PROGRAM WILL COUNT TOTAL OF NEIGHBOURS FOR    
      * ALL THE CELLS                                                   
      * IF THIS NUMBER IS EQUAL TO ZERO IT MEANS THAT                   
      * ALL THE CELLS ARE DEAD                                          
      *           
      ******************************************************************
       2141-ITERATE-THE-GAME.                                           
      *TEST                                                             
      *    DISPLAY '2141-ITERATE-THE-GAME '                             
      */TEST                                                            
          INITIALIZE WS-NEIGHBORS-TOTAL                                
                                                                       
          PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 24       
             PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 79  
                INITIALIZE WS-COUNT-NEIGHBORS                          
                PERFORM 2142-COUNT-NEIGHBORS                           
                                                                       
                SET SO-DO-NOTHING TO TRUE                              
      * THIS DECISION WILL BE MADE BASED ON NUMBER OF NEIGHBORS         
                PERFORM 2145-MAKE-DECISION-LIVE-DEAD                   
                                                                       
                 EVALUATE TRUE                                          
                   WHEN SO-KILL-THE-CELL                                
      *TEST                                                             
      *    DISPLAY '2141 KILING THE  CELL  '                            
      *    DISPLAY '2141 SPACE WAS MOVED TO THIS POSITION '             
      */TEST                                                            
                       MOVE ' ' TO POLEO(WS-ITER)(WS-ITER2:1)           
                   WHEN SO-CREATE-NEW-CELL                              
      *TEST                                                             
      *    DISPLAY '2141 CREATING THE CELL  (BIRTH) '                   
      *    DISPLAY '2141 X WAS PLACED ON THAT POSITION '                
      */TEST                                                            
                       MOVE 'X' TO POLEO(WS-ITER)(WS-ITER2:1)           
                    WHEN SO-DO-NOTHING                                   
      *TEST                                                             
      *    DISPLAY '2141 SO-DO-NOTHING  NO ACTION IS TAKEN '            
      */TEST                                                            
                       CONTINUE                                         
                 END-EVALUATE      
                  ADD WS-COUNT-NEIGHBORS TO WS-NEIGHBORS-TOTAL           
               END-PERFORM                                               
            END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2142-COUNT-NEIGHBORS                          
      * PARAGRAPH WILL RETURN EXECT AMOUNT OF NEIGHBORS EACH CELL HAS   
      *                                                                 
      * DECISITION ABOUT DEATH / BIRTH WILL BE MADED BASED ON THAT      
      * NUMBER                                                          
      *                                                                 
      *                                                                 
      *                                                                 
      ******************************************************************
       2142-COUNT-NEIGHBORS.                                            
      *TEST                                                             
      *    DISPLAY '2142-COUNT-NEIGHBORS  '                             
      */TEST                                                            
           MOVE 0 TO WS-COUNT-NEIGHBORS                                 
                                                                        
      * SENCARIO:                *********************************      
      *                          *                               *      
      *                          *                               *      
      *                          *         X                     *      
      *                          *                               *      
      *                          *********************************      
      * 'X' DONT TOUCH ANY CORNER OR SIDE                               
      * SO WE WILL CHECK ALL POSSIBLE NEIGHBORS                         
      *                                                                 
           IF WS-ITER > 1 AND WS-ITER < 24                              
           AND                                                          
              WS-ITER2 > 1 AND WS-ITER2 < 79                            
           THEN                                                         
      * HERE ALL THE NEIGHBORS PHYSICLY EXISTS                          
      *TEST     
      */TEST                                                  
              PERFORM 2150-CHECK-LEFT-TOP                    
              PERFORM 2151-CHECK-LEFT-MEDIUM                 
              PERFORM 2152-CHECK-LEFT-BOTTOM                 
              PERFORM 2153-CHECK-MEDIUM-BOTTOM               
              PERFORM 2154-CHECK-RIGHT-BOTTOM                
              PERFORM 2155-CHECK-MEDIUM-RIGHT                
              PERFORM 2156-CHECK-RIGHT-TOP                   
              PERFORM 2157-CHECK-MEDIUM-TOP                  
           ELSE                                               
      * HERE WE WILL CHECK NOT PERFECT SCENARIOS              
      * LIKE BEEING AT THE CORNER OR AT THE SIDE              
                                                             
                                                             
      * LEFT TOP CORNER                                       
      *            ***********************                    
      *            *X                    *                    
      *            *                     *                    
      *            *                     *                    
      *            ***********************                    
                                                             
            IF WS-ITER = 1 AND WS-ITER2 = 1 THEN             
      *TEST                                                   
      *    DISPLAY '2142 TOP LEFT CORNER   '                  
      */TEST                                                  
               PERFORM 2155-CHECK-MEDIUM-RIGHT               
               PERFORM 2154-CHECK-RIGHT-BOTTOM               
               PERFORM 2153-CHECK-MEDIUM-BOTTOM              
            END-IF                                           
      * TOP RIGHT CORNER                                      
      *            ***********************                    
      *            *                    X*                    
      *            *                     *                    
      *            *                     *                    
      *            ***********************                    
            IF WS-ITER = 1 AND WS-ITER2 = 79 THEN   
      *TEST                                              
      *    DISPLAY '2142 TOP RIGHT CORNER   '            
      */TEST                                             
                PERFORM 2151-CHECK-LEFT-MEDIUM           
                PERFORM 2152-CHECK-LEFT-BOTTOM           
                PERFORM 2153-CHECK-MEDIUM-BOTTOM         
             END-IF                                      
                                                         
      * LEFT BOTTOM CORNER                               
      *            ***********************               
      *            *                     *               
      *            *                     *               
      *            *X                    *               
      *            ***********************               
             IF WS-ITER = 24 AND WS-ITER2 = 1 THEN       
      *TEST                                              
      *    DISPLAY '2142 BOTTOM LEFT CORNER '            
      */TEST                                             
               PERFORM 2157-CHECK-MEDIUM-TOP             
               PERFORM 2156-CHECK-RIGHT-TOP              
               PERFORM 2155-CHECK-MEDIUM-RIGHT           
             END-IF                                      
      * RIGHT BOTTOM CORNER                              
      *            ***********************               
      *            *                     *               
      *            *                     *               
      *            *                    X*               
      *            ***********************               
             IF WS-ITER = 24 AND WS-ITER2 = 79 THEN      
      *TEST                                              
      *       DISPLAY '2142 BOTTOM RIGHT CORNER '        
      */TEST                                             
               PERFORM 2157-CHECK-MEDIUM-TOP             
               PERFORM 2150-CHECK-LEFT-TOP               
               PERFORM 2151-CHECK-LEFT-MEDIUM            
             END-IF                                      
      *  TOUCHING BOTTOM SIDE  *****************************            
      *                        *                           *            
      *                        *                           *            
      *                        *                           *            
      *                        *         X                 *            
      *                        *****************************            
      *                                                                 
             IF WS-ITER = 24 AND WS-ITER2 > 1 AND WS-ITER2 < 79         
      *TEST                                                             
      *       DISPLAY '2142 BOTTOM SIDE '                               
      */TEST                                                            
               PERFORM 2155-CHECK-MEDIUM-RIGHT                          
               PERFORM 2156-CHECK-RIGHT-TOP                             
               PERFORM 2157-CHECK-MEDIUM-TOP                            
               PERFORM 2150-CHECK-LEFT-TOP                              
               PERFORM 2151-CHECK-LEFT-MEDIUM                           
             END-IF                                                     
      *   TOUCHING UPPER SIDE                                           
      *                                 *******************             
      *                                 *       X         *             
      *                                 *                 *             
      *                                 *******************             
             IF WS-ITER = 1 AND WS-ITER2 > 1 AND WS-ITER2 < 79 THEN     
      *TEST                                                             
      *       DISPLAY '2142 UPPER SIDE '                                
      */TEST                                                            
               PERFORM 2151-CHECK-LEFT-MEDIUM                           
               PERFORM 2152-CHECK-LEFT-BOTTOM                           
               PERFORM 2153-CHECK-MEDIUM-BOTTOM                         
               PERFORM 2154-CHECK-RIGHT-BOTTOM                          
               PERFORM 2155-CHECK-MEDIUM-RIGHT                          
             END-IF                                                     
      *   TOUCHING LEFT SIDE                                            
      *                                 *******************             
      *                                 *                 *             
      *                                 *X                *          
      *                                 *                 *             
      *                                 *******************             
             IF WS-ITER2 = 1 AND WS-ITER > 1 AND WS-ITER < 24           
      *TEST                                                             
      *       DISPLAY '2142 LEFT SIDE '                                 
      */TEST                                                            
               PERFORM 2157-CHECK-MEDIUM-TOP                            
               PERFORM 2156-CHECK-RIGHT-TOP                             
               PERFORM 2155-CHECK-MEDIUM-RIGHT                          
               PERFORM 2154-CHECK-RIGHT-BOTTOM                          
               PERFORM 2153-CHECK-MEDIUM-BOTTOM                         
             END-IF                                                     
      *  TOUCHING RIGHT SIDE                                            
      *                                 *******************             
      *                                 *                 *             
      *                                 *                X*             
      *                                 *                 *             
      *                                 *******************             
             IF WS-ITER2 = 79 AND WS-ITER > 1 AND WS-ITER < 24          
             THEN                                                       
      *TEST                                                             
      *       DISPLAY '2142 RIGHT SIDE '                                
      */TEST                                                            
               PERFORM 2157-CHECK-MEDIUM-TOP                            
               PERFORM 2150-CHECK-LEFT-TOP                              
               PERFORM 2151-CHECK-LEFT-MEDIUM                           
               PERFORM 2152-CHECK-LEFT-BOTTOM                           
               PERFORM 2153-CHECK-MEDIUM-BOTTOM                         
             END-IF                                                     
           END-IF                                                       
           .                                                            
                                                                        
      ******************************************************************
      *                   2143-CHECK-IF-ALIVE                           
      * CHECKING IF THIS MAP POSITION(WITH GIVEN X AND Y) IS ALIVE OR   
      * NOT, IF SO WE ARE ADDING 1 TO WS-COUNT-NEIGHBORS VARRIABLE      
      ******************************************************************
       2143-CHECK-IF-ALIVE.                                             
      *TEST                                                             
      *    DISPLAY '2143-CHECK-IF-ALIVE '                               
      */TEST                                                            
           IF WS-MAP-LINE(WS-Y-VALUE)(WS-X-VALUE: 1)  = 'X'             
           THEN                                                         
      *TEST                                                             
      *    DISPLAY 'WS-MAP-LINE(WS-Y-VALUE)(WS-X-VALUE: 1)  = X  '      
      *    DISPLAY '2143 ADDING 1 TO WS-COUNT-NEIGHBORS'                
      */TEST                                                            
              ADD 1 TO WS-COUNT-NEIGHBORS                               
           ELSE                                                         
      *TEST                                                             
      *    DISPLAY 'WS-MAP-LINE(WS-Y-VALUE)(WS-X-VALUE: 1) NOT  = X  '  
      *    DISPLAY 'NO ACTION IS TAKEN '                                
            CONTINUE                                                    
      */TEST                                                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *             2145-MAKE-DECISION-LIVE-DEAD                        
      * THIS PARAGRAPH WILL SET FLAG BASED ON NUMBER OF NEIGHBORS       
      * CELL HAS                                                        
      * AND BASED ON THAT FLAG CELL CEN SURVIVE OR DIE                  
      ******************************************************************
       2145-MAKE-DECISION-LIVE-DEAD.                                    
      *TEST                                                             
      *    DISPLAY ' 2145-MAKE-DECISION-LIVE-DEAD '                     
      */TEST                                                            
           IF WS-MAP-LINE(WS-ITER)(WS-ITER2:1) = ' ' THEN               
                                                                        
      *TEST                                                             
      *     DISPLAY '2145 WS-MAP-LINE(WS-ITER)(WS-ITER2:1) = ' '    '   
      *       ' ( CELL IS NOT ALIVE ) '                                 
      */TEST   
              IF WS-COUNT-NEIGHBORS = 3 THEN                            
      *TEST                                                             
      *          DISPLAY '2145 WS-COUNT-NEIGHBORS = 3 '                 
      *          DISPLAY '2145 SET SO-CREATE-NEW-CELL TO TRUE'          
      */TEST                                                            
                 SET SO-CREATE-NEW-CELL TO TRUE                         
              ELSE                                                      
      *TEST                                                             
      *          DISPLAY '2145  WS-COUNT-NEIGHBORS NOT = 3 '            
      *          DISPLAY '2145 SET SO-DO-NOTHING TO TRUE '              
      */TEST                                                            
                 SET SO-DO-NOTHING      TO TRUE                         
              END-IF                                                    
           ELSE                                                         
      * CELL IS ALIVE                                                   
      *TEST                                                             
           DISPLAY ' 2145 WS-MAP-LINE(WS-ITER)(WS-ITER2:1)NOT  = ' ' '  
                  ' CELL IS ALIVE '                                     
      */TEST                                                            
              IF WS-COUNT-NEIGHBORS = 3 OR 2 THEN                       
      *TEST                                                             
            DISPLAY '2145 WS-COUNT-NEIGHBORS = 3 OR 2 '                 
            DISPLAY '2145 SET SO-DO-NOTHING TO TRUE '                   
      */TEST                                                            
                 SET SO-DO-NOTHING TO TRUE                              
              ELSE                                                      
      *TEST                                                             
      *     DISPLAY '2145 WS-COUNT-NEIGHBORS NOT = 3 OR 2 '             
      *     DISPLAY '2145 SET SO-KILL-THE-CELL TO TRUE '                
      */TEST                                                            
                 SET SO-KILL-THE-CELL TO TRUE                           
              END-IF                                                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2146-DELAY-100MS                    
      ******************************************************************
       2146-DELAY-100MS.                                                
      *TEST                                                             
      *    DISPLAY ' 2146-DELAY-100MS '                                 
      */TEST                                                            
           EXEC CICS                                                    
           DELAY FOR MILLISECS(100)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2147-CHECK-FOR-ACTIVE-CELLS                    
      ******************************************************************
       2147-CHECK-FOR-ACTIVE-CELLS.                                     
      *TEST                                                             
      *    DISPLAY '  2147-CHECK-FOR-ACTIVE-CELLS  '                    
      */TEST                                                            
                                                                        
           IF  WS-NEIGHBORS-TOTAL = 0 THEN                              
              DISPLAY ' WS-NEIGBORS-TOTAL = 0 '                         
              DISPLAY ' SET SO-NO-ACTIVE-CELLS TO TRUE'                 
              SET SO-NO-ACTIVE-CELLS  TO TRUE                           
           ELSE                                                         
      *TEST                                                             
      *       DISPLAY '2147 WS-NEIGHBORS NOT EQUAL TO 0 '               
      *       DISPLAY '2147 NO ACTION IS TAKEN  '                       
             CONTINUE                                                   
      */TEST                                                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *             2148-MOVE-SCREEN-DATA-TO-PROG                       
      * PROGRAM WILL MOVE MAP DATA FROM SYMOBLIC MAP TO                 
      * PROGRAM VARIABLES                                               
      ******************************************************************
       2148-MOVE-SCREEN-DATA-TO-PROG.                                   
      *TEST                                                             
           DISPLAY '2148-MOVE-SCREEN-DATA-TO-PROG  '                    
      */TEST                                                            
                                                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 24     
              MOVE POLEO(WS-ITER5) TO WS-MAP-LINE(WS-ITER5)             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2150-CHECK-LEFT-TOP                             
      ******************************************************************
       2150-CHECK-LEFT-TOP.                                             
      *TEST                                                             
      *147-CHECK-FOR-ACTIVE-CELLSFT-TOP  '                              
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
                                                                        
           COMPUTE WS-Y-VALUE = WS-ITER  - 1                            
           COMPUTE WS-X-VALUE = WS-ITER2 - 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *             2151-CHECK-LEFT-MEDIUM                              
      ******************************************************************
       2151-CHECK-LEFT-MEDIUM.                                          
      *TEST                                                             
      *    DISPLAY '2151-CHECK-LEFT-MEDIUM '                            
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER      
           COMPUTE WS-X-VALUE = WS-ITER2 - 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *             2152-CHECK-LEFT-BOTTOM                              
      ******************************************************************
       2152-CHECK-LEFT-BOTTOM.                                          
      *TEST                                                             
      *    DISPLAY '2152-CHECK-LEFT-BOTTOM '                            
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER  + 1                            
           COMPUTE WS-X-VALUE = WS-ITER2 - 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *             2153-CHECK-MEDIUM-BOTTOM                            
      ******************************************************************
       2153-CHECK-MEDIUM-BOTTOM.                                        
      *TEST                                                             
      *    DISPLAY '2153-CHECK-MEDIUM-BOTTOM '                          
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER  + 1                            
           COMPUTE WS-X-VALUE = WS-ITER2                                
      *TEST            
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *             2154-CHECK-RIGHT-BOTTOM                             
      ******************************************************************
       2154-CHECK-RIGHT-BOTTOM.                                         
      *TEST                                                             
      *    DISPLAY '2154-CHECK-RIGHT-BOTTOM '                           
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER  + 1                            
           COMPUTE WS-X-VALUE = WS-ITER2 + 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *              2155-CHECK-MEDIUM-RIGHT                            
      ******************************************************************
       2155-CHECK-MEDIUM-RIGHT.                                         
      *TEST                                                             
      *    DISPLAY '2155-CHECK-MEDIUM-RIGHT '                           
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER                                 
           COMPUTE WS-X-VALUE = WS-ITER2 + 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE      
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *               2156-CHECK-RIGHT-TOP                              
      ******************************************************************
       2156-CHECK-RIGHT-TOP.                                            
      *TEST                                                             
      *    DISPLAY '2156-CHECK-RIGHT-TOP '                              
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER  - 1                            
           COMPUTE WS-X-VALUE = WS-ITER2 + 1                            
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *                 2157-CHECK-MEDIUM-TOP                           
      ******************************************************************
       2157-CHECK-MEDIUM-TOP.                                           
      *TEST                                                             
      *    DISPLAY '2157-CHECK-MEDIUM-TOP '                             
      *    DISPLAY 'WS-ITER: ' WS-ITER                                  
      *    DISPLAY 'WS-ITER2: ' WS-ITER2                                
      */TEST                                                            
           COMPUTE WS-Y-VALUE = WS-ITER  - 1                            
           COMPUTE WS-X-VALUE = WS-ITER2                                
      *TEST                                                             
      *    DISPLAY 'WS-Y-VALUE ' WS-Y-VALUE                             
      *    DISPLAY 'WS-X-VALUE ' WS-X-VALUE                             
      */TEST                                                            
           PERFORM 2143-CHECK-IF-ALIVE
           PERFORM 2143-CHECK-IF-ALIVE                                  
           .                                                            
      ******************************************************************
      *                   2200-CHECK-EIBRESP                            
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
      *TEST                                                             
      *    DISPLAY '2200-CHECK-EIBRESP  '                               
      */TEST                                                            
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
      *TEST                                                             
      *    DISPLAY '2200-CHECK-EIBRESP  DFHRESP NORMAL '                
      */TEST                                                            
               CONTINUE                                                 
           WHEN OTHER                                                   
      *TEST                                                             
      *    DISPLAY '2200-CHECK-EIBRESP  DFHRESP ERROR  '                
      */TEST                                                            
               MOVE 'EIB RESP ERROR ' TO  ZZEC0210-O-ERROR-MESSAGE      
                                                                        
                                                                        
      *TEST                                                             
      *    DISPLAY '2200-CHECK-EIBRESP  SET EIBRESP-ERROR FLAG TO TRUE '
      */TEST                                                            
               SET ZZEC0210-O-RC-EIBRESP-ERROR TO TRUE                  
               PERFORM 3100-CICS-ERORR-FINAL                            
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2160-CHECK-IF-INPUT-IS-VALID                 
      * PARAGRAPH WILL CHECK IF POSITION X AND POSITION Y TAKEN         
      * FROM CALLING PROGRAM IS VALID NUMERIC                           
      ******************************************************************
       2160-CHECK-IF-INPUT-IS-VALID.                                    
           IF ZZEC0210-I-POSITION-X IS NUMERIC AND                      
              ZZEC0210-I-POSITION-Y IS NUMERIC THEN  
             SET SO-VALID-INPUT-DATA  TO TRUE                           
           ELSE                                                         
             SET SO-INVALID-INPUT-DATA TO TRUE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                         3000-FINAL                              
      ******************************************************************
       3000-FINAL.                                                      
      *TEST                                                             
      *    DISPLAY '3000-FINAL                 '                        
      */TEST                                                            
           EXEC CICS                                                    
           RETURN                                                       
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    3020-FINAL-NO-CONCURRENCY                    
      ******************************************************************
       3020-FINAL-NO-CONCURRENCY.                                       
      *TEST                                                             
      *    DISPLAY '3020-FINAL-NO-CONCURRENCY  '                        
      */TEST                                                            
                                                                        
           MOVE 'SOME ONE PLAYES RIGHT NOW, TRY LATER' TO               
                                      ZZEC0210-O-ERROR-MESSAGE          
                                                                        
      *TEST                                                             
      *    DISPLAY 'ZZEC0210-O-RC-NO-CONCURRENCY TO TRUE '              
      */TEST                                                            
           SET ZZEC0210-O-RC-NO-CONCURRENCY TO TRUE                     
           MOVE WS-ZZEC0210 TO DFHCOMMAREA                              
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                    3100-CICS-ERORR-FINAL     
      ******************************************************************
       3100-CICS-ERORR-FINAL.                                           
      *TEST                                                             
      *    DISPLAY '3100-CICS-ERORR-FINAL  '                            
      */TEST                                                            
           MOVE WS-ZZEC0210 TO DFHCOMMAREA                              
           PERFORM 7900-UNLOCK-RESOURCES                                
           EXEC CICS                                                    
           RETURN                                                       
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                     3500-DB2-ERROR-FINAL                        
      ******************************************************************
       3500-DB2-ERROR-FINAL.                                            
      *TEST                                                             
      *    DISPLAY '3500-DB2-ERROR-FINAL  '                             
      */TEST                                                            
           MOVE WS-ZZEC0210 TO DFHCOMMAREA                              
      *    PERFORM 7900-UNLOCK-RESOURCES                                
           EXEC CICS                                                    
           RETURN                                                       
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                   7100-SAVE-THE-MAP                             
      * MAP IS SAVED TO DATABASE                                        
      * WE ARE USING SQL UPDATE BECAUSE WE ONLY NEED 1 RECORD           
      ******************************************************************
       7100-SAVE-THE-MAP.                                               
      *TEST                                                             
      *    DISPLAY '7100-SAVE-THE-MAP     '                             
      */TEST                                                            
           MOVE 1 TO MAP-ID                                             
           MOVE 1896 TO MAP-DATA-LEN                                    
           EXEC SQL
           UPDATE  MAP_TABLE                                            
           SET MAP_DATA = :MAP-DATA                                     
           WHERE MAP_ID = :MAP-ID                                       
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145 TO SW-SQLCODE                                      
           IF SO-SQLCODE-OK THEN                                        
             CONTINUE                                                   
      *TEST                                                             
      *     DISPLAY '7100 SQLCODE OK '                                  
      */TEST                                                            
           ELSE                                                         
      *TEST                                                             
      *     DISPLAY '7100 SQLCODE ERROR '                               
      */TEST                                                            
             SET SO-7100-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7200-SELECT-MAP-FROM-DB                     
      ******************************************************************
       7200-SELECT-MAP-FROM-DB.                                         
      *TEST                                                             
      *    DISPLAY '7200-SELECT-MAP-FROM-DB   '                         
      */TEST                                                            
           INITIALIZE MP0211O                                           
           PERFORM VARYING WS-ITER FROM 1 BY 1 UNTIL WS-ITER > 24       
               MOVE LOW-VALUES TO POLEA(WS-ITER)                        
           END-PERFORM                                                  
           MOVE 1 TO MAP-ID                                             
           INITIALIZE MAP-DATA                                          
           EXEC SQL                                                     
           SELECT MAP_DATA                                              
           INTO :MAP-DATA  
           FROM MAP_TABLE                                               
           WHERE MAP_ID = :MAP-ID                                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *     MOVE -145 TO SW-SQLCODE                                     
           IF SO-SQLCODE-OK                                             
           THEN                                                         
                                                                        
      *TEST                                                             
      *     DISPLAY '7200 SQLCODE OK  '                                 
      */TEST                                                            
      *TEST                                                             
      *     DISPLAY '7200 DATA MOVED FROM DB TO PROGRAM VARIABLES '     
      */TEST                                                            
               INITIALIZE WS-MAP                                        
               MOVE MAP-DATA-TEXT TO WS-MAP                             
                                                                        
           ELSE                                                         
      *TEST                                                             
      *     DISPLAY '7200 SQLCODE ERROR '                               
      */TEST                                                            
               SET SO-7200-PARA TO TRUE                                 
               PERFORM  9000-DB2-ERROR                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                      7300-CHECK-IF-SHAPE-EXIST                  
      * PARAGRAPH WILL CHECK IF SHAPE OF GIVEN NAME EXIST               
      * IF IT IS TRUE THEN WE ALSO WILL GET MAXIMUM OF X AND            
      * MAXIMUM OF Y                                                    
      * THAT WILL ALLOW PROGRAM TO DETERMINE IF THIS SHAPE              
      * CAN BE PLACED ON THE MAP                                        
      *                                                                 
      ******************************************************************
       7300-CHECK-IF-SHAPE-EXIST.                                       
      * ZEBY SPRAWDZIC CZY TEN KSZTALT ISTNIEJE MUSIMY                  
      * ZROBIC ZAPYTANIE DO SHAPE_TABLE3_NAME                           
                                                                        
      *TEST                                                             
      *    DISPLAY '7300-CHECK-IF-SHAPE-EXIST '                         
      */TEST                                                            
           MOVE ZZEC0210-I-SHAPE-NAME TO SHAPE-NAME                     
                                                                        
           EXEC SQL                                                     
           SELECT SHAPE_NAME, SHAPE_NAME_ID                             
           INTO :SHAPE-NAME, :SHAPE-NAME-ID                             
           FROM SHAPE_TABLE3_NAME                                       
           WHERE SHAPE_NAME = :SHAPE-NAME                               
           FETCH FIRST ROW ONLY                                         
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145 TO SW-SQLCODE                                      
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
      *TEST                                                             
      *    DISPLAY '7300 FIRST SELECT SQLCODE NORMAL '                  
      *    DISPLAY 'SET SO-SHAPE-EXISTS TO TRUE'                        
      */TEST                                                            
                SET SO-SHAPE-EXISTS  TO TRUE                            
      * IF THIS SHAPE EXISTS WE NEED TO GET                             
      * MAXIMUM OF X AND MAXIMUM OF Y FOR THAT SHAPE                    
                                                                        
      * MOVING VARIABLE FROM SHAPE_TABLE3_NAME TO                       
      * SHAPE_TABLE3 VARIABLE                                           
                                                                        
                MOVE SHAPE-NAME-ID TO SHAPE-ID                          
                                                                        
      *TEST                                                             
      *    DISPLAY '7300-CHECK-IF-SHAPE-EXIST DRUGI SELECT '            
      */TEST                                                            
                EXEC SQL    
                SELECT MAX(POSITION_X),                                
                       MAX(POSITION_Y)                                 
                INTO                                                   
                       :WS-MAX-POS-OF-X,                               
                       :WS-MAX-POS-OF-Y                                
                FROM SHAPE_TABLE3                                      
                WHERE SHAPE_ID = :SHAPE-ID                             
                GROUP BY SHAPE_ID                                      
                FETCH FIRST ROW ONLY                                   
                END-EXEC                                               
                                                                       
                MOVE SQLCODE TO SW-SQLCODE                             
      *         MOVE -145    TO SW-SQLCODE                             
      * IF SQLCODE = 000                                               
                IF SO-SQLCODE-NORMAL THEN                              
                  CONTINUE                                             
      *TEST                                                            
      *    DISPLAY ' 7300 SECOND SELECT  000  '                        
      *    DISPLAY ' 7300 WS-MAX-POS-OF-X VALUE: ' WS-MAX-POS-OF-X     
      *    DISPLAY ' 7300 WS-MAX-POS-OF-Y VALUE: ' WS-MAX-POS-OF-Y     
      *                                                                
      */TEST                                                           
                ELSE                                                   
      *TEST                                                            
      *    DISPLAY ' 7300 SECOND SELECT SQLCODE OTHER THAN 000'        
      */TEST                                                           
                  SET SO-7300-PARA TO TRUE                             
                  PERFORM 9000-DB2-ERROR                               
                END-IF                                                 
                                                                       
           WHEN SO-SQLCODE-NOT-FOUND                                   
                SET SO-SHAPE-DONT-EXISTS TO TRUE                       
      *TEST                                                            
      *    DISPLAY ' 7300-CHECK-IF-SHAPE-EXIST FIRST SELECT 100 '      
      *    DISPLAY ' 7300 SET SO-SHAPE-DONT-EXISTS '                   
      */TEST     
           WHEN OTHER                                                   
      *TEST                                                             
      *    DISPLAY ' 7300-CHECK-IF-SHAPE-EXIST FIRST SELECT OTHER '     
      */TEST                                                            
               SET SO-7300-PARA TO TRUE                                 
               PERFORM 9000-DB2-ERROR                                   
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    7400-OPEN-CURSOR                             
      ******************************************************************
       7400-OPEN-CURSOR.                                                
      *TEST                                                             
      *    DISPLAY ' 7400-OPEN-CURSOR '                                 
      */TEST                                                            
           EXEC SQL                                                     
           OPEN C-NAME                                                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145    TO SW-SQLCODE                                   
           IF SO-SQLCODE-OK THEN                                        
             CONTINUE                                                   
      *TEST                                                             
      *    DISPLAY '7400-OPEN-CURSOR  SQLCODE OK'                       
      */TEST                                                            
           ELSE                                                         
      *TEST                                                             
      *    DISPLAY '7400-OPEN-CURSOR  SQLCODE ERROR '                   
      */TEST                                                            
             SET SO-7400-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     7500-CLOSE-CURSOR                           
      ******************************************************************
       7500-CLOSE-CURSOR.                                               
      *TEST                                                             
      *    DISPLAY '7500-CLOSE-CURSOR '                                 
      */TEST                                                            
           EXEC SQL                                                     
           CLOSE C-NAME                                                 
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145    TO SW-SQLCODE                                   
           IF SO-SQLCODE-OK THEN                                        
             CONTINUE                                                   
      *TEST                                                             
      *    DISPLAY '7500-CLOSE-CURSOR  SQLCODE OK'                      
      */TEST                                                            
           ELSE                                                         
      *TEST                                                             
      *    DISPLAY '7500-CLOSE-CURSOR  SQLCODE ERROR '                  
      */TEST                                                            
             SET SO-7500-PARA  TO TRUE                                  
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   7600-FETCH-SHAPE-TABLE-RECORD                 
      ******************************************************************
       7600-FETCH-SHAPE-TABLE-RECORD.                                   
      *TEST                                                             
      *    DISPLAY '7600-FETCH-SHAPE-TABLE-RECORD    '                  
      */TEST                                                            
           EXEC SQL                                                     
           FETCH C-NAME                                                 
           INTO                                                         
           :POSITION-X,                                                 
           :POSITION-Y                                                  
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE     
      *     MOVE -145    TO SW-SQLCODE                                  
           EVALUATE TRUE                                                
             WHEN SO-SQLCODE-NORMAL                                     
      *TEST                                                             
      *    DISPLAY '7600-FETCH-SHAPE-TABLE-RECORD SQLCODE 000   '       
      *    DISPLAY 'POSITION-X: ' POSITION-X                            
      *    DISPLAY 'POSITION-Y: ' POSITION-Y                            
      */TEST                                                            
                CONTINUE                                                
             WHEN SO-SQLCODE-NOT-FOUND                                  
      *TEST                                                             
      *    DISPLAY '7600-FETCH-SHAPE-TABLE-RECORD SQLCODE 100   '       
      *    DISPLAY '7600 SO-END-OF-DATA-SHAPE-TABLE SET TO TRUE '       
      */TEST                                                            
                SET SO-END-OF-DATA-SHAPE-TABLE TO TRUE                  
             WHEN OTHER                                                 
      *TEST                                                             
      *    DISPLAY '7600-FETCH-SHAPE-TABLE-RECORD SQLCODE OTHER '       
      */TEST                                                            
              SET SO-7600-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 7700-CHECK-CONCURRENCY                          
      ******************************************************************
       7700-CHECK-CONCURRENCY.                                          
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY       '                      
      */TEST                                                            
           MOVE 1 TO CONCURRENCY-ID                                     
           EXEC SQL                                                     
           SELECT CONCURRENCY_VALUE                                     
           INTO  :CONCURRENCY-VALUE                                     
           FROM  CONCURRENCY_TABLE                                      
           WHERE CONCURRENCY_ID = :CONCURRENCY-ID     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145     TO SW-SQLCODE                                  
                                                                        
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY   SQLCODE 000 '              
      */TEST                                                            
                IF CONCURRENCY-VALUE = CT-UNLOCK-INDICATOR  THEN        
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY  CONCURENCY VALUE '          
      *            ' = CT-UNLOCK-INDICATOR '                            
      *    DISPLAY 'SO-OTHERS-CAN-PLAY SET TO TRUE '                    
      */TEST                                                            
                   SET SO-OTHERS-CAN-PLAY  TO TRUE                      
                ELSE                                                    
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY RESOURCES LOCKED '           
      *    DISPLAY 'SO-RESOURCES-ARE-LOCKED TO TRUE '                   
      */TEST                                                            
                   SET SO-RESOURCES-ARE-LOCKED TO TRUE                  
                END-IF                                                  
           WHEN SO-SQLCODE-NOT-FOUND                                    
                                                                        
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY   SQLCODE 100 '              
      *    DISPLAY 'ZZEC0210-O-RC-CONCUR-ERROR TO TRUE  '               
      *    DISPLAY 'MESSAGE MOVED '                                     
      */TEST                                                            
                                                                        
      * THIS SHOULD NOT HAPPEN                                          
                SET ZZEC0210-O-RC-CONCUR-ERROR TO TRUE                  
                MOVE ' CUNCURRENCY DATABASE TABLE ERROR ' TO            
                                                ZZEC0210-O-ERROR-MESSAGE
                MOVE WS-ZZEC0210 TO DFHCOMMAREA        
                                                                        
                PERFORM 3000-FINAL                                      
                                                                        
           WHEN OTHER                                                   
      *TEST                                                             
      *    DISPLAY '7700-CHECK-CONCURRENCY   SQLCODE OTHER '            
      */TEST                                                            
                SET SO-7700-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7800-LOCK-THE-RESOURCES                       
      * WE NEED TO LOCK RESOURCES ONLY FOR THIS TRANSACTOIN             
      * IN ORDER TO DO THAT WE WILL MODIFY REKORD IN CONCURRENCY_TABLE  
      * 'Y' MEANS THAT RESOURCES ARE LOCKED                             
      * 'N' MEANS THAT RESOURCES ARE NOT LOCKED                         
      *                                                                 
      * WE ARE GOING TO LOCK THE RESOURCES SO WE WILL USE 'Y'           
      ******************************************************************
       7800-LOCK-THE-RESOURCES.                                         
      *TEST                                                             
      *    DISPLAY '7800-LOCK-THE-RESOURCES '                           
      */TEST                                                            
           MOVE  1  TO CONCURRENCY-ID                                   
           MOVE CT-LOCK-INDICATOR  TO CONCURRENCY-VALUE                 
                                                                        
           EXEC SQL                                                     
           UPDATE CONCURRENCY_TABLE                                     
           SET CONCURRENCY_VALUE = :CONCURRENCY-VALUE                   
           WHERE CONCURRENCY_ID = :CONCURRENCY-ID                       
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145    TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK THEN                                    
      *TEST   
      *    DISPLAY '7800-LOCK-THE-RESOURCES  SQLCODE OTHER'             
      */TEST                                                            
              SET SO-7800-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           ELSE                                                         
      *TEST                                                             
      *    DISPLAY '7800-LOCK-THE-RESOURCES  SQLCODE OK '               
      *    DISPLAY '7800-LOCK-THE-RESOURCES  SYNCPOINT '                
      */TEST                                                            
             EXEC CICS                                                  
             SYNCPOINT                                                  
             END-EXEC                                                   
             PERFORM 2200-CHECK-EIBRESP                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    7900-UNLOCK-RESOURCES                        
      ******************************************************************
       7900-UNLOCK-RESOURCES.                                           
      *TEST                                                             
      *    DISPLAY '7900-UNLOCK-RESOURCES '                             
      */TEST                                                            
           MOVE 1 TO CONCURRENCY-ID                                     
           MOVE CT-UNLOCK-INDICATOR  TO CONCURRENCY-VALUE               
           EXEC SQL                                                     
           UPDATE CONCURRENCY_TABLE                                     
           SET   CONCURRENCY_VALUE = :CONCURRENCY-VALUE                 
           WHERE CONCURRENCY_ID    = :CONCURRENCY-ID                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
      *    MOVE -145    TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-OK  THEN                                   
      *TEST                                                             
      *    DISPLAY '7900-UNLOCK-RESOURCES  SQLCODE OTHER'               
      */TEST                                                            
              SET SO-7900-PARA TO TRUE  
              PERFORM 9000-DB2-ERROR                                    
           ELSE                                                         
              DISPLAY ' 7900 SUCCESSFULL UPDATE STATEMETN SQLCODE 000 ' 
              DISPLAY ' 7900 NO ACTION IS TAKEN '                       
           END-IF                                                       
           .                                                            
      ***************************************************************** 
      *                                                                 
      *                    9000-DB2-ERROR                               
      *                                                                 
      ***************************************************************** 
      *                                                                 
       9000-DB2-ERROR.                                                  
      *TEST                                                             
      *    DISPLAY '9000-DB2-ERROR           '                          
      */TEST                                                            
           DISPLAY 'DB2 ERROR'                                          
           MOVE      SW-SQLCODE TO WS-SQLCODE-FORMAT                    
           DISPLAY 'SQLCODE '      WS-SQLCODE-FORMAT                    
           DISPLAY 'SQLERRMC '     SQLERRMC                             
           DISPLAY 'ST. IDENTIFICATOR ' SW-ST-IDENTIFICATOR             
                                                                        
           SET ZZEC0210-O-RC-DB2-ERROR  TO TRUE                         
                                                                        
           STRING ' DB2 ERROR IN STATMENT '                             
           SW-ST-IDENTIFICATOR                                          
           DELIMITED BY SIZE                                            
           INTO ZZEC0210-O-ERROR-MESSAGE                                
           END-STRING                                                   
                                                                        
           PERFORM 9100-ROLLBACK                                        
           PERFORM 3500-DB2-ERROR-FINAL                                 
           .                                                            
      ******************************************************************
      *                    9100-ROLLBACK                                
      ******************************************************************
       9100-ROLLBACK.                                                   
      *TEST                                                             
      *    DISPLAY '9000-ROLLBACK            '                          
      */TEST                                                            
           EXEC CICS                                                    
           SYNCPOINT ROLLBACK                                           
           END-EXEC                                                     
           .                                                            
                                
                                                          
                 
                  
                              

                                                      
                                            

                                             
                                                     
                   
                   
                                  
                       
                                                 
                           

          
                                                         

   

         
                                                        
                                     
                                                                       
                                                      
                  
                                                                        
                                                       
                         
                                                   
                                                                        
                                  

                             
            
           

                              
                        
                                                      
                                                                       
