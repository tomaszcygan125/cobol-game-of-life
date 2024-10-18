 IDENTIFICATION DIVISION.                                         
 PROGRAM-ID. Z02101.                                              
******************************************************************
*                                                                 
*       PROGRAM ALLOW USER TO PLAY IN 'GAME IN LIFE'              
* PROGRAM PERFORM THE TASK BELOW:                                 
*  1. DISPAY THE GAME'S MAP                                       
*  2. ADD A SHAPE TO THE MAP                                      
*  3. RESET THE MAP                                               
*  4. PLAY THE GAME                                               
*                                                                 
*  TO ALLOW ALL OF THAT PROGRAM WILL CALL ROUTINE NAMED           
*   Z02112                                                        
*                                                                 
*                                                                 
*                                                                 
*                      CHANGE LOG                                 
******************************************************************
*                                                                 
*                                                                 
*                                                                 
*                                                                 
******************************************************************
 DATA DIVISION.                                                   
 WORKING-STORAGE SECTION.                                         
     COPY DFHAID.                                                 
     COPY ZZMP0210.                                               
* SWITCHES                                                        
 01 SW-SWITCHES.                                                  
     05 SW-WHAT-SEND-TYPE        PIC X.                           
          88 SO-SEND-WHOLE-MAP   VALUE 'M'.                       
          88 SO-SEND-ONLY-DATA   VALUE 'D'.                       
     05 SW-USER-CHOICE           PIC X.                           
          88 SO-DISPLAY-CURRENT-MAP VALUE '1'.                    
          88 SO-ADD-A-SHAPE-TO-MAP  VALUE '2'.                    
          88 SO-RESET-THE-GAME-MAP  VALUE '3'.  
          88 SO-START-THE-GAME      VALUE '4'.                    
     05 SW-INPUT-CORRECT            PIC X.                        
          88 SO-INPUT-CORRECT       VALUE 'Y'.                    
          88 SO-INPUT-NOT-CORRECT   VALUE 'N'.                    
 01 CT-MAPFAIL-MESSAGE   PIC X(25)                                
                                VALUE 'YOU NEED TO MAKE A CHOICE'.
 01 CT-3100-ERROR-MESSAGE   PIC X(16) VALUE '3100-FINAL-ERROR'.   
 01 CT-INVALID-CHOICE-MESSAGE PIC X(23) VALUE                     
                                    'THERE IS NO SUCH CHOICE'.    
 01 CT-DB2-ERROR-MESSAGE   PIC X(20) VALUE 'END DUE TO DB2 ERROR'.
 01 CT-END-FINAL-MESSAGE  PIC X(16) VALUE ' END OF PROGRAM '.     
* COMMAREA                                                        
     COPY ZZEC0210.                                               
 LINKAGE SECTION.                                                 
 01 DFHCOMMAREA PIC X(108).                                       
***************************************************************   
*                 PROCEDURE DIVISION                              
***************************************************************   
                                                                  
 PROCEDURE DIVISION USING DFHCOMMAREA.                            
     PERFORM 2000-PROCESS                                         
     PERFORM 3000-FINAL                                           
     .                                                            
***************************************************************   
*                    1010-FIRST-TIME-RUN                          
*  INITILIZING THE MAP AND SENDING IT TO THE USER                 
***************************************************************   
 1010-FIRST-TIME-RUN.                                             
     MOVE LOW-VALUES TO MP0210O                                   
     SET SO-SEND-WHOLE-MAP TO TRUE                                
     PERFORM 2100-SEND-THE-MAP                                    
     .                                                            
***************************************************************   
*                      1015-IGNORE                                
***************************************************************   
 1015-IGNORE.   
     EXEC CICS                                                  
     IGNORE CONDITION ERROR                                     
     END-EXEC                                                   
     .                                                          
 2000-PROCESS.                                                  
     IF EIBCALEN = 0 THEN                                       
          PERFORM 1010-FIRST-TIME-RUN                           
          PERFORM 1015-IGNORE                                   
          EXEC CICS                                             
          RETURN TRANSID('0206') COMMAREA(WS-ZZEC0210)          
          END-EXEC                                              
     ELSE                                                       
       MOVE DFHCOMMAREA TO WS-ZZEC0210                          
                                                                
       EVALUATE EIBAID                                          
                                                                
* IF USER PRESSED ENTER WE WILL TAKE DATA HE PROVIDED           
* AND BESED ON THAT PROGRAM WILL DO SOME ACTION                 
         WHEN DFHENTER                                          
                                                                
            PERFORM 2300-DATA-FROM-USER                         
                                                                
* EVALUATE THRU USER CHOICES                                    
                                                                
            EVALUATE TRUE                                       
                WHEN SO-DISPLAY-CURRENT-MAP                     
                                                                
                  PERFORM 2310-DISPLAY-CURRENT-MAP              
                                                                
                WHEN SO-ADD-A-SHAPE-TO-MAP                      
                                                                
                  PERFORM 2320-ADD-A-SHAPE                      
                                                                
                WHEN SO-RESET-THE-GAME-MAP                      
                                                                
                  PERFORM 2330-RESET-THE-GAME                            
                                                                  
                WHEN SO-START-THE-GAME                            
                                                                  
                  PERFORM 2340-START-THE-GAME                     
                                                                  
                WHEN OTHER                                        
                                                                  
                  PERFORM 2350-INVALID-CHOICE                     
                                                                  
              END-EVALUATE                                        
         WHEN DFHPF1                                              
                                                                  
            PERFORM 2050-CLEAR-THE-SCREEN                         
                                                                  
         WHEN DFHPF3                                              
* END OF TRANSACTION                                              
            PERFORM 3000-FINAL                                    
         WHEN OTHER                                               
                                                                  
* USER PRESSED KEY THAT DON'T HAVE ANY ACTION                     
            MOVE 'NO-ACITON KEY' TO MSGO                          
            SET SO-SEND-ONLY-DATA  TO TRUE                        
            PERFORM 2100-SEND-THE-MAP                             
                                                                  
       END-EVALUATE                                               
     END-IF                                                       
                                                                  
     MOVE WS-ZZEC0210 TO DFHCOMMAREA                              
                                                                  
     EXEC CICS                                                    
     RETURN TRANSID('0206') COMMAREA(DFHCOMMAREA)                 
     END-EXEC                                                     
     .                                                            
***************************************************************   
*                2050-CLEAR-THE-SCREEN                            
***************************************************************             
 2050-CLEAR-THE-SCREEN.                                           
     MOVE    LOW-VALUES              TO MP0210O                   
     SET     SO-SEND-WHOLE-MAP       TO TRUE                      
     PERFORM 2100-SEND-THE-MAP                                    
     .                                                            
***************************************************************   
*                2100-SEND-THE-MAP                                
***************************************************************   
 2100-SEND-THE-MAP.                                               
     EVALUATE TRUE                                                
     WHEN SO-SEND-WHOLE-MAP                                       
        EXEC CICS                                                 
        SEND MAP('MP0210') MAPSET('MP0210')                       
        FROM(MP0210O)                                             
        ERASE                                                     
        END-EXEC                                                  
     WHEN SO-SEND-ONLY-DATA                                       
        EXEC CICS                                                 
        SEND MAP('MP0210') MAPSET('MP0210')                       
        FROM(MP0210O)                                             
        DATAONLY                                                  
        FREEKB                                                    
        ERASEAUP                                                  
        END-EXEC                                                  
     WHEN OTHER                                                   
        MOVE 'ERROR IN MAP SEND' TO MSGO                          
        SET SO-SEND-ONLY-DATA TO TRUE                             
        PERFORM 2100-SEND-THE-MAP                                 
     END-EVALUATE                                                 
     PERFORM 2200-CHECK-EIBRESP                                   
     .                                                            
***************************************************************   
*                  2200-CHECK-EIBRESP                             
***************************************************************   
 2200-CHECK-EIBRESP.                                              
     EVALUATE EIBRESP
       WHEN DFHRESP(NORMAL)                                       
       WHEN DFHRESP(EOC)                                          
            CONTINUE                                              
       WHEN DFHRESP(MAPFAIL)                                      
            MOVE CT-MAPFAIL-MESSAGE TO MSGO                       
            SET SO-SEND-ONLY-DATA   TO TRUE                       
            PERFORM 2100-SEND-THE-MAP                             
       WHEN OTHER                                                 
            DISPLAY 'TSOUS02 OTHER ERROR'                         
            PERFORM 3100-FINAL-WITH-ERROR                         
     END-EVALUATE                                                 
     .                                                            
***************************************************************   
*                   2300-DATA-FROM-USER                           
***************************************************************   
 2300-DATA-FROM-USER.                                             
     MOVE LOW-VALUES TO MP0210I                                   
     EXEC CICS                                                    
     RECEIVE MAP('MP0210') MAPSET('MP0210')                       
     INTO(MP0210I)                                                
     NOHANDLE                                                     
     END-EXEC                                                     
     PERFORM 2200-CHECK-EIBRESP                                   
     MOVE CHOICEO TO SW-USER-CHOICE                               
     .                                                            
******************************************************************
*                2310-DISPLAY-CURRENT-MAP                         
******************************************************************
 2310-DISPLAY-CURRENT-MAP.                                        
     SET ZZEC0210-M-DISPLAY-MAP TO TRUE                           
     PERFORM 2323-CALL-ROUTINE                                    
     .                                                            
******************************************************************
*                   2320-ADD-A-SHAPE                              
*                                                                 
******************************************************************
 2320-ADD-A-SHAPE.                                                
     INSPECT NAMEOFSI REPLACING ALL '_' BY ' '                    
     MOVE NAMEOFSI TO ZZEC0210-I-SHAPE-NAME                       
                                                                  
     INSPECT POS-XI REPLACING ALL '_' BY ' '                      
     INSPECT POS-YI REPLACING ALL '_' BY ' '                      
                                                                  
     IF POS-XI IS NUMERIC AND                                     
        POS-YI IS NUMERIC THEN                                    
                                                                  
        MOVE POS-XI TO ZZEC0210-I-POSITION-X                      
        MOVE POS-YI TO ZZEC0210-I-POSITION-Y                      
        SET  ZZEC0210-M-ADD-A-SHAPE TO TRUE                       
        PERFORM 2323-CALL-ROUTINE                                 
     ELSE                                                         
        PERFORM 2321-INVALID-DATA-MESSAGE                         
     END-IF                                                       
     .                                                            
***************************************************************   
*                2321-INVALID-DATA-MESSAGE                        
***************************************************************   
 2321-INVALID-DATA-MESSAGE.                                       
     MOVE 'INVALID DATA ' TO MSGO                                 
     SET SO-SEND-WHOLE-MAP TO TRUE                                
     PERFORM 2100-SEND-THE-MAP                                    
     .                                                            
***************************************************************   
*                  2323-CALL-ROUTINE                              
***************************************************************   
 2323-CALL-ROUTINE.                                               
     EXEC CICS                                                    
     LINK PROGRAM('Z02112') COMMAREA(WS-ZZEC0210)                 
     END-EXEC                                                     
                                                                  
     EVALUATE TRUE                                                
       WHEN ZZEC0210-O-RC-NORMAL 
         MOVE 'NORMAL -> SUCCESS ' TO MSGO                        
         SET SO-SEND-ONLY-DATA TO TRUE                            
         PERFORM 2100-SEND-THE-MAP                                
       WHEN ZZEC0210-O-RC-SUCCESS-DISPLAY                         
                                                                  
         CONTINUE                                                 
                                                                  
       WHEN ZZEC0210-O-RC-SUCCESS-SHAPE                           
                                                                  
         CONTINUE                                                 
                                                                  
       WHEN ZZEC0210-O-RC-SUCCESS-PLAY                            
                                                                  
         CONTINUE                                                 
                                                                  
       WHEN ZZEC0210-O-RC-SUCCESS-RESET                           
         MOVE 'RESET SUCCESSFULL ' TO MSGO                        
         SET SO-SEND-WHOLE-MAP TO TRUE                            
         PERFORM 2100-SEND-THE-MAP                                
                                                                  
       WHEN OTHER                                                 
          MOVE ZZEC0210-O-ERROR-MESSAGE TO MSGO                   
          SET SO-SEND-ONLY-DATA TO TRUE                           
          PERFORM 2100-SEND-THE-MAP                               
     END-EVALUATE                                                 
                                                                  
     INITIALIZE WS-ZZEC0210                                       
     .                                                            
                                                                  
***************************************************************   
*                   2330-RESET-THE-GAME                           
***************************************************************   
 2330-RESET-THE-GAME.                                             
     SET ZZEC0210-M-RESET-THE-GAME TO TRUE                        
     PERFORM 2323-CALL-ROUTINE                                    
     .   
***************************************************************  
*                   2340-START-THE-GAME                          
***************************************************************  
 2340-START-THE-GAME.                                            
     SET ZZEC0210-M-START-THE-GAME TO TRUE                       
     PERFORM 2323-CALL-ROUTINE                                   
     .                                                           
***************************************************************  
*                 2350-INVALID-CHOICE                            
***************************************************************  
 2350-INVALID-CHOICE.                                            
     MOVE CT-INVALID-CHOICE-MESSAGE TO MSGO                      
     SET SO-SEND-ONLY-DATA           TO TRUE                     
     PERFORM 2100-SEND-THE-MAP                                   
     .                                                           
***************************************************************  
*                 3000-FINAL                                     
***************************************************************  
 3000-FINAL.                                                     
     EXEC CICS                                                   
     SEND TEXT FROM(CT-END-FINAL-MESSAGE)                        
     ERASE                                                       
     END-EXEC                                                    
                                                                 
     EXEC CICS                                                   
     RETURN                                                      
     END-EXEC                                                    
     .                                                           
***************************************************************  
*                 3100-FINAL-WITH-ERROR                          
***************************************************************  
 3100-FINAL-WITH-ERROR.                                          
     EXEC CICS                                                   
     SEND TEXT FROM(CT-3100-ERROR-MESSAGE)                       
     ERASE                                                       
     END-EXEC 
     EXEC CICS     
     RETURN        
     END-EXEC      
     .                                                                
                                                         
                                 

                                             
                                                  
                  