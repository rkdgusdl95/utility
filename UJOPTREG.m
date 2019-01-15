UJOPTREG    ; EHS/SWT ; 1 Jun 2010 ; 1/15/11 11:40am
    ;;;;;;Build 11
    ;
REGP01(RET,DATA)       ;"Function to register a patient
    ;"Purpose: RPC entry point
    ;"Input: RET -- PASS BY REFERENCE.  An OUT parameter
    ;"      DATA -- PASS BY REFERENCE.  Format:
    ;"      DATA(FldNUM)=Value
    ;"          also DATA("IHS",4101)
    ;"Result: none
    ;"Output: RET(0)=0 if error, 1 if success
    ;"    RET(1)=ErrNum/Message if error, or DFN if patient was added
    ;
    SET RET(0)=0
    ;checking mandatory information
    IF $G(DATA(".09"))="" SET RET(1)="E001^National ID OR SSN is not provided" QUIT
    IF $G(DATA(".01"))="" SET RET(1)="E001^Name is not defined" QUIT
    IF $G(DATA(".02"))="" SET RET(1)="E001^Sex is not defined" QUIT
    IF $G(DATA(".03"))="" SET RET(1)="E001^Date of birth is not defined" QUIT
    ;------------------------------------------------------------------------------
    NEW UJOFDA
    ; Begin Transaction
    NEW TMGTEMP SET TMGTEMP=0  ;"NOTE: Need to set to 1 when stepping through code with debugger.
    IF TMGTEMP=0 TSTART *:SERIAL
    ;
    ; Special HL7s variable to prevent A08 messages from being triggered for modified values when adding/editing data in Fileman.
    NEW DGRUGA08,VAFCA08,VAFHCA08 
    SET (DGRUGA08,VAFCA08,VAFHCA08)="" 
    ;
    SET DATA(1901)="NO" ; Veteran?
    SET DATA(.301)="NO" ; Service Connected
    SET DATA(391)="NON-VETERAN (OTHER)"  ; Type of Patient
    SET DATA(22700)=""
    ;"SET DATA(391)="GENERAL ELIGIBILITY"  ; Type of Patient
    SET DATA(.096)="`"_DUZ   ; Who entered patient
    ;
    NEW FN SET FN="" ;Field Num
    FOR  SET FN=$ORDER(DATA(FN)) QUIT:(+FN'>0)  DO
    . IF $GET(DATA(FN))="" QUIT
    . SET UJOFDA(2,"+1,",FN)=DATA(FN)
    ;
    ; the Following are fields required either by identification in the patient file 
    ; (e.g. SSN), or are neccessary for the functionaing of VistA
    ; (e.g. service connected)
    ; SET CONSTANTS
    ; -----------
    NEW UJOSSN SET UJOSSN=$GET(DATA(.09))
    SET UJOFDA(2,"+1,",.09)=UJOSSN
    ;
    NEW IEN,ERR ;Internal Entry Number Array
    DO UPDATE^DIE("E","UJOFDA","IEN","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    NEW PIEN SET PIEN=IEN(1)
    SET RET(1)=PIEN  ;"Return DFN result.  //kt
    NEW UJOELIG SET UJOELIG="EHR PATIENT"   ;"//kt changed.  was --> "GENERAL ELIGIBILITY" ; Primary Eligiblity
    NEW UJOPOS SET UJOPOS="OTHER OR NONE" ; Period of Service
    ;
    NEW UJOFDA,ERR
    SET UJOFDA(2,PIEN_",",.361)=UJOELIG
    SET UJOFDA(2,PIEN_",",.323)=UJOPOS
    DO FILE^DIE("E","UJOFDA","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    NEW UJOFDA,ERR
    NEW UJOELIGVER SET UJOELIGVER="V" ; Eligibility Verification
    SET UJOFDA(2,PIEN_",",.3611)=UJOELIGVER
    DO FILE^DIE("","UJOFDA","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    ;---------------------------------------------------------------------------------------------
    ; File Data in IHS Patient file.
    ;
    ; First, create entry on top level.
    SET ESTABLISHINGDATE=$$DT^XLFDT
    ;
    ; IHS Patient is dinummed to Patient File. So IEN to file must be the same as the IEN of patient file
    ; If IEN already exists, use it; otherwise, create it (Laygo)
    NEW UJOFDA
    SET UJOFDA(9000001,"?+"_PIEN_",",.01)=PIEN ;patient IEN (e.g. "12")
    SET UJOFDA(9000001,"?+"_PIEN_",",.02)=ESTABLISHINGDATE
    SET UJOFDA(9000001,"?+"_PIEN_",",.12)=DUZ ;logged in user IEN (e.g. "13")
    SET UJOFDA(9000001,"?+"_PIEN_",",.16)=ESTABLISHINGDATE
    ;
    NEW IEN,ERR
    ; IEN returns record number in PIEN
    DO UPDATE^DIE("","UJOFDA","IEN","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    ; Second, create health record numbers for each Medical Center Division
    NEW INSTIT,LISTERR ; Institutions, Lister Error
    ; LIST^DIC from Medical Center Division; retrieve Internal Institution Pointer
    ; Packed, Quick (follow index)
    ; Use AD Index (simple regular index on .07)
    ; Store data in INSTIT and errors in LISTERR
    D LIST^DIC(40.8,"","@;.07I","PQ","","","","AD","","","INSTIT","LISTERR")
    IF $DATA(LISTERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(LISTERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    ;Loop through results to add SSH/HRN as health record number in IHS PATIENT file
    ; S I=0 FOR  S I=$O(SAM("DILIST",I)) Q:'I  W $P(SAM("DILIST",I,0),"^",2),!
    ;"NEW UJOHRN SET UJOHRN=UJOSSN  ;HRN is going to the National ID
    NEW UJOFDA ; Fileman Data Array
    NEW UJOI SET UJOI=0 ; Loop variable
    NEW LHRFAC SET LHRFAC=0  ;"//kt  11/11/13 
    FOR  SET UJOI=$O(INSTIT("DILIST",UJOI)) QUIT:'UJOI  DO
    . NEW IENS,HRFAC
    . SET IENS="?+"_UJOI_","_PIEN_"," ; we may have multiple institutions; so we are stacking them up.
    . SET HRFAC="`"_$P(INSTIT("DILIST",UJOI,0),U,2) ; Get the institution
    . IF LHRFAC=HRFAC QUIT  ;"//kt KEEP DUPLICATE INSTITUTIONS FROM REGISTERING
    . SET LHRFAC=HRFAC   ;"//kt  11/11/13
    . SET UJOFDA(9000001.41,IENS,.01)=HRFAC
    . SET UJOFDA(9000001.41,IENS,.02)=UJOSSN  ;"Was UJOHRN 
    NEW ERR
    DO UPDATE^DIE("E","UJOFDA","IEN","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
    ;
    ; Loop through results to add Hospital Number (if provided) as a health record num in IHS PATIENT file
    NEW UJOHOSPN SET UJOHOSPN=$GET(DATA("IHS",4101))
    IF UJOHOSPN="" GOTO L2
    KILL UJOFDA ; Fileman Data Array
    SET UJOI=0 ; Loop variable
    FOR  SET UJOI=$O(INSTIT("DILIST",UJOI)) QUIT:'UJOI  DO
    . NEW IENS,HRFAC
    . SET IENS="?+"_UJOI_","_PIEN_"," ; we may have multiple institutions; so we are stacking them up.
    . SET HRFAC="`"_$P(INSTIT("DILIST",UJOI,0),U,2) ; Get the institution
    . SET UJOFDA(9000001.41,IENS,.01)=HRFAC
    . SET UJOFDA(9000001.41,IENS,.02)=UJOHOSPN
    NEW ERR
    DO UPDATE^DIE("E","UJOFDA","IEN","ERR")
    IF $DATA(ERR)>0 SET RET(0)=0 SET RET(1)="E001^"_$GET(ERR("DIERR",1,"TEXT",1)) TROLLBACK:(TMGTEMP=0)  QUIT
        ;
    ;---------------------------------------------------------------------------------------------------
    ;setting patient sensitive record
L2  NEW DGSEC,ERR,VAL
    IF $G(DATA("400000010"))="1" DO  IF VAL=0 SET RET(0)=0 SET RET(1)=$GET(ERR) TROLLBACK:(TMGTEMP=0)  QUIT
    . SET DGSEC("DFN")=PIEN
    . SET DGSEC("LEVEL")="1"
    . SET DGSEC("USER")="`"_DUZ
    . SET DGSEC("DATETIME")=ESTABLISHINGDATE
    . SET DGSEC("SOURCE")="AAC"
    . SET VAL=$$SET^UJOPSAPI(.DGSEC,.ERR)
    ;
    SET RET(0)=1 ;Success
    IF TMGTEMP=0 TCOMMIT
    ;---------------------------------------------------------------------------------------------------
    ; Last but not least... HL7 messaging 
    NEW ERR ; Garbage variable
    ; If the MAS paramter saying we can send V2.3 HL7 messages is on, go ahead and send it.
    ; HL7 protocol hard-coded into the reg routine is VAFC ADT-A04 SERVER
    ; To receive HL7 messages, subscribe to this protocol with client protocols.
    ; To receive update messages (not triggered here), subscribe to 
    ; VAFC ADT-A08 SERVER
    IF $P($$SEND^VAFHUTL(),"^",2)>0 SET ERR=$$EN^VAFCA04(PIEN,$$NOW^XLFDT) 
    QUIT
    ;
ERROR   
    I $TL>0 TRO
    D ^%ZTER
    S RET(0)=0
    S RET(1)="E999^M ERROR"
    QUIT
    ;
    ;
TMGTEST ;
     NEW DIC SET DIC=2,DIC(0)="MAEQ"
     DO ^DIC WRITE !
     IF +Y<1 QUIT
     NEW PIEN SET PIEN=+Y
     ;---------------------------------------------------------------------------------------------------
     ; Test HL7 messaging 
     NEW ERR ; Garbage variable
     ; If the MAS paramter saying we can send V2.3 HL7 messages is on, go ahead and send it.
     ; HL7 protocol hard-coded into the reg routine is VAFC ADT-A04 SERVER
     ; To receive HL7 messages, subscribe to this protocol with client protocols.
     ; To receive update messages (not triggered here), subscribe to 
     ; VAFC ADT-A08 SERVER
     NEW TEMP SET TEMP=$$SEND^VAFHUTL()
     NEW TIME SET TIME=$$NOW^XLFDT
     IF $P(TEMP,"^",2)'>0 QUIT
     SET ERR=$$EN^VAFCA04(PIEN,TIME) 
     WRITE "MESSAGE (IF ANY): '",ERR,"'",!
     QUIT

