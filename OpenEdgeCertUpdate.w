&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

using OpenEdge.Core.Memptr.
using OpenEdge.Core.STRING.
using OpenEdge.Net.HTTP.ClientBuilder.
using OpenEdge.Net.HTTP.IHttpClient.
using OpenEdge.Net.HTTP.RequestBuilder.
using OpenEdge.Net.HTTP.HttpClient.
using OpenEdge.Net.HTTP.IHttpRequest.
using OpenEdge.Net.HTTP.IHttpResponse.
using OpenEdge.Net.HTTP.Credentials.
USING OpenEdge.Net.HTTP.HttpHeader FROM PROPATH.
USING OpenEdge.Core.ByteBucket FROM PROPATH.
using System.*.
using System.Security.Cryptography.X509Certificates.*.
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE STREAM sImport.

DEFINE VARIABLE DLCDirectory AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE ttCARootCert NO-UNDO
    FIELD certName AS CHARACTER
    FIELD PEMCERT  AS CLOB.

&GLOBAL-DEFINE xMasterCACertFile cacert.pem

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCACertificatesURL bntRefesh edInfo ~
slCAList bnImportCert BUTTON-1 edStatus 
&Scoped-Define DISPLAYED-OBJECTS fiCACertificatesURL edInfo slCAList ~
edStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bnImportCert 
     LABEL "Import Cert" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bntRefesh 
     LABEL "Download" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-1 
     LABEL "Cert Info" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiCACertificatesURL AS CHARACTER FORMAT "X(256)":U 
     LABEL "CA Bundle Source" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt","https://raw.githubusercontent.com/gisle/mozilla-ca/master/lib/Mozilla/CA/cacert.pem","https://curl.haxx.se/ca/cacert.pem" 
     DROP-DOWN-LIST
     SIZE 80.5 BY 1 NO-UNDO.

DEFINE VARIABLE edInfo AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 99 BY 4 NO-UNDO.

DEFINE VARIABLE edStatus AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 99 BY 5.75 NO-UNDO.

DEFINE VARIABLE slCAList AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 99 BY 11.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiCACertificatesURL AT ROW 1.5 COL 19 COLON-ALIGNED WIDGET-ID 16
     bntRefesh AT ROW 1.5 COL 103 WIDGET-ID 4
     edInfo AT ROW 2.75 COL 3 NO-LABEL WIDGET-ID 6 NO-TAB-STOP 
     slCAList AT ROW 7.75 COL 3 NO-LABEL WIDGET-ID 8
     bnImportCert AT ROW 7.75 COL 103 WIDGET-ID 10
     BUTTON-1 AT ROW 9 COL 103 WIDGET-ID 18
     edStatus AT ROW 19.5 COL 3 NO-LABEL WIDGET-ID 12
     "CA Trusted Root Certificate Authorities." VIEW-AS TEXT
          SIZE 35 BY .75 AT ROW 7 COL 3.5 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165.13 BY 27.69 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "OpenEdge CA Root Certificate Import"
         HEIGHT             = 24.56
         WIDTH              = 119.13
         MAX-HEIGHT         = 27.66
         MAX-WIDTH          = 165.25
         VIRTUAL-HEIGHT     = 27.66
         VIRTUAL-WIDTH      = 165.25
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edInfo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       edStatus:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* OpenEdge CA Root Certificate Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* OpenEdge CA Root Certificate Import */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bnImportCert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bnImportCert C-Win
ON CHOOSE OF bnImportCert IN FRAME DEFAULT-FRAME /* Import Cert */
DO:
  RUN importCACertificates IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bntRefesh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bntRefesh C-Win
ON CHOOSE OF bntRefesh IN FRAME DEFAULT-FRAME /* Download */
DO:
  RUN getLatestCACertificates IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Cert Info */
DO:
  RUN getCACertificateInfo IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  
  RUN initialise IN THIS-PROCEDURE.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiCACertificatesURL edInfo slCAList edStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiCACertificatesURL bntRefesh edInfo slCAList bnImportCert BUTTON-1 
         edStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCACertificateInfo C-Win 
PROCEDURE getCACertificateInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE iEntry AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEntry AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCAList AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOSCommand AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cImportLine AS CHARACTER   NO-UNDO.
    
    
    DEFINE VARIABLE cert AS CLASS X509Certificate  NO-UNDO.
    DEFINE VARIABLE cCertInfo     AS CHARACTER   NO-UNDO.

    
    &SCOPED-DEFINE xCACertificateFile  CACertificate.crt
    
    
    DO WITH FRAME {&FRAME-NAME}:
    
        cCAList = slCAList:SCREEN-VALUE.
    
        IF cCAList EQ ? THEN
        DO:
            MESSAGE "No Certificate Selected."  
                VIEW-AS ALERT-BOX INFO.
            RETURN.
        END.
        
        ASSIGN
            cEntry = ENTRY(1, cCAList).
            
           
        
        FOR FIRST ttCARootCert
            WHERE ttCARootCert.certName EQ cEntry:
                
            COPY-LOB FROM ttCARootCert.PEMCERT TO FILE "{&xCACertificateFile}" .       
                
            cImportLine = "{&xCACertificateFile}".
                
            cert = NEW X509Certificate(cImportLine).

            cCertInfo = cert:ToString(true).

            MESSAGE  cCertInfo
                VIEW-AS ALERT-BOX INFO.
            
        END.
        
        STATUS DEFAULT "".
        
    END.
    
    RETURN.
    
    FINALLY:
        SESSION:SET-WAIT-STATE("").
        OS-DELETE "{&xCACertificateFile}".
        
        IF VALID-OBJECT(cert) THEN
            DELETE OBJECT cert.
            
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLatestCACertificates C-Win 
PROCEDURE getLatestCACertificates PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE oRequest  AS IHttpRequest  NO-UNDO.
    DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
    
    
    
    def    var      oClobData  as CLASS STRING NO-UNDO.
    def    var      oContent   as ByteBucket.
    
    DEFINE VARIABLE cURLEndPoint AS CHARACTER   NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
    
        cURLEndPoint = fiCACertificatesURL:SCREEN-VALUE.
        
        oRequest = RequestBuilder:GET( cURLEndPoint ):Request.
        
        oResponse = ClientBuilder:Build():Client:Execute(oRequest).
        
/*         MESSAGE                     */
/*             oResponse:StatusCode    */
/*             oResponse:StatusReason. */
           
        IF oResponse:StatusCode EQ 200 THEN
        DO:
            oContent = new ByteBucket().
            oClobData = cast(oResponse:Entity, OpenEdge.Core.STRING).
            
            /*oClobData = oContent:GetBytes().*/
            
            COPY-LOB FROM oClobData:value TO FILE "{&xMasterCACertFile}".
            
        END.
        
        RUN parseCACertPEMFile IN THIS-PROCEDURE.
        
    END.

    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importCACertificates C-Win 
PROCEDURE importCACertificates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE iEntry AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEntry AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCAList AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOSCommand AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cImportLine AS CHARACTER   NO-UNDO.
    
    &SCOPED-DEFINE xCACertificateFile  CACertificate.crt
    
    
    DO WITH FRAME {&FRAME-NAME}:
    
        SESSION:SET-WAIT-STATE("GENERAL").
    
        cCAList = slCAList:SCREEN-VALUE.
        
        edStatus:SCREEN-VALUE = "".
        
        IF cCAList EQ ? THEN
        DO:
            MESSAGE "No Certificate Selected."  
                VIEW-AS ALERT-BOX INFO.
            RETURN.
        END.
        
        DO iEntry = 1 TO NUM-ENTRIES(cCAList):
        
            PROCESS EVENTS.
        
            ASSIGN
                cEntry = ENTRY(iEntry, cCAList).
                        
            FOR FIRST ttCARootCert
                WHERE ttCARootCert.certName EQ cEntry:
                
                COPY-LOB FROM ttCARootCert.PEMCERT TO FILE "{&xCACertificateFile}" .       
                
                cOSCommand = SUBSTITUTE("&1\bin\certutil -format PEM -import {&xCACertificateFile}", DLCDirectory).
                
                STATUS DEFAULT SUBSTITUTE("Importing &1",cEntry).
                
                INPUT STREAM sImport THROUGH VALUE(cOSCommand) NO-ECHO.
                REPEAT:
                
                    cImportLine = "".
                    
                    IMPORT STREAM sImport UNFORMATTED cImportLine.
                    
                    edStatus:MOVE-TO-EOF().
                    
                    IF cImportLine EQ "" THEN
                        NEXT.
                    
                    edStatus:INSERT-STRING(cImportLine + '~n').
                    
                END.
                INPUT STREAM sImport CLOSE.
                
                STATUS DEFAULT SUBSTITUTE("Imported &1", cEntry).
                
            END.
            
        END.
        
        SESSION:SET-WAIT-STATE("").
        
        STATUS DEFAULT "".
        
        MESSAGE "Finshed Importing CA Certificate(s)"
            VIEW-AS ALERT-BOX INFO.
    END.
    
    RETURN.
    
        
    
    FINALLY:
        SESSION:SET-WAIT-STATE("").
        OS-DELETE "{&xCACertificateFile}".
    END FINALLY.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialise C-Win 
PROCEDURE initialise PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
    
        GET-KEY-VALUE SECTION 'Startup' KEY 'DLC' VALUE DLCDirectory.
            
        fiCACertificatesURL:SCREEN-VALUE = fiCACertificatesURL:ENTRY( 1 ) .
            
        RUN parseCACertPEMFile IN THIS-PROCEDURE.
    
    END.
    
    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parseCACertPEMFile C-Win 
PROCEDURE parseCACertPEMFile PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE importLine  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE importState AS INTEGER INITIAL 0 NO-UNDO.
    DEFINE VARIABLE CAPEMCert   AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE iEntry      AS INTEGER     NO-UNDO.
    
    &SCOPED-DEFINE xImportHeaderInfo  0
    &SCOPED-DEFINE xImportCertName    1 
    &SCOPED-DEFINE xImportCertContent 2
    
    
    IF SEARCH("{&xMasterCACertFile}") EQ ? THEN
        RUN getLatestCACertificates IN THIS-PROCEDURE.
        
    DO WITH FRAME {&FRAME-NAME}:
    
        edInfo:SCREEN-VALUE = "".
        
        /** Clear the selection list**/
        DO iEntry = 1 TO slCAList:NUM-ITEMS:
           slCAList:DELETE (iEntry).
        END.
        
        IF SEARCH("{&xMasterCACertFile}") EQ ? THEN
        DO:
            MESSAGE "Missing {&xMasterCACertFile}. Import Failed.".
            RETURN.
        END.
        
        EMPTY TEMP-TABLE ttCARootCert.
        
        INPUT STREAM sImport FROM "{&xMasterCACertFile}" NO-ECHO NO-CONVERT.
        
        REPEAT:
        
            ASSIGN 
                importLine = "".
        
            IMPORT STREAM sImport UNFORMATTED importLine.
        
            CASE importState:
                WHEN {&xImportHeaderInfo} THEN
                DO:
                
                    IF importLine BEGINS "##" THEN
                    DO:
                    
                        edInfo:MOVE-TO-EOF( ).
                        edInfo:INSERT-STRING( importLine + "~r~n").
                        
                    END.
                    
                    IF importLine EQ "" THEN
                    DO:
                        edInfo:SET-SELECTION ( 1 , 1 ) .
                        
                        ASSIGN
                            importState = {&xImportCertName}.
                            
                        NEXT.
                    END.
                    
                END.
                WHEN {&xImportCertName} THEN
                DO:
                    
                    /** Skip blank lines**/
                    IF importLine EQ "" THEN
                        NEXT.
                
                    CREATE ttCARootCert.
                    
                    ASSIGN
                        ttCARootCert.certName = importLine.
                    
                    /** Add the Cert name to the secletion List. **/
                    slCAList:ADD-LAST( ttCARootCert.certName ).
                                         
                    ASSIGN
                        importState = {&xImportCertContent}.
                END.
                WHEN {&xImportCertContent} THEN
                DO:
                
                    /** Skip the underlines **/                
                    IF importLine BEGINS "=" THEN
                        NEXT.
                        
                    IF importLine EQ "" THEN
                    DO:
                        ASSIGN
                            importState = {&xImportCertName}.
                        NEXT.
                    END.
                        
                
                    IF importLine EQ "-----BEGIN CERTIFICATE-----" THEN
                        ASSIGN
                            CAPEMCert = "".
                    
                     ASSIGN
                        CAPEMCert = CAPEMCert + importLine + '~n'.
                    
                    
                    IF importLine EQ "-----END CERTIFICATE-----" THEN
                        COPY-LOB FROM CAPEMCert TO ttCARootCert.PEMCERT.
                    
                END.
                
            END CASE.
        
        END.
        
        INPUT STREAM sImport CLOSE.
        
        /*TEMP-TABLE  ttCARootCert:WRITE-XML("FILE",'cacert.xml',TRUE).*/
    
    END.

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

