*&---------------------------------------------------------------------*
*& Message Class: YGMS_MSG
*& Package: YGMS
*& Description: Message Class for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Message Class Definition
* Short Text: ONGC CST Purchase Data Sharing Messages
*----------------------------------------------------------------------*

* Message 001 - Type E (Error)
* Text: File upload failed: &1
* Variables: &1 = Error description

* Message 002 - Type E (Error)
* Text: Data contains multiple fortnights. Upload single fortnight only.
* Variables: None

* Message 003 - Type E (Error)
* Text: Location mapping not found for CTP ID: &1 (Valid on &2)
* Variables: &1 = CTP ID, &2 = Valid date

* Message 004 - Type E (Error)
* Text: Material mapping not found for ONGC Material: &1 Location: &2
* Variables: &1 = ONGC Material Code, &2 = Location ID

* Message 005 - Type E (Error)
* Text: GCV/NCV not available for Gas Day &1 Location &2
* Variables: &1 = Gas Day, &2 = Location ID

* Message 006 - Type W (Warning)
* Text: Data already exists for selected period. Overwrite?
* Variables: None

* Message 007 - Type S (Success)
* Text: Data uploaded successfully. &1 records processed.
* Variables: &1 = Number of records

* Message 008 - Type S (Success)
* Text: Allocation completed. &1 states allocated.
* Variables: &1 = Number of states

* Message 009 - Type E (Error)
* Text: Validation failed: Allocation total &1 does not match supply &2
* Variables: &1 = Allocation total, &2 = Supply total

* Message 010 - Type S (Success)
* Text: Data saved successfully. GAIL ID: &1
* Variables: &1 = GAIL Transaction ID

* Message 011 - Type E (Error)
* Text: Authorization failed for Location: &1
* Variables: &1 = Location ID

* Message 012 - Type E (Error)
* Text: Authorization failed for State: &1
* Variables: &1 = State Code

* Message 013 - Type S (Success)
* Text: Email sent successfully to &1
* Variables: &1 = Email address

* Message 014 - Type E (Error)
* Text: Email transmission failed: &1
* Variables: &1 = Error description

* Message 015 - Type I (Information)
* Text: Processing completed: &1 success, &2 errors
* Variables: &1 = Success count, &2 = Error count

* Message 016 - Type I (Information)
* Text: Audit log entry created: &1
* Variables: &1 = Log ID

* Message 017 - Type E (Error)
* Text: Mapping validity expired for &1 on date &2
* Variables: &1 = Mapping type, &2 = Date

*----------------------------------------------------------------------*
* Message Definition in SAP Format (for SE91 import)
*----------------------------------------------------------------------*
* MSGCLASS YGMS_MSG
*   001 E File upload failed: &1
*   002 E Data contains multiple fortnights. Upload single fortnight only.
*   003 E Location mapping not found for CTP ID: &1 (Valid on &2)
*   004 E Material mapping not found for ONGC Material: &1 Location: &2
*   005 E GCV/NCV not available for Gas Day &1 Location &2
*   006 W Data already exists for selected period. Overwrite?
*   007 S Data uploaded successfully. &1 records processed.
*   008 S Allocation completed. &1 states allocated.
*   009 E Validation failed: Allocation total &1 does not match supply &2
*   010 S Data saved successfully. GAIL ID: &1
*   011 E Authorization failed for Location: &1
*   012 E Authorization failed for State: &1
*   013 S Email sent successfully to &1
*   014 E Email transmission failed: &1
*   015 I Processing completed: &1 success, &2 errors
*   016 I Audit log entry created: &1
*   017 E Mapping validity expired for &1 on date &2
* ENDMSGCLASS
*----------------------------------------------------------------------*
