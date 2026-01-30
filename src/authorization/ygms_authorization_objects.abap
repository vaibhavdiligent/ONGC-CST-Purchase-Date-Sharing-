*&---------------------------------------------------------------------*
*& Authorization Objects Definition
*& Package: YGMS
*& Description: Authorization Objects for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Authorization Object: YGMS_LOC
* Description: Location Authorization
* Class: ZGAIL (Custom Authorization Class)
*----------------------------------------------------------------------*
* Field Definitions:
* | Field | Description                | Check Table |
* |-------|----------------------------|-------------|
* | LOCID | GAIL Location ID           | YGMS_CST_LOC_MAP |
* | ACTVT | Activity                   | TACT |
*
* Activity Values:
* - 01 = Create
* - 02 = Change
* - 03 = Display
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Authorization Object: YGMS_STATE
* Description: State Authorization
* Class: ZGAIL (Custom Authorization Class)
*----------------------------------------------------------------------*
* Field Definitions:
* | Field | Description                | Check Table |
* |-------|----------------------------|-------------|
* | STATE | State Code                 | - |
* | ACTVT | Activity                   | TACT |
*
* Activity Values:
* - 01 = Create
* - 02 = Change
* - 03 = Display
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role Definitions
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role: YGMS_UPLOAD
* Description: Data Uploader Role
* Authorizations:
*   - YGMS_LOC: ACTVT = 01 (Create), all locations or specific
*   - S_TCODE: YGMS_CST_PUR
*----------------------------------------------------------------------*
* Role Profile:
*   Authorization Object: YGMS_LOC
*     LOCID: * (or specific locations)
*     ACTVT: 01
*   Authorization Object: S_TCODE
*     TCD: YGMS_CST_PUR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role: YGMS_ALLOC
* Description: Allocator Role
* Authorizations:
*   - YGMS_LOC: ACTVT = 01, 02 (Create, Change)
*   - YGMS_STATE: ACTVT = 01, 02 (Create, Change)
*   - S_TCODE: YGMS_CST_PUR
*----------------------------------------------------------------------*
* Role Profile:
*   Authorization Object: YGMS_LOC
*     LOCID: * (or specific locations)
*     ACTVT: 01, 02
*   Authorization Object: YGMS_STATE
*     STATE: * (or specific states)
*     ACTVT: 01, 02
*   Authorization Object: S_TCODE
*     TCD: YGMS_CST_PUR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role: YGMS_SEND
* Description: Data Sender Role
* Authorizations:
*   - YGMS_LOC: ACTVT = 02 (Change) - to mark as sent
*   - Email authorization
*   - S_TCODE: YGMS_CST_PUR
*----------------------------------------------------------------------*
* Role Profile:
*   Authorization Object: YGMS_LOC
*     LOCID: * (or specific locations)
*     ACTVT: 02
*   Authorization Object: S_TCODE
*     TCD: YGMS_CST_PUR
*   Authorization Object: S_BCS_SEND
*     ACTVT: 16 (Execute)
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role: YGMS_VIEW
* Description: Viewer Role (Read-Only)
* Authorizations:
*   - YGMS_LOC: ACTVT = 03 (Display)
*   - YGMS_STATE: ACTVT = 03 (Display)
*   - S_TCODE: YGMS_CST_PUR
*----------------------------------------------------------------------*
* Role Profile:
*   Authorization Object: YGMS_LOC
*     LOCID: * (or specific locations)
*     ACTVT: 03
*   Authorization Object: YGMS_STATE
*     STATE: * (or specific states)
*     ACTVT: 03
*   Authorization Object: S_TCODE
*     TCD: YGMS_CST_PUR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Role: YGMS_ADMIN
* Description: Administrator Role (Full Access)
* Authorizations:
*   - YGMS_LOC: All activities
*   - YGMS_STATE: All activities
*   - SM30 access for config tables
*   - S_TCODE: YGMS_CST_PUR, SM30
*----------------------------------------------------------------------*
* Role Profile:
*   Authorization Object: YGMS_LOC
*     LOCID: *
*     ACTVT: 01, 02, 03
*   Authorization Object: YGMS_STATE
*     STATE: *
*     ACTVT: 01, 02, 03
*   Authorization Object: S_TCODE
*     TCD: YGMS_CST_PUR, SM30
*   Authorization Object: S_TABU_DIS
*     ACTVT: 02, 03
*     DICBERCLS: &NC& (for YGMS_CST_LOC_MAP, YGMS_CST_MAT_MAP)
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* SU24 Proposal Values for Transaction YGMS_CST_PUR
*----------------------------------------------------------------------*
* | Auth Object | Field | Proposal Value |
* |-------------|-------|----------------|
* | YGMS_LOC    | LOCID | Required       |
* | YGMS_LOC    | ACTVT | Required       |
* | YGMS_STATE  | STATE | Optional       |
* | YGMS_STATE  | ACTVT | Optional       |
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Authorization Check Implementation in YGMS_CL_CST_CONTROLLER
*----------------------------------------------------------------------*
* Location Authorization Check:
*   AUTHORITY-CHECK OBJECT 'YGMS_LOC'
*     ID 'LOCID' FIELD iv_location_id
*     ID 'ACTVT' FIELD iv_activity.
*
* State Authorization Check:
*   AUTHORITY-CHECK OBJECT 'YGMS_STATE'
*     ID 'STATE' FIELD iv_state_code
*     ID 'ACTVT' FIELD iv_activity.
*----------------------------------------------------------------------*
