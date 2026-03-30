# Claude Code Prompt: ONGC CST Purchase Data Sharing - ABAP Code Generation
## Version 1.2 - Updated with YGMS Prefix Naming Convention & Audit Capabilities

---

## CONTEXT & ROLE

You are an expert SAP ABAP developer specializing in S/4HANA development with deep expertise in Clean ABAP principles, OO ABAP design patterns, and SAP best practices. Your task is to generate production-ready ABAP code for the **ONGC CST Purchase Data Sharing** system based on the Technical Specification Document (TSD v1.2).

---

## PROJECT INFORMATION

| Attribute | Value |
|-----------|-------|
| **Project Name** | ONGC CST Purchase Data Sharing |
| **Document ID** | TSD-GAIL-CST-PUR-001 |
| **WRICEF ID** | R-GAIL-CST-001 |
| **WRICEF Type** | Report with Interface Capabilities |
| **Client** | GAIL (India) Limited |
| **SAP Version** | S/4HANA 2021+ |
| **ABAP Style** | Clean ABAP / Modern ABAP (7.40+) |
| **Development Class** | YGMS |
| **TSD Version** | 1.2 |

---

## CRITICAL NAMING CONVENTION

**IMPORTANT:** All custom development objects in this project must use the **'YGMS' namespace prefix**. This is a client-specific naming standard for GAIL.

| Object Type | Prefix/Pattern | Example |
|-------------|----------------|---------|
| Program/Report | YGMS_ | YGMS_CST_PURCHASE_MAIN |
| Class | YGMS_CL_ | YGMS_CL_CST_CONTROLLER |
| Interface | YGMS_IF_ | YGMS_IF_CST_PROCESSOR |
| Exception Class | YGMS_CX_ | YGMS_CX_CST_ERROR |
| Function Module | YGMS_ | YGMS_ALLOCATE_PURCHASE |
| Database Table | YGMS_ | YGMS_CST_PUR, YGMS_CST_LOC_MAP |
| Structure | YGMS_S_ | YGMS_S_RECEIPT, YGMS_S_ALLOCATION |
| Table Type | YGMS_TT_ | YGMS_TT_RECEIPT, YGMS_TT_ALLOCATION |
| Data Element | YGMS_ | YGMS_GAS_DAY, YGMS_QTY_SCM |
| Domain | YGMS_ | YGMS_TAX_TYPE, YGMS_FLAG |
| Message Class | YGMS_MSG | YGMS_MSG |
| Transaction Code | YGMS_ | YGMS_CST_PUR |
| Authorization Object | YGMS_ | YGMS_LOC, YGMS_STATE |
| Number Range | YGMS | YGMS |

---

## TECHNICAL SPECIFICATION SUMMARY

### 1. Business Context

GAIL needs a system to:
1. Receive gas purchase data from ONGC (via Excel upload or B2B PI interface)
2. Allocate received gas quantities to different states based on sales data
3. Classify allocations as CST (Central Sales Tax) or VAT (Value Added Tax)
4. Share the processed data back with ONGC via email with PDF/Excel attachments
5. Maintain complete audit trail of all data changes

### 2. Architecture Pattern

The solution follows a **layered architecture** with:
- **Presentation Layer**: ALV Grid for interactive display and editing
- **Business Logic Layer**: Allocation engine with variance adjustment
- **Data Access Layer**: Database operations and mapping functions
- **Audit Layer**: Comprehensive change tracking and logging

**Design Patterns Used:**
- Factory Pattern for object instantiation
- Strategy Pattern for allocation algorithms
- Observer Pattern for audit logging

### 3. Key Features in v1.2

1. **Time-Dependent Validity**: Mapping tables support validity dates (VALID_FROM, VALID_TO)
2. **Comprehensive Audit Logging**: All custom tables include audit fields + central audit log
3. **YGMS Prefix Naming**: All objects use 'YGMS' prefix per GAIL standards

---

## DEVELOPMENT OBJECTS TO GENERATE

### A. Data Dictionary Objects

#### A.1 Domains

| Domain | Data Type | Length | Fixed Values | Description |
|--------|-----------|--------|--------------|-------------|
| YGMS_CTP_ID | CHAR | 20 | - | ONGC CTP/Terminal ID |
| YGMS_LOC_ID | CHAR | 10 | - | GAIL Location ID |
| YGMS_ONGC_MAT | CHAR | 20 | - | ONGC Material Code |
| YGMS_QTY_SCM | DEC | 15,3 | - | Quantity in SCM |
| YGMS_QTY_MBG | DEC | 15,3 | - | Quantity in MMBTU |
| YGMS_GCV | DEC | 13,3 | - | Gross Calorific Value |
| YGMS_NCV | DEC | 13,3 | - | Net Calorific Value |
| YGMS_ONGC_ID | CHAR | 9 | - | ONGC Transaction ID |
| YGMS_GAIL_ID | CHAR | 16 | - | GAIL Transaction ID |
| YGMS_STATE_CD | CHAR | 4 | - | State Code |
| YGMS_STATE | CHAR | 40 | - | State Name |
| YGMS_TAX_TYPE | CHAR | 3 | CST, VAT | Tax Type |
| YGMS_FLAG | CHAR | 1 | X, (blank) | Generic Flag |
| YGMS_LOG_ID | CHAR | 32 | - | Audit Log Entry ID (GUID) |

#### A.2 Data Elements

| Data Element | Domain | Description |
|--------------|--------|-------------|
| YGMS_CTP_ID | YGMS_CTP_ID | ONGC CTP/Terminal ID |
| YGMS_LOC_ID | YGMS_LOC_ID | GAIL Location ID |
| YGMS_ONGC_MAT | YGMS_ONGC_MAT | ONGC Material Code |
| YGMS_GAS_DAY | DATUM | Gas Day/Receipt Date |
| YGMS_QTY_SCM | YGMS_QTY_SCM | Quantity in Standard Cubic Meters |
| YGMS_QTY_MBG | YGMS_QTY_MBG | Quantity in MMBTU |
| YGMS_GCV | YGMS_GCV | Gross Calorific Value (kcal/SCM) |
| YGMS_NCV | YGMS_NCV | Net Calorific Value (kcal/SCM) |
| YGMS_ONGC_ID | YGMS_ONGC_ID | ONGC Transaction ID |
| YGMS_GAIL_ID | YGMS_GAIL_ID | GAIL Transaction ID |
| YGMS_STATE_CD | YGMS_STATE_CD | State Code (GJ, MH, etc.) |
| YGMS_STATE | YGMS_STATE | State Name |
| YGMS_TAX_TYPE | YGMS_TAX_TYPE | Tax Type (CST/VAT) |
| YGMS_EXCL_FLG | YGMS_FLAG | Exclusion Flag (X=Excluded) |
| YGMS_SENT_FLG | YGMS_FLAG | Sent Flag (X=Sent) |
| YGMS_LOG_ID | YGMS_LOG_ID | Audit Log Entry ID (GUID) |

#### A.3 Database Tables

**IMPORTANT:** All custom tables include:
- **Validity Dates** (for mapping tables): VALID_FROM, VALID_TO
- **Audit Fields**: CREATED_BY, CREATED_ON, CREATED_AT, CHANGED_BY, CHANGED_ON, CHANGED_AT

---

**Table 1: YGMS_CST_LOC_MAP** (Location Mapping with Time-Dependent Validity)

**Delivery Class:** C (Customizing Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| CTP_ID | YGMS_CTP_ID | X | ONGC Terminal ID |
| VALID_FROM | DATAB | X | Valid From Date |
| VALID_TO | DATBI | | Valid To Date |
| LOCATION_ID | YGMS_LOC_ID | | GAIL Location ID |
| DESCRIPTION | TEXT40 | | Location Description |
| CREATED_BY | ERNAM | | Created By User |
| CREATED_ON | ERDAT | | Creation Date |
| CREATED_AT | ERZET | | Creation Time |
| CHANGED_BY | AENAM | | Last Changed By |
| CHANGED_ON | AEDAT | | Last Change Date |
| CHANGED_AT | AEZET | | Last Change Time |

---

**Table 2: YGMS_CST_MAT_MAP** (Material Mapping with Time-Dependent Validity)

**Delivery Class:** C (Customizing Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| ONGC_MATERIAL | YGMS_ONGC_MAT | X | ONGC Material Code |
| LOCATION_ID | YGMS_LOC_ID | X | GAIL Location ID |
| VALID_FROM | DATAB | X | Valid From Date |
| VALID_TO | DATBI | | Valid To Date |
| GAIL_MATERIAL | MATNR | | GAIL Material Code |
| CREATED_BY | ERNAM | | Created By User |
| CREATED_ON | ERDAT | | Creation Date |
| CREATED_AT | ERZET | | Creation Time |
| CHANGED_BY | AENAM | | Last Changed By |
| CHANGED_ON | AEDAT | | Last Change Date |
| CHANGED_AT | AEZET | | Last Change Time |

---

**Table 3: YGMS_CST_B2B_1** (Receipt Data Storage with Audit)

**Delivery Class:** A (Application Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| GAS_DAY | YGMS_GAS_DAY | X | Gas Receipt Date |
| CTP_ID | YGMS_CTP_ID | X | ONGC Terminal ID |
| ONGC_MATERIAL | YGMS_ONGC_MAT | X | ONGC Material Code |
| LOCATION_ID | YGMS_LOC_ID | | GAIL Location ID (Mapped) |
| MATERIAL | MATNR | | GAIL Material Code (Mapped) |
| QTY_SCM | YGMS_QTY_SCM | | Quantity in SCM |
| QTY_MBG | YGMS_QTY_MBG | | Quantity in MMBTU |
| ONGC_ID | YGMS_ONGC_ID | | ONGC Transaction ID |
| CREATED_BY | ERNAM | | Created By User |
| CREATED_ON | ERDAT | | Creation Date |
| CREATED_AT | ERZET | | Creation Time |
| CHANGED_BY | AENAM | | Last Changed By |
| CHANGED_ON | AEDAT | | Last Change Date |
| CHANGED_AT | AEZET | | Last Change Time |

---

**Table 4: YGMS_CST_PUR** (Daily Purchase Data with Audit)

**Delivery Class:** A (Application Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| GAS_DAY | YGMS_GAS_DAY | X | Gas Receipt Date |
| LOCATION_ID | YGMS_LOC_ID | X | GAIL Location ID |
| MATERIAL | MATNR | X | GAIL Material Code |
| STATE_CODE | YGMS_STATE_CD | X | State Code |
| STATE | YGMS_STATE | | State Name |
| CTP_ID | YGMS_CTP_ID | | ONGC Terminal ID |
| ONGC_MATERIAL | YGMS_ONGC_MAT | | ONGC Material Code |
| QTY_MBG | YGMS_QTY_MBG | | Quantity in MMBTU |
| GCV | YGMS_GCV | | Gross Calorific Value |
| NCV | YGMS_NCV | | Net Calorific Value |
| QTY_SCM | YGMS_QTY_SCM | | Quantity in SCM |
| GAIL_ID | YGMS_GAIL_ID | | GAIL Transaction ID |
| TAX_TYPE | YGMS_TAX_TYPE | | CST/VAT Indicator |
| EXCLUDED | YGMS_EXCL_FLG | | Exclusion Flag |
| SENT | YGMS_SENT_FLG | | Sent Flag |
| CREATED_BY | ERNAM | | Created By User |
| CREATED_ON | ERDAT | | Creation Date |
| CREATED_AT | ERZET | | Creation Time |
| CHANGED_BY | AENAM | | Last Changed By |
| CHANGED_ON | AEDAT | | Last Change Date |
| CHANGED_AT | AEZET | | Last Change Time |

---

**Table 5: YGMS_CST_FNT_DATA** (Fortnightly Aggregate Data with Audit)

**Delivery Class:** A (Application Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| FROM_DATE | DATUM | X | Fortnight Start Date |
| TO_DATE | DATUM | X | Fortnight End Date |
| LOCATION_ID | YGMS_LOC_ID | X | GAIL Location ID |
| MATERIAL | MATNR | X | GAIL Material Code |
| STATE_CODE | YGMS_STATE_CD | X | State Code |
| TOTAL_QTY_MBG | YGMS_QTY_MBG | | Total Quantity in MMBTU |
| AVG_GCV | YGMS_GCV | | Weighted Average GCV |
| AVG_NCV | YGMS_NCV | | Weighted Average NCV |
| TOTAL_QTY_SCM | YGMS_QTY_SCM | | Total Quantity in SCM |
| GAIL_ID | YGMS_GAIL_ID | | GAIL Transaction ID |
| CREATED_BY | ERNAM | | Created By User |
| CREATED_ON | ERDAT | | Creation Date |
| CREATED_AT | ERZET | | Creation Time |
| CHANGED_BY | AENAM | | Last Changed By |
| CHANGED_ON | AEDAT | | Last Change Date |
| CHANGED_AT | AEZET | | Last Change Time |

---

**Table 6: YGMS_CST_AUDIT_LOG** (Central Audit Log Table) - NEW in v1.2

**Delivery Class:** A (Application Table)

| Field | Data Element | Key | Description |
|-------|--------------|-----|-------------|
| MANDT | MANDT | X | Client |
| LOG_ID | YGMS_LOG_ID | X | Unique Log Entry ID (GUID) |
| TABLE_NAME | TABNAME | | Table Being Changed |
| OPERATION | CHAR1 | | Operation (I=Insert, U=Update, D=Delete) |
| KEY_FIELD1 | CHAR100 | | Primary Key Value 1 |
| KEY_FIELD2 | CHAR100 | | Primary Key Value 2 |
| KEY_FIELD3 | CHAR100 | | Primary Key Value 3 |
| FIELD_NAME | FIELDNAME | | Changed Field Name |
| OLD_VALUE | CHAR255 | | Old Value |
| NEW_VALUE | CHAR255 | | New Value |
| CHANGED_BY | ERNAM | | Changed By User |
| CHANGED_ON | ERDAT | | Change Date |
| CHANGED_AT | ERZET | | Change Time |

---

#### A.4 Structures

**YGMS_S_RECEIPT** - Receipt Data Structure
```
GAS_DAY, CTP_ID, ONGC_MATERIAL, LOCATION_ID, MATERIAL, QTY_SCM, QTY_MBG, ONGC_ID
```

**YGMS_S_ALLOCATION** - Allocation Data Structure
```
GAS_DAY, LOCATION_ID, MATERIAL, STATE_CODE, STATE, SALES_QTY, ALLOC_QTY_MBG, 
ALLOC_QTY_SCM, GCV, NCV, TAX_TYPE, EXCLUDED
```

**YGMS_S_VALIDATION** - Validation Result Structure
```
LOCATION_ID, MATERIAL, ALLOC_SCM, ALLOC_MBG, SUPPLY_SCM, SUPPLY_MBG, 
DIFF_SCM, DIFF_MBG, STATUS (VS=Valid, ER=Error)
```

**YGMS_S_AUDIT** - Audit Log Entry Structure (NEW in v1.2)
```
LOG_ID, TABLE_NAME, OPERATION, KEY_FIELD1, KEY_FIELD2, KEY_FIELD3,
FIELD_NAME, OLD_VALUE, NEW_VALUE, CHANGED_BY, CHANGED_ON, CHANGED_AT
```

#### A.5 Table Types

| Table Type | Line Type | Key Type | Description |
|------------|-----------|----------|-------------|
| YGMS_TT_RECEIPT | YGMS_S_RECEIPT | Standard | Receipt data table type |
| YGMS_TT_ALLOCATION | YGMS_S_ALLOCATION | Sorted | Allocation data table type |
| YGMS_TT_VALIDATION | YGMS_S_VALIDATION | Standard | Validation results table type |
| YGMS_TT_PURCHASE | YGMS_CST_PUR | Standard | Purchase data table type |
| YGMS_TT_FNT_DATA | YGMS_CST_FNT_DATA | Standard | Fortnightly data table type |
| YGMS_TT_AUDIT_LOG | YGMS_CST_AUDIT_LOG | Standard | Audit log table type |

---

### B. Message Class

**Message Class: YGMS_MSG**

| No. | Type | Message Text |
|-----|------|--------------|
| 001 | E | File upload failed: &1 |
| 002 | E | Data contains multiple fortnights. Upload single fortnight only. |
| 003 | E | Location mapping not found for CTP ID: &1 (Valid on &2) |
| 004 | E | Material mapping not found for ONGC Material: &1 Location: &2 |
| 005 | E | GCV/NCV not available for Gas Day &1 Location &2 |
| 006 | W | Data already exists for selected period. Overwrite? |
| 007 | S | Data uploaded successfully. &1 records processed. |
| 008 | S | Allocation completed. &1 states allocated. |
| 009 | E | Validation failed: Allocation total &1 does not match supply &2 |
| 010 | S | Data saved successfully. GAIL ID: &1 |
| 011 | E | Authorization failed for Location: &1 |
| 012 | E | Authorization failed for State: &1 |
| 013 | S | Email sent successfully to &1 |
| 014 | E | Email transmission failed: &1 |
| 015 | I | Processing completed: &1 success, &2 errors |
| 016 | I | Audit log entry created: &1 |
| 017 | E | Mapping validity expired for &1 on date &2 |

---

### C. Interfaces

**YGMS_IF_CST_CONSTANTS** - Constants Interface

```abap
INTERFACE ygms_if_cst_constants PUBLIC.
  CONSTANTS:
    " Tax Types
    co_tax_cst         TYPE char3 VALUE 'CST',
    co_tax_vat         TYPE char3 VALUE 'VAT',
    
    " State Codes
    co_state_gujarat   TYPE char4 VALUE 'GJ',
    
    " Validation Status
    co_status_valid    TYPE char2 VALUE 'VS',
    co_status_error    TYPE char2 VALUE 'ER',
    
    " Flags
    co_flag_true       TYPE char1 VALUE 'X',
    co_flag_false      TYPE char1 VALUE '',
    
    " Number Range
    co_nr_object       TYPE nrobj VALUE 'YGMS',
    
    " Message Class
    co_msg_class       TYPE arbgb VALUE 'YGMS_MSG',
    
    " Audit Operations (NEW in v1.2)
    co_audit_insert    TYPE char1 VALUE 'I',
    co_audit_update    TYPE char1 VALUE 'U',
    co_audit_delete    TYPE char1 VALUE 'D',
    
    " Processing Modes
    co_mode_upload     TYPE char1 VALUE 'U',
    co_mode_allocate   TYPE char1 VALUE 'A',
    co_mode_view       TYPE char1 VALUE 'V',
    co_mode_send       TYPE char1 VALUE 'S',
    co_mode_download   TYPE char1 VALUE 'D',
    co_mode_nomination TYPE char1 VALUE 'N'.

  " Fortnight determination
  TYPES: BEGIN OF ty_fortnight,
           from_date TYPE datum,
           to_date   TYPE datum,
         END OF ty_fortnight.

ENDINTERFACE.
```

**YGMS_IF_CST_PROCESSOR** - Processing Interface

```abap
INTERFACE ygms_if_cst_processor PUBLIC.

  METHODS:
    process
      IMPORTING
        iv_mode     TYPE char1
      EXPORTING
        et_messages TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error,
        
    validate
      IMPORTING
        it_data     TYPE ygms_tt_allocation
      RETURNING
        VALUE(rv_valid) TYPE abap_bool
      RAISING
        ygms_cx_cst_validation,
        
    get_results
      RETURNING
        VALUE(rt_results) TYPE ygms_tt_purchase.

ENDINTERFACE.
```

---

### D. Exception Classes

**YGMS_CX_CST_ERROR** - Base Exception Class

```abap
CLASS ygms_cx_cst_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    CONSTANTS:
      BEGIN OF upload_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF upload_failed,
      
      BEGIN OF mapping_not_found,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_not_found,
      
      BEGIN OF gcv_ncv_missing,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gcv_ncv_missing,
      
      BEGIN OF multi_fortnight,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF multi_fortnight,
      
      BEGIN OF validation_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF validation_failed,
      
      BEGIN OF save_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_failed,
      
      BEGIN OF auth_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF auth_failed,
      
      BEGIN OF mapping_expired,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_expired.

    DATA: mv_param1 TYPE string READ-ONLY,
          mv_param2 TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        previous  LIKE previous OPTIONAL
        mv_param1 TYPE string OPTIONAL
        mv_param2 TYPE string OPTIONAL.

ENDCLASS.

CLASS ygms_cx_cst_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    me->mv_param1 = mv_param1.
    me->mv_param2 = mv_param2.
    
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

**YGMS_CX_CST_VALIDATION** - Validation Exception Class

```abap
CLASS ygms_cx_cst_validation DEFINITION
  PUBLIC
  INHERITING FROM ygms_cx_cst_error
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        previous  LIKE previous OPTIONAL
        mv_param1 TYPE string OPTIONAL
        mv_param2 TYPE string OPTIONAL.

ENDCLASS.

CLASS ygms_cx_cst_validation IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      textid    = textid
      previous  = previous
      mv_param1 = mv_param1
      mv_param2 = mv_param2
    ).
  ENDMETHOD.
ENDCLASS.
```

---

### E. Business Classes

#### E.1 Class Summary

| Class | Description | Key Methods |
|-------|-------------|-------------|
| YGMS_CL_CST_CONTROLLER | Main controller - orchestrates all processing | PROCESS_UPLOAD, EXECUTE_ALLOCATION, SAVE_DATA |
| YGMS_CL_CST_DATA_HANDLER | Data access layer for all DB operations | GET_MAPPING, SAVE_DATA, GET_DATA |
| YGMS_CL_CST_ALLOCATOR | Business logic for state-wise allocation | CALCULATE_ALLOCATION, APPLY_VARIANCE |
| YGMS_CL_CST_EXCEL_HANDLER | Excel file upload and parsing | UPLOAD_FILE, PARSE_DATA, VALIDATE |
| YGMS_CL_CST_EMAIL_SENDER | Email transmission with attachments | SEND_EMAIL, ATTACH_PDF, ATTACH_EXCEL |
| YGMS_CL_CST_ALV_HANDLER | ALV grid display and event handling | DISPLAY_ALV, HANDLE_EVENTS |
| YGMS_CL_CST_VALIDATOR | Validation logic for allocations | VALIDATE_TOTALS, CHECK_MANDATORY |
| YGMS_CL_CST_PDF_GENERATOR | PDF report generation | GENERATE_PDF, GET_PDF_CONTENT |
| YGMS_CL_CST_AUDIT_HANDLER | Audit log management (NEW in v1.2) | LOG_INSERT, LOG_UPDATE, LOG_DELETE |

#### E.2 YGMS_CL_CST_CONTROLLER - Main Controller Class

```abap
CLASS ygms_cl_cst_controller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES ygms_if_cst_processor.

    METHODS:
      constructor
        IMPORTING
          iv_location_id TYPE ygms_loc_id OPTIONAL
          iv_from_date   TYPE datum OPTIONAL
          iv_to_date     TYPE datum OPTIONAL,
          
      process_upload
        IMPORTING
          iv_file_path TYPE string
        EXPORTING
          et_messages  TYPE bapiret2_t
        RAISING
          ygms_cx_cst_error,
          
      execute_allocation
        IMPORTING
          it_excluded_states TYPE RANGE OF ygms_state_cd OPTIONAL
        EXPORTING
          et_allocation      TYPE ygms_tt_allocation
          et_messages        TYPE bapiret2_t
        RAISING
          ygms_cx_cst_error,
          
      save_data
        IMPORTING
          it_data     TYPE ygms_tt_allocation
        EXPORTING
          ev_gail_id  TYPE ygms_gail_id
          et_messages TYPE bapiret2_t
        RAISING
          ygms_cx_cst_error,
          
      send_data
        IMPORTING
          iv_email_address TYPE ad_smtpadr
        EXPORTING
          et_messages      TYPE bapiret2_t
        RAISING
          ygms_cx_cst_error.

  PRIVATE SECTION.
    DATA: mo_data_handler   TYPE REF TO ygms_cl_cst_data_handler,
          mo_allocator      TYPE REF TO ygms_cl_cst_allocator,
          mo_excel_handler  TYPE REF TO ygms_cl_cst_excel_handler,
          mo_email_sender   TYPE REF TO ygms_cl_cst_email_sender,
          mo_validator      TYPE REF TO ygms_cl_cst_validator,
          mo_pdf_generator  TYPE REF TO ygms_cl_cst_pdf_generator,
          mo_audit_handler  TYPE REF TO ygms_cl_cst_audit_handler,
          mv_location_id    TYPE ygms_loc_id,
          mv_from_date      TYPE datum,
          mv_to_date        TYPE datum.

    METHODS:
      check_authorization
        IMPORTING
          iv_location_id TYPE ygms_loc_id
          iv_state_code  TYPE ygms_state_cd OPTIONAL
          iv_activity    TYPE activ_auth
        RAISING
          ygms_cx_cst_error,
          
      determine_fortnight
        IMPORTING
          iv_date TYPE datum
        RETURNING
          VALUE(rs_fortnight) TYPE ygms_if_cst_constants=>ty_fortnight.

ENDCLASS.

CLASS ygms_cl_cst_controller IMPLEMENTATION.

  METHOD constructor.
    mv_location_id = iv_location_id.
    mv_from_date   = iv_from_date.
    mv_to_date     = iv_to_date.
    
    " Initialize handler objects
    mo_audit_handler  = NEW ygms_cl_cst_audit_handler( ).
    mo_data_handler   = NEW ygms_cl_cst_data_handler( io_audit_handler = mo_audit_handler ).
    mo_allocator      = NEW ygms_cl_cst_allocator( ).
    mo_excel_handler  = NEW ygms_cl_cst_excel_handler( ).
    mo_validator      = NEW ygms_cl_cst_validator( ).
    mo_email_sender   = NEW ygms_cl_cst_email_sender( ).
    mo_pdf_generator  = NEW ygms_cl_cst_pdf_generator( ).
  ENDMETHOD.

  METHOD process_upload.
    " Check authorization
    check_authorization(
      iv_location_id = mv_location_id
      iv_activity    = '01'  " Create
    ).
    
    " Upload and parse Excel file
    DATA(lt_receipt) = mo_excel_handler->upload_file( iv_file_path ).
    
    " Validate single fortnight
    mo_excel_handler->validate_single_fortnight( lt_receipt ).
    
    " Apply mappings
    LOOP AT lt_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>).
      <fs_receipt>-location_id = mo_data_handler->get_location_mapping(
        iv_ctp_id     = <fs_receipt>-ctp_id
        iv_valid_date = <fs_receipt>-gas_day
      ).
      
      <fs_receipt>-material = mo_data_handler->get_material_mapping(
        iv_ongc_material = <fs_receipt>-ongc_material
        iv_location_id   = <fs_receipt>-location_id
        iv_valid_date    = <fs_receipt>-gas_day
      ).
    ENDLOOP.
    
    " Save receipt data
    mo_data_handler->save_receipt_data( lt_receipt ).
    
    " Return success message
    APPEND VALUE #(
      type    = 'S'
      id      = ygms_if_cst_constants=>co_msg_class
      number  = '007'
      message_v1 = |{ lines( lt_receipt ) }|
    ) TO et_messages.
  ENDMETHOD.

  METHOD execute_allocation.
    " Implementation for allocation logic
    " ...
  ENDMETHOD.

  METHOD save_data.
    " Implementation for saving data
    " ...
  ENDMETHOD.

  METHOD send_data.
    " Implementation for sending email
    " ...
  ENDMETHOD.

  METHOD check_authorization.
    " Location authorization check
    AUTHORITY-CHECK OBJECT 'YGMS_LOC'
      ID 'LOCID' FIELD iv_location_id
      ID 'ACTVT' FIELD iv_activity.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>auth_failed
          mv_param1 = |Location { iv_location_id }|.
    ENDIF.

    " State authorization check
    IF iv_state_code IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'YGMS_STATE'
        ID 'STATE' FIELD iv_state_code
        ID 'ACTVT' FIELD iv_activity.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>auth_failed
            mv_param1 = |State { iv_state_code }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD determine_fortnight.
    DATA(lv_day) = iv_date+6(2).
    
    IF lv_day <= 15.
      " First fortnight: Day 1-15
      rs_fortnight-from_date = iv_date(6) && '01'.
      rs_fortnight-to_date   = iv_date(6) && '15'.
    ELSE.
      " Second fortnight: Day 16 - End of month
      rs_fortnight-from_date = iv_date(6) && '16'.
      
      " Calculate end of month
      DATA(lv_next_month) = iv_date.
      lv_next_month+6(2) = '01'.
      lv_next_month = lv_next_month + 32.
      lv_next_month+6(2) = '01'.
      rs_fortnight-to_date = lv_next_month - 1.
    ENDIF.
  ENDMETHOD.

  METHOD ygms_if_cst_processor~process.
    " Dispatch based on mode
    CASE iv_mode.
      WHEN ygms_if_cst_constants=>co_mode_upload.
        " Upload mode - requires file path from selection screen
        
      WHEN ygms_if_cst_constants=>co_mode_allocate.
        execute_allocation(
          IMPORTING
            et_messages = et_messages
        ).
        
      WHEN OTHERS.
        " Handle other modes
    ENDCASE.
  ENDMETHOD.

  METHOD ygms_if_cst_processor~validate.
    rv_valid = mo_validator->validate_totals( it_data ).
  ENDMETHOD.

  METHOD ygms_if_cst_processor~get_results.
    " Return processed purchase data
  ENDMETHOD.

ENDCLASS.
```

#### E.3 YGMS_CL_CST_DATA_HANDLER - Data Access Layer

```abap
CLASS ygms_cl_cst_data_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_audit_handler TYPE REF TO ygms_cl_cst_audit_handler OPTIONAL,
          
      get_location_mapping
        IMPORTING
          iv_ctp_id     TYPE ygms_ctp_id
          iv_valid_date TYPE datum DEFAULT sy-datum
        RETURNING
          VALUE(rv_location_id) TYPE ygms_loc_id
        RAISING
          ygms_cx_cst_error,
          
      get_material_mapping
        IMPORTING
          iv_ongc_material TYPE ygms_ongc_mat
          iv_location_id   TYPE ygms_loc_id
          iv_valid_date    TYPE datum DEFAULT sy-datum
        RETURNING
          VALUE(rv_gail_material) TYPE matnr
        RAISING
          ygms_cx_cst_error,
          
      save_receipt_data
        IMPORTING
          it_data TYPE ygms_tt_receipt
        RAISING
          ygms_cx_cst_error,
          
      save_purchase_data
        IMPORTING
          it_data TYPE ygms_tt_purchase
        EXPORTING
          ev_gail_id TYPE ygms_gail_id
        RAISING
          ygms_cx_cst_error,
          
      get_receipt_data
        IMPORTING
          iv_from_date   TYPE datum
          iv_to_date     TYPE datum
          iv_location_id TYPE ygms_loc_id OPTIONAL
        RETURNING
          VALUE(rt_data) TYPE ygms_tt_receipt,
          
      get_gcv_ncv
        IMPORTING
          iv_gas_day     TYPE datum
          iv_location_id TYPE ygms_loc_id
        EXPORTING
          ev_gcv         TYPE ygms_gcv
          ev_ncv         TYPE ygms_ncv
        RAISING
          ygms_cx_cst_error.

  PRIVATE SECTION.
    DATA: mo_audit_handler TYPE REF TO ygms_cl_cst_audit_handler.

ENDCLASS.

CLASS ygms_cl_cst_data_handler IMPLEMENTATION.

  METHOD constructor.
    mo_audit_handler = io_audit_handler.
  ENDMETHOD.

  METHOD get_location_mapping.
    " Get location mapping with validity check
    SELECT SINGLE location_id
      FROM ygms_cst_loc_map
      WHERE ctp_id     = @iv_ctp_id
        AND valid_from <= @iv_valid_date
        AND valid_to   >= @iv_valid_date
      INTO @rv_location_id.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>mapping_not_found
          mv_param1 = |{ iv_ctp_id }|
          mv_param2 = |{ iv_valid_date DATE = USER }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_material_mapping.
    " Get material mapping with validity check
    SELECT SINGLE gail_material
      FROM ygms_cst_mat_map
      WHERE ongc_material = @iv_ongc_material
        AND location_id   = @iv_location_id
        AND valid_from   <= @iv_valid_date
        AND valid_to     >= @iv_valid_date
      INTO @rv_gail_material.
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>mapping_not_found
          mv_param1 = |{ iv_ongc_material }|
          mv_param2 = |{ iv_location_id }|.
    ENDIF.
  ENDMETHOD.

  METHOD save_receipt_data.
    " Prepare data with audit fields
    DATA(lt_db_data) = VALUE ygms_tt_receipt( ).
    
    LOOP AT it_data INTO DATA(ls_data).
      APPEND VALUE #(
        BASE ls_data
        created_by = sy-uname
        created_on = sy-datum
        created_at = sy-uzeit
      ) TO lt_db_data.
    ENDLOOP.
    
    " Insert data
    INSERT ygms_cst_b2b_1 FROM TABLE @lt_db_data.
    
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>save_failed
          mv_param1 = 'YGMS_CST_B2B_1'.
    ENDIF.
    
    " Log audit entries
    IF mo_audit_handler IS BOUND.
      LOOP AT lt_db_data INTO ls_data.
        mo_audit_handler->log_insert(
          iv_table_name = 'YGMS_CST_B2B_1'
          is_new_data   = ls_data
          iv_key1       = |{ ls_data-gas_day }|
          iv_key2       = |{ ls_data-ctp_id }|
          iv_key3       = |{ ls_data-ongc_material }|
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD save_purchase_data.
    " Implementation for saving purchase data
  ENDMETHOD.

  METHOD get_receipt_data.
    SELECT *
      FROM ygms_cst_b2b_1
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND ( @iv_location_id IS INITIAL OR location_id = @iv_location_id )
      INTO TABLE @rt_data.
  ENDMETHOD.

  METHOD get_gcv_ncv.
    " Get GCV/NCV from existing table (YRXA_CMDATA or similar)
    " This is a placeholder - actual table name needs confirmation
    SELECT SINGLE gcv, ncv
      FROM yrxa_cmdata
      WHERE gas_day     = @iv_gas_day
        AND location_id = @iv_location_id
      INTO (@ev_gcv, @ev_ncv).
      
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>gcv_ncv_missing
          mv_param1 = |{ iv_gas_day DATE = USER }|
          mv_param2 = |{ iv_location_id }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

#### E.4 YGMS_CL_CST_AUDIT_HANDLER - Audit Log Management (NEW in v1.2)

```abap
CLASS ygms_cl_cst_audit_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      log_insert
        IMPORTING
          iv_table_name TYPE tabname
          is_new_data   TYPE any
          iv_key1       TYPE string OPTIONAL
          iv_key2       TYPE string OPTIONAL
          iv_key3       TYPE string OPTIONAL,
          
      log_update
        IMPORTING
          iv_table_name TYPE tabname
          is_old_data   TYPE any
          is_new_data   TYPE any
          iv_key1       TYPE string OPTIONAL
          iv_key2       TYPE string OPTIONAL
          iv_key3       TYPE string OPTIONAL,
          
      log_delete
        IMPORTING
          iv_table_name TYPE tabname
          is_old_data   TYPE any
          iv_key1       TYPE string OPTIONAL
          iv_key2       TYPE string OPTIONAL
          iv_key3       TYPE string OPTIONAL,
          
      get_audit_log
        IMPORTING
          iv_table_name TYPE tabname OPTIONAL
          iv_from_date  TYPE datum OPTIONAL
          iv_to_date    TYPE datum OPTIONAL
        RETURNING
          VALUE(rt_log) TYPE ygms_tt_audit_log.

  PRIVATE SECTION.
    METHODS:
      generate_log_id
        RETURNING
          VALUE(rv_log_id) TYPE ygms_log_id,
          
      save_log_entry
        IMPORTING
          is_log_entry TYPE ygms_s_audit.

ENDCLASS.

CLASS ygms_cl_cst_audit_handler IMPLEMENTATION.

  METHOD log_insert.
    DATA(ls_log) = VALUE ygms_s_audit(
      log_id      = generate_log_id( )
      table_name  = iv_table_name
      operation   = ygms_if_cst_constants=>co_audit_insert
      key_field1  = iv_key1
      key_field2  = iv_key2
      key_field3  = iv_key3
      changed_by  = sy-uname
      changed_on  = sy-datum
      changed_at  = sy-uzeit
    ).
    
    save_log_entry( ls_log ).
  ENDMETHOD.

  METHOD log_update.
    " Compare old and new data and log field-level changes
    DATA: lo_old_descr TYPE REF TO cl_abap_structdescr.
          
    lo_old_descr ?= cl_abap_typedescr=>describe_by_data( is_old_data ).
    
    LOOP AT lo_old_descr->components INTO DATA(ls_component).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_old_data TO FIELD-SYMBOL(<fs_old>).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_new_data TO FIELD-SYMBOL(<fs_new>).
      
      IF <fs_old> IS ASSIGNED AND <fs_new> IS ASSIGNED.
        IF <fs_old> <> <fs_new>.
          DATA(ls_log) = VALUE ygms_s_audit(
            log_id      = generate_log_id( )
            table_name  = iv_table_name
            operation   = ygms_if_cst_constants=>co_audit_update
            key_field1  = iv_key1
            key_field2  = iv_key2
            key_field3  = iv_key3
            field_name  = ls_component-name
            old_value   = |{ <fs_old> }|
            new_value   = |{ <fs_new> }|
            changed_by  = sy-uname
            changed_on  = sy-datum
            changed_at  = sy-uzeit
          ).
          
          save_log_entry( ls_log ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD log_delete.
    DATA(ls_log) = VALUE ygms_s_audit(
      log_id      = generate_log_id( )
      table_name  = iv_table_name
      operation   = ygms_if_cst_constants=>co_audit_delete
      key_field1  = iv_key1
      key_field2  = iv_key2
      key_field3  = iv_key3
      changed_by  = sy-uname
      changed_on  = sy-datum
      changed_at  = sy-uzeit
    ).
    
    save_log_entry( ls_log ).
  ENDMETHOD.

  METHOD get_audit_log.
    SELECT *
      FROM ygms_cst_audit_log
      WHERE ( @iv_table_name IS INITIAL OR table_name = @iv_table_name )
        AND ( @iv_from_date IS INITIAL OR changed_on >= @iv_from_date )
        AND ( @iv_to_date IS INITIAL OR changed_on <= @iv_to_date )
      ORDER BY changed_on DESCENDING, changed_at DESCENDING
      INTO TABLE @rt_log.
  ENDMETHOD.

  METHOD generate_log_id.
    " Generate GUID for log entry
    TRY.
        rv_log_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        " Fallback to timestamp-based ID
        rv_log_id = |{ sy-datum }{ sy-uzeit }{ sy-uname }|.
    ENDTRY.
  ENDMETHOD.

  METHOD save_log_entry.
    DATA(ls_db_entry) = CORRESPONDING ygms_cst_audit_log( is_log_entry ).
    ls_db_entry-mandt = sy-mandt.
    
    INSERT ygms_cst_audit_log FROM @ls_db_entry.
  ENDMETHOD.

ENDCLASS.
```

---

### F. Main Program

**Program: YGMS_CST_PURCHASE_MAIN**

```abap
*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*&---------------------------------------------------------------------*
*& Description: ONGC CST Purchase Data Sharing - Main Program
*& Author:      [To be filled]
*& Date:        [Creation date]
*& Module:      FI/SD
*& WRICEF ID:   R-GAIL-CST-001
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main MESSAGE-ID ygms_msg.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: go_controller TYPE REF TO ygms_cl_cst_controller,
      go_alv_handler TYPE REF TO ygms_cl_cst_alv_handler.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    rb_upld  RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,
    rb_alloc RADIOBUTTON GROUP mode,
    rb_view  RADIOBUTTON GROUP mode,
    rb_send  RADIOBUTTON GROUP mode,
    rb_dwnld RADIOBUTTON GROUP mode,
    rb_nom   RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_loc    TYPE ygms_loc_id OBLIGATORY,
    p_frdat  TYPE datum,
    p_todat  TYPE datum.
  SELECT-OPTIONS:
    s_mat    FOR ygms_cst_pur-material.
  PARAMETERS:
    p_file   TYPE string LOWER CASE MODIF ID upl.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_exst   FOR ygms_cst_pur-state_code NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc.
  PERFORM f4_location.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default dates to current fortnight
  DATA(lv_day) = sy-datum+6(2).
  IF lv_day <= 15.
    p_frdat = sy-datum(6) && '01'.
    p_todat = sy-datum(6) && '15'.
  ELSE.
    p_frdat = sy-datum(6) && '16'.
    " Calculate end of month
    DATA(lv_next_month) = sy-datum.
    lv_next_month+6(2) = '01'.
    lv_next_month = lv_next_month + 32.
    lv_next_month+6(2) = '01'.
    p_todat = lv_next_month - 1.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Initialize controller
  go_controller = NEW ygms_cl_cst_controller(
    iv_location_id = p_loc
    iv_from_date   = p_frdat
    iv_to_date     = p_todat
  ).

  TRY.
      DATA: lt_messages   TYPE bapiret2_t,
            lt_allocation TYPE ygms_tt_allocation.

      CASE abap_true.
        WHEN rb_upld.
          " Upload mode
          go_controller->process_upload(
            EXPORTING iv_file_path = p_file
            IMPORTING et_messages  = lt_messages
          ).
          PERFORM display_messages USING lt_messages.
          
        WHEN rb_alloc.
          " Allocation mode
          go_controller->execute_allocation(
            EXPORTING it_excluded_states = s_exst[]
            IMPORTING et_allocation      = lt_allocation
                      et_messages        = lt_messages
          ).
          
          " Display ALV for editing
          go_alv_handler = NEW ygms_cl_cst_alv_handler( ).
          go_alv_handler->display_allocation( lt_allocation ).
          
        WHEN rb_view.
          " View mode - Display existing data in ALV
          
        WHEN rb_send.
          " Send mode - Send data via email
          
        WHEN rb_dwnld.
          " Download mode - Download data to Excel
          
        WHEN rb_nom.
          " Nomination mode - Display purchase nominations
          
      ENDCASE.

    CATCH ygms_cx_cst_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

*----------------------------------------------------------------------*
* Forms
*----------------------------------------------------------------------*
FORM f4_location.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.
  
  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'YGMS_CST_LOC_MAP'
      fieldname  = 'LOCATION_ID'
    TABLES
      return_tab = lt_return.
      
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_loc = ls_return-fieldval.
  ENDIF.
ENDFORM.

FORM f4_file.
  DATA: lt_file TYPE filetable,
        lv_rc   TYPE i.
        
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter = '*.xlsx;*.xls'
    CHANGING
      file_table  = lt_file
      rc          = lv_rc
  ).
  
  READ TABLE lt_file INTO DATA(ls_file) INDEX 1.
  IF sy-subrc = 0.
    p_file = ls_file-filename.
  ENDIF.
ENDFORM.

FORM modify_screen.
  LOOP AT SCREEN.
    " Show file path only for upload mode
    IF screen-group1 = 'UPL' AND rb_upld <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM display_messages USING it_messages TYPE bapiret2_t.
  LOOP AT it_messages INTO DATA(ls_message).
    CASE ls_message-type.
      WHEN 'E'.
        WRITE: / icon_led_red AS ICON, ls_message-message.
      WHEN 'W'.
        WRITE: / icon_led_yellow AS ICON, ls_message-message.
      WHEN 'S' OR 'I'.
        WRITE: / icon_led_green AS ICON, ls_message-message.
    ENDCASE.
  ENDLOOP.
ENDFORM.
```

---

### G. Authorization Objects

**YGMS_LOC** - Location Authorization

| Field | Description |
|-------|-------------|
| LOCID | GAIL Location ID |
| ACTVT | Activity (01=Create, 02=Change, 03=Display) |

**YGMS_STATE** - State Authorization

| Field | Description |
|-------|-------------|
| STATE | State Code |
| ACTVT | Activity (01=Create, 02=Change, 03=Display) |

**Role Definitions:**

| Role | Description | Authorization |
|------|-------------|---------------|
| YGMS_UPLOAD | Data Uploader | YGMS_LOC: ACTVT=01, S_TCODE: YGMS_CST_PUR |
| YGMS_ALLOC | Allocator | YGMS_LOC: ACTVT=01,02, YGMS_STATE: ACTVT=01,02 |
| YGMS_SEND | Data Sender | YGMS_LOC: ACTVT=02, Email authorization |
| YGMS_VIEW | Viewer | YGMS_LOC: ACTVT=03, YGMS_STATE: ACTVT=03 |
| YGMS_ADMIN | Administrator | All authorizations + SM30 for config tables |

---

### H. Unit Test Classes

```abap
*----------------------------------------------------------------------*
* Test Class for YGMS_CL_CST_ALLOCATOR
*----------------------------------------------------------------------*
CLASS ltcl_cst_allocator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: go_environment TYPE REF TO if_osql_test_environment.
    
    DATA: mo_cut TYPE REF TO ygms_cl_cst_allocator.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      test_allocation_single_state FOR TESTING,
      test_allocation_multi_state FOR TESTING,
      test_allocation_with_variance FOR TESTING,
      test_allocation_excluded_state FOR TESTING,
      test_gujarat_vat_classification FOR TESTING.

ENDCLASS.


CLASS ltcl_cst_allocator IMPLEMENTATION.

  METHOD class_setup.
    go_environment = cl_osql_test_environment=>create(
      VALUE #( ( 'YGMS_CST_B2B_1' ) ( 'YGMS_CST_PUR' ) )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    go_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW ygms_cl_cst_allocator( ).
  ENDMETHOD.

  METHOD teardown.
    go_environment->clear_doubles( ).
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_allocation_single_state.
    " Given: Receipt data for single location
    " When: Allocation is executed
    " Then: All quantity assigned to Gujarat with VAT classification
    
    cl_abap_unit_assert=>assert_true(
      act = abap_true
      msg = 'Single state allocation should succeed'
    ).
  ENDMETHOD.

  METHOD test_gujarat_vat_classification.
    " Given: Allocation for Gujarat state
    " When: Tax type is determined
    " Then: Tax type should be VAT (not CST)
    
    DATA(lv_tax_type) = mo_cut->determine_tax_type( iv_state_code = 'GJ' ).
    
    cl_abap_unit_assert=>assert_equals(
      act = lv_tax_type
      exp = ygms_if_cst_constants=>co_tax_vat
      msg = 'Gujarat should have VAT tax type'
    ).
  ENDMETHOD.

  METHOD test_allocation_multi_state.
    " Test allocation across multiple states
    cl_abap_unit_assert=>assert_true(
      act = abap_true
      msg = 'Multi-state allocation should succeed'
    ).
  ENDMETHOD.

  METHOD test_allocation_with_variance.
    " Test variance adjustment logic
    cl_abap_unit_assert=>assert_true(
      act = abap_true
      msg = 'Variance adjustment should work correctly'
    ).
  ENDMETHOD.

  METHOD test_allocation_excluded_state.
    " Test excluded states have zero allocation
    cl_abap_unit_assert=>assert_true(
      act = abap_true
      msg = 'Excluded states should have zero allocation'
    ).
  ENDMETHOD.

ENDCLASS.
```

---

## ACTIVATION SEQUENCE

1. Domains (YGMS_*)
2. Data Elements (YGMS_*)
3. Structures (YGMS_S_*)
4. Table Types (YGMS_TT_*)
5. Database Tables (YGMS_CST_*)
6. Interfaces (YGMS_IF_*)
7. Exception Classes (YGMS_CX_*)
8. Business Classes (YGMS_CL_*)
9. Programs (YGMS_*)
10. Transaction Codes (YGMS_*)
11. Authorization Objects (YGMS_*)
12. Message Class (YGMS_MSG)

---

## POST-GENERATION CHECKLIST

```markdown
[ ] Code compiles without syntax errors
[ ] All objects follow YGMS naming conventions
[ ] Header comments are complete
[ ] All methods have ABAP Doc documentation
[ ] Exception handling is implemented
[ ] Authorization checks are in place
[ ] Audit logging is implemented for all data changes
[ ] Unit tests cover positive and negative cases
[ ] Performance best practices followed
[ ] Clean ABAP principles applied
[ ] Time-dependent validity logic works correctly
[ ] Code is ready for transport
```

---

## ADDITIONAL NOTES

1. **Sales FM Integration:** The sales data function module name needs to be confirmed with the client. Use a placeholder interface that can be easily updated.

2. **GCV/NCV Source:** The calorific values are read from existing table YRXA_CMDATA. Confirm the exact field names with the client.

3. **Email Recipients:** Email recipient configuration should be stored in a customizing table or retrieved from a configuration class.

4. **Number Range:** Number range object YGMS needs to be created manually using SNRO transaction.

5. **Table Maintenance:** Generate table maintenance dialogs (SM30) for YGMS_CST_LOC_MAP and YGMS_CST_MAT_MAP customizing tables.

6. **Audit Log Retention:** Define a retention policy for the YGMS_CST_AUDIT_LOG table and implement periodic cleanup if required.

---

*End of Claude Code Prompt - Version 1.2 with YGMS Prefix*
