FUNCTION-POOL YEHSM_EXCEL_DOWNLOAD.

* Selection parameters (set by YEHSM_ECEL_DOWN)
DATA: p_werks TYPE werks_d,
      p_autyp TYPE char40,
      p_stdat TYPE datum,
      p_rcdat TYPE datum,
      p_obsno TYPE i.

* Download work variables
DATA: lv_xml      TYPE string,
      lv_xstring  TYPE xstring,
      lv_filename TYPE string,
      lv_path     TYPE string,
      lv_fullpath TYPE string,
      lv_action   TYPE i,
      lv_size     TYPE i.

* Location table
TYPES: BEGIN OF ty_location,
         plant       TYPE werks_d,
         locid       TYPE char10,
         locdesc     TYPE char40,
       END OF ty_location.

DATA: lt_locations TYPE TABLE OF ty_location.

* Upload staging table
TYPES: BEGIN OF ty_upload_g,
         sno      TYPE i,
         plant    TYPE werks_d,
         autype   TYPE char40,
         aufrom   TYPE char20,
         rcdate   TYPE char20,
         refno    TYPE char40,
         priority TYPE char10,
         tgtdate  TYPE char20,
         location TYPE char40,
         subloc   TYPE char40,
         obstext  TYPE char1024,
         impl01   TYPE char10,
         impl02   TYPE char10,
         impl03   TYPE char10,
         impl04   TYPE char10,
         impl05   TYPE char10,
         impl06   TYPE char10,
         impl07   TYPE char10,
         impl08   TYPE char10,
         impl09   TYPE char10,
         impl10   TYPE char10,
         impl11   TYPE char10,
         impl12   TYPE char10,
         impl13   TYPE char10,
         impl14   TYPE char10,
         impl15   TYPE char10,
       END OF ty_upload_g.

DATA: gt_upload TYPE TABLE OF ty_upload_g.

* Binary download table
DATA: lt_binary TYPE STANDARD TABLE OF raw255.

*----------------------------------------------------------------------*
* FORM format_date
*   Convert DATUM to d-mmm-yyyy string (e.g. 01-Jan-2024)
*----------------------------------------------------------------------*
FORM format_date USING    pv_date   TYPE datum
                 CHANGING pv_string TYPE string.

  DATA: lv_day   TYPE string,
        lv_month TYPE string,
        lv_year  TYPE string,
        lv_mm    TYPE n LENGTH 2.

  IF pv_date IS INITIAL.
    pv_string = ''.
    RETURN.
  ENDIF.

  lv_day  = pv_date+6(2).
  lv_mm   = pv_date+4(2).
  lv_year = pv_date+0(4).

  CASE lv_mm.
    WHEN '01'. lv_month = 'Jan'.
    WHEN '02'. lv_month = 'Feb'.
    WHEN '03'. lv_month = 'Mar'.
    WHEN '04'. lv_month = 'Apr'.
    WHEN '05'. lv_month = 'May'.
    WHEN '06'. lv_month = 'Jun'.
    WHEN '07'. lv_month = 'Jul'.
    WHEN '08'. lv_month = 'Aug'.
    WHEN '09'. lv_month = 'Sep'.
    WHEN '10'. lv_month = 'Oct'.
    WHEN '11'. lv_month = 'Nov'.
    WHEN '12'. lv_month = 'Dec'.
    WHEN OTHERS. lv_month = ''.
  ENDCASE.

  pv_string = |{ lv_day }-{ lv_month }-{ lv_year }|.

ENDFORM.

*----------------------------------------------------------------------*
* FORM build_excel_xml
*   Build SpreadsheetML XML for the audit observation download template
*----------------------------------------------------------------------*
FORM build_excel_xml USING    pv_werks        TYPE werks_d
                               pv_autyp        TYPE char40
                               pv_stdate       TYPE string
                               pv_rcdate       TYPE string
                               pv_obsno        TYPE i
                               pv_loc_dropdown TYPE string
                     CHANGING pv_xml          TYPE string.

  DATA: lv_rows     TYPE string,
        lv_row      TYPE string,
        lv_i        TYPE i,
        lv_sno      TYPE string.

* Build data rows (empty template rows for the observation count)
  DO pv_obsno TIMES.
    lv_i   = sy-index.
    lv_sno = lv_i.

    lv_row = |<Row>| &&
             |<Cell><Data ss:Type="Number">{ lv_sno }</Data></Cell>| &&
             |<Cell><Data ss:Type="String">{ pv_werks }</Data></Cell>| &&
             |<Cell><Data ss:Type="String">{ pv_autyp }</Data></Cell>| &&
             |<Cell><Data ss:Type="String">{ pv_stdate }</Data></Cell>| &&
             |<Cell><Data ss:Type="String">{ pv_rcdate }</Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |<Cell><Data ss:Type="String"></Data></Cell>| &&
             |</Row>|.

    IF lv_rows IS INITIAL.
      lv_rows = lv_row.
    ELSE.
      lv_rows = lv_rows && lv_row.
    ENDIF.

  ENDDO.

  pv_xml =
    |<?xml version="1.0"?>| &&
    |<?mso-application progid="Excel.Sheet"?>| &&
    |<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"| &&
    | xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"| &&
    | xmlns:x="urn:schemas-microsoft-com:office:excel">| &&
    |<Styles>| &&
    |<Style ss:ID="s1">| &&
    |<Font ss:Bold="1"/>| &&
    |<Interior ss:Color="#FFFF00" ss:Pattern="Solid"/>| &&
    |</Style>| &&
    |<Style ss:ID="s2">| &&
    |<Interior ss:Color="#C0C0C0" ss:Pattern="Solid"/>| &&
    |</Style>| &&
    |</Styles>| &&
    |<Worksheet ss:Name="Audit_Observations">| &&
    |<Table>| &&
    |<Row>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">S.No</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Plant</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Audit Type</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Audit From</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">RC Date</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Ref No</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Priority</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Target Date</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Location</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Sub Location</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Observation</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 01</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 02</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 03</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 04</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 05</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 06</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 07</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 08</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 09</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 10</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 11</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 12</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 13</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 14</Data></Cell>| &&
    |<Cell ss:StyleID="s1"><Data ss:Type="String">Impl 15</Data></Cell>| &&
    |</Row>| &&
    lv_rows &&
    |</Table>| &&
    |</Worksheet>| &&
    |</Workbook>|.

ENDFORM.
