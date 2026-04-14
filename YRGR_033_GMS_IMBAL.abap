*&---------------------------------------------------------------------*
*& Report YRGR_033_GMS_IMBAL
*&---------------------------------------------------------------------*
* Name of Program : YRGR_033_GMS_IMBAL
* Developed By    : Ravinder Singh
* Functional      : Pratibha Dangwal
* Date            : 08.05.2024
* DESCRIPTION     : Report For Closing Imbalance Of Expired Contracts
* CHARM ID        : 2000000826 / TR NO : DVRK9A19P3
* Change  : Added Till Date radio button, Send Email checkbox, email logic
*&---------------------------------------------------------------------*
REPORT yrgr_033_gms_imbal.

INCLUDE yrgr_033_gms_imbal_top.   " Global declarations, Selection Screen, Initialization
INCLUDE yrgr_033_gms_imbal_cls.   " Class lcl_event_handler (button_click, top_of_page)
INCLUDE yrgr_033_gms_imbal_f01.   " START-OF-SELECTION + all FORMs
