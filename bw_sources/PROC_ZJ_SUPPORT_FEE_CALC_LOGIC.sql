CREATE PROCEDURE "CCEJ_VIRTUAL"."ZJ_AccountPL.SupportFee::ZJ_SUPPORT_FEE_CALC_LOGIC" (IN IP_YEAR NVARCHAR(4), IN IP_PERIOD NVARCHAR(3),
OUT OP_RES "_SYS_BIC"."ZJ_AccountPL.SupportFee/ZJAPL_SU_FEE_FAGLFLEXA" ) 
LANGUAGE SQLSCRIPT
SQL SECURITY INVOKER 
--DEFAULT SCHEMA <default_schema_name>
READS SQL DATA AS
BEGIN
/***************************** 
Write your procedure logic 
 *****************************/
 OP_RES = 
 select * from "_SYS_BIC"."ZJ_AccountPL.SupportFee/ZJAPL_SU_FEE_FAGLFLEXA" where FISCYEAR = :IP_YEAR and FISCPER3 = :IP_PERIOD;
 
END;
