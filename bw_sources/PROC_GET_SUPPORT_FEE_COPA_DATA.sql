CREATE PROCEDURE "CCEJ_VIRTUAL"."ZJ_AccountPL::GET_SUPPORT_FEE_COPA_DATA" (IN P_GJAHR NVARCHAR(4) , IN P_FISCPER NVARCHAR(3) ,IN P_LANGU NVARCHAR(1),
IN P_COMP_CODE NVARCHAR(4) ,  IN P_AC_DOC_TYP NCLOB , IN P_GL_ACCOUNT NCLOB , IN P_CHANNEL NCLOB , IN P_RPPCAT NCLOB , 
IN P_BAC NCLOB , IN P_SOURCE NVARCHAR(10),
--OUT P_RESULT_TEMP  "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER",
OUT P_RESULT  "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER" )
   LANGUAGE SQLSCRIPT
   SQL SECURITY INVOKER
   --DEFAULT SCHEMA <default_schema_name>
   AS
BEGIN
   /*************************************
       Write your procedure logic 
   *************************************/

   declare v_filter NCLOB := '' ;
   Declare P_RESULT_TEMP  "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER";
   
IF P_SOURCE IS NULL or P_SOURCE = '' THEN  
 
 P_RESULT_TEMP  =  SELECT * FROM "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER";
 
 ELSEIF P_SOURCE = 'FI' THEN 
 
 P_RESULT_TEMP  =  SELECT * FROM "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER" WHERE SOURCE = 'FI';
 
 ELSEIF P_SOURCE = 'COPA' THEN
 
 P_RESULT_TEMP  =  SELECT * FROM "CCEJ_VIRTUAL"."ZJ_SUPPORT_FEE_PER" WHERE SOURCE = 'COPA';
 
 END IF ;
 
  --and "GL_ACCOUNT" in ( :P_GL_ACCOUNT )
   --and "BAC" in ( :P_BAC )
   --and "RPPCAT" in ( :P_RPPCAT )
   --and "AC_DOC_TYP" in ( :P_AC_DOC_TYP ) ;

 v_filter := :P_AC_DOC_TYP ;
   
     if :P_GL_ACCOUNT  != ''  then
   
   if :v_filter != ''  then
   
   v_filter := v_filter || ' AND ' || :P_GL_ACCOUNT ;
   
  else 
  v_filter :=  :P_GL_ACCOUNT ;
  end if;

  end if ;
   
   
  if :P_CHANNEL  != ''  then
   
   if :v_filter != ''  then
   
  v_filter := v_filter || ' AND ' || :P_CHANNEL ;
   
  else 
  v_filter :=  :P_CHANNEL ;
  end if;

  end if ;
   
  if :P_RPPCAT  != ''  then
   
    if :v_filter != ''  then
  v_filter := v_filter || ' AND ' || :P_RPPCAT ;
   
  else 
  v_filter :=  :P_RPPCAT ;
  end if;

  end if ;
   
   
  if :P_BAC  != ''  then
   
  if :v_filter != ''  then
  v_filter := v_filter || ' AND ' || :P_BAC ;
  else 
  v_filter :=  :P_BAC ;
  end if;

  end if ;
      
 P_RESULT =  apply_filter( :P_RESULT_TEMP  , :v_filter  ) ;  
 
END;
