//JCLCOB JOB (ACCOUNT),'COBOL',NOTIFY=&SYSUID, 
//    MSGCLASS=A,CLASS=A,MSGLEVEL=(1,1),REGION=0M,TIME=(0,7) 
//**************************************************************** 
//SETP SET PGM=PROGRA01 
//**************************************************************** 
//* STEP 1 COMPILAR Y LINKEDITAR COBOL 
//**************************************************************** 
//COMPILE      EXEC IGYWCL, 
//             PARM.COBOL='DYNAM,LIB,APOST,XREF,MAP,OFFSET' 
//COBOL.SYSIN  DD DSN=FELIX.CURSO.FUENTES(&PGM),DISP=SHR 
//SYSLIB       DD DSN=FELIX.CURSO.COPY,DISP=SHR 
//STEPLIB      DD DSN=IGY420.SIGYCOMP,DISP=SHR 
//LKED.SYSLMOD DD DSN=FELIX.CURSO.LOAD(&PGM),DISP=SHR 
//**************************************************************** 
//* STEP 2 EJECUTAR 
//**************************************************************** 
//RUN      EXEC PGM=&PGM,COND=(0,GT) 
//STEPLIB  DD DSN=FELIX.CURSO.LOAD,DISP=SHR 
//**************************************************************** 
//ENTRADA DD DSN=FELIX.CURSO.ENTRADA,DISP=SHR 
//**************************************************************** 
//SYSOUT DD SYSOUT=* 
//SYSIN DD * 
/* 
// 
