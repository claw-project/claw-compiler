PROGRAM testmacro

#ifdef CLAW_ONLY
  PRINT*,'These lines'
  PRINT*,'are not ignored'
  PRINT*,'by the preprocessor.'
#else
  PRINT*,'These lines'
  PRINT*,'are ignored by the preprocessor.'
#endif

END PROGRAM testmacro
