call ..\VE_Installer\setup.bat

REM set VPR_DEBUG_NFY_LEVEL=3

cd %VE_SUITE_HOME%\bin\win32
VoiceCommand.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 -SAPIGrammar voice_commands.xml
cmd