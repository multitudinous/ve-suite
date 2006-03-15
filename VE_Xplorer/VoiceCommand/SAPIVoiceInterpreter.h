#ifndef SAPI_VOICE_INTERPRETER_H
#define SAPI_VOICE_INTERPRETER_H
#ifdef WIN32

// Prevent winsock 1 from getting loaded (sapi.h -> windows.h -> winsock.h).
// Something somewhere else uses winsock 2, and the two don't get along.
#define _WINSOCKAPI_
#include <sapi.h>
#include <sphelper.h>
#include "VEVoiceInterpreter.h"

class /* VE_VOICE_EXPORTS */ SAPIVoiceInterpreter : public VEVoiceInterpreter
{
public:
   SAPIVoiceInterpreter(int argc, char** argv);
   ~SAPIVoiceInterpreter();
   void Listen();
   bool SAPIInit();
protected:
   CComPtr<ISpRecognizer>  rec;
   CComPtr<ISpRecoGrammar> grammar;
   CComPtr<ISpRecoContext> context;
   CComPtr<ISpAudio>       audio;
   std::string cmdsFile;
   int ConfigureSAPI(std::string grammar_file);
   // SAPI can't do non-static member callbacks.
   // http://support.microsoft.com/kb/q102352/ is relevant.
   // The this pointer of a SAPIVoiceInterpreter instance can be passed in
   // the LPARAM via reinterpret_cast'ing, though, which is done here.
   // This works on Win32, and it should work on Win64 since LPARAM becomes 8 bytes.
   static void __stdcall SAPICallback(WPARAM wp, LPARAM lp);

};

#endif // WIN32
#endif // SAPI_VOICE_INTERPRETER_H