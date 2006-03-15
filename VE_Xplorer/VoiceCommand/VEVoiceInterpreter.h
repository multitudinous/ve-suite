#ifndef VE_VOICE_INTERPRETER_H
#define VE_VOICE_INTERPRETER_H

#include "VEMsgInterpreter.h"

// Base class for a voice interpreter,
// in case something other than SAPI gets used.

class /*VE_VOICE_EXPORTS*/ VEVoiceInterpreter : public VEMsgInterpreter
{
public:
   VEVoiceInterpreter(int argc, char** argv);
   virtual ~VEVoiceInterpreter() {;}

   virtual void Listen() = 0;
   // Listen() Implementations should invoke ProcessPhrase to send commands.
   bool ProcessPhrase(std::string phrase);

protected:

   void SetStepMode();
   void SetGlideMode();
   void StopMoving();
   void Quit();
   void SendLevelCmd();
   void SendPointCmd();
   void SendPickCmd();
   void SendResetCmd();
   void SendReleaseCmd();
   void InterpretMoveMessage(std::string navMsg);
   void InterpretStepMessage(std::string navMsg);
   
   bool _glide;
   bool _step;
};

#endif // VE_VOICE_INTERPRETER_H