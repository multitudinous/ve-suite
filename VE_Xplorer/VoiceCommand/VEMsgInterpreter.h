#ifndef VE_MSG_INTERPRETER_H
#define VE_MSG_INTERPRETER_H

#include "VE_Installer/include/VEConfig.h"

#include <iostream>
#include <string>
#include <orbsvcs/CosNamingC.h>
#include "VEOpen/skel/moduleC.h"
#include "VEOpen/skel/VjObsC.h"

//we may want this to be a dll that gets loaded optionally later

class /* VE_VOICE_EXPORTS */ VEMsgInterpreter
{
public:
   VEMsgInterpreter(int argc, char** argv);
   VEMsgInterpreter(int argc, char** argv, VjObs_ptr server);
   virtual ~VEMsgInterpreter();

   bool IsRunning();
   void SetServer(VjObs_ptr _server);

protected:
   void _initCORBA(int argc, char** argv);
   void _shutdownCORBA();
   void _sendVoiceCommandToVE();
   
   short _numScalars;
   short _numVectors;
   short _numGeoArrays;
   int   _clients;
   int   _iso_value;
   int   _sc;
   int   _min;
   int   _max;
   long  _id;
   long  _geo_state;
   short _postdata_state;
   bool  _pre_state;
   short _timesteps;
   short _numTeacherArrays;
   short _teacher_state;
   int _numOfClientInfo;
   bool _quit;

   VjObs::obj_pd_var _clientInfoArray;
   VjObs_ptr _server;
};
#endif // VE_MSG_INTERPRETER_H