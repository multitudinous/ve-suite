#ifndef EXECUTE_THREAD_H
#define EXECUTE_THREAD_H

#include <ace/Task.h>

#include "moduleS.h"

class Body_Executive_i;

class Execute_Thread : public ACE_Task_Base {

public:

  Execute_Thread (Body::Unit_var m, Body_Executive_i* ex);
  ~Execute_Thread ();

  virtual int svc (void);

  int lock();
  int unlock();
  int needexecute ();

private:

  Body::Unit_var _mod;

  bool _is_exec;

  Body_Executive_i *_executive;

  ACE_Thread_Mutex _mutex;

};

#endif
