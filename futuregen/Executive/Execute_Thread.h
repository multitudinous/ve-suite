#ifndef EXECUTE_THREAD_H
#define EXECUTE_THREAD_H

#include <ace/Task.h>

#include "moduleS.h"

class Execute_Thread : public ACE_Task_Base {

public:

  Execute_Thread (Body::Unit_var m);
  ~Execute_Thread ();

  virtual int svc (void);

  int lock();
  int unlock();
  int needexecute ();

private:

  Body::Unit_var _mod;

  bool _is_exec;

  ACE_Thread_Mutex _mutex;

};

#endif
