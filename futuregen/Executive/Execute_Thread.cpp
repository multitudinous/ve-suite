
#include <ace/Task.h>

#include "Execute_Thread.h"

#include <iostream>

using namespace std;

Execute_Thread::Execute_Thread (Body::Unit_var m) :
  _mod     ( m ),
  _is_exec ( false)
{

}

Execute_Thread::~Execute_Thread ()
{

}

int Execute_Thread::svc (void)
{

  while(true) {
    while(true) {
      _mutex.acquire();
      if(_is_exec) {
	break;
      }
      _mutex.release();
      
      ACE_OS::sleep(2);
      
    }
    
    _mutex.release();
    _mod->StartCalc();

    _mutex.acquire();
    _is_exec = false;
    _mutex.release();
    
  }

}

int Execute_Thread::lock ()
{
  _mutex.acquire();
}

int Execute_Thread::unlock ()
{
  _mutex.release();
}

int Execute_Thread::needexecute ()
{
  int ret = 1;

  _mutex.acquire();
  if(_is_exec == true) ret = 0;
  else                 _is_exec = true;
  _mutex.release();

  return ret;
}

