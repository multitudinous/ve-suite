#include "Executive_i.h"

#include <ace/Task.h>
#include <ace/OS.h>
#include "Execute_Thread.h"

#include <iostream>

using namespace std;

Execute_Thread::Execute_Thread (Body::Unit_var m, Body_Executive_i* ex) :
  _mod       ( m ),
  _is_exec   ( false),
  _executive ( ex )
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
      if(_is_exec) break;
      
      _mutex.release();
      
      ACE_OS::sleep(2); 	    
    }
    
    _mutex.release();
    try {
      _mod->StartCalc();
    } catch (CORBA::Exception &) {
      cout <<"Module Execution Messed up.\n";
    }
    _mutex.acquire();
    _is_exec = false;
    _mutex.release();
    
    try {
      _executive->execute_next_mod ((long)_mod->GetID());
    }catch (CORBA::Exception &) {
      cout <<"Module GetID Messed up.\n";
    }
  }
  
}

int Execute_Thread::lock ()
{
  _mutex.acquire();

  return 0;
}

int Execute_Thread::unlock ()
{
  _mutex.release();
  
  return 0;
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

