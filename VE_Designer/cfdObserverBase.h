#ifndef CFDOBSERVER_BASE_H
#define CFDOBSERVER_BASE_H

#include <iostream>
#include <vector>
#include "cfdSubjectBase.h"

class cfdSubjectBase;

class cfdObserverBase
{
   public:
      cfdObserverBase();
      virtual ~cfdObserverBase();
      virtual void update(cfdSubjectBase* theChangeSubject) =0;

};


#endif
