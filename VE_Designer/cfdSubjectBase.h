#ifndef CFDSUBJECTBASE_H
#define CFDSUBJECTBASE_H

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <vector.h>
#include <algorithm>
#include "cfdObserver.h"

class cfdObserverBase;
class cfdSubjectBase
{
   public:
      cfdSubjectBase();
      ~cfdSubjecBaset();
      void addObserver( cfdObserverBase* );
      void deleteObserver( cfdObserverBase* );
      void notifyObservers();
     // virtual void notifyObservers(cfdStateBuffer* );
      
   protected:
      void setChanged();
      void clearChanged();
      
   public:
      bool hasChanged();
      
   private:
      std::vector <cfdObserverBase*> _observers;
      bool statechanged;


};




#endif
