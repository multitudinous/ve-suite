#include "cfdSubjectBase.h"

cfdSubjectBase::cfdSubjectBase()
{

}

cfdSubjectBase::~cfdSubjectBase()
{

}

void cfdSubjectBase::addObserver(cfdObserverBase* newobserver)
{
   _observers.push_back(newobserver);

}

void cfdSubjectBase::deleteObserver(cfdObserverBase* oldobserver)
{
   int count = _observers.size();
   //std::vector<cfdObserverBase* >::iterator iter;
   int iter;
   //for(iter.begin();iter<iter.end();iter++)
   for(iter=0; iter<count;iter++)
   {
      if(_observers[iter]== oldobserver)
         break;
   }

   if(iter<count)
   {
      _observers.erase(_observers.begin()+iter);
   }
  
   
}

void cfdSubjectBase::notifyObservers()
{
   //std::vector< cfdObserverBase* > :: iterator iter;
   int iter;
   //for(iter.begin();iter<iter.end();iter++)
   for(iter=0; iter<_observers.size();iter++)
   {
      _observers[iter]->update(this);

   }
}

/*void cfdSubjectBase::notifyObservers(cfdStateBuffer* statebuffer)
{
   std::vector<cfdObserverBase*>::iterator iter;

   for(iter.begin();iter<iter.end();iter++)
   {
      iter->update(this);
   }

}*/

void cfdSubjectBase::setChanged()
{
   this->statechanged = true;

}

void cfdSubjectBase::clearChanged()
{
   this->statechanged = false;
}

bool cfdSubjectBase::hasChanged()
{
   return this->statechanged;

}


