#ifndef CFD_SWITCH_H
#define CFD_SWITCH_H

#include "cfdGroup.h"

#ifdef _PERFORMER
class pfSwitch;
#elif _OSG
#include <osg/Switch>
#elif OPENSG
#endif

class cfdSwitch: public cfdGroup{
public:
   cfdSwitch();
   cfdSwitch(const cfdSwitch& cSwitch);
   virtual ~cfdSwitch();

   //probably need more functions but
   //this is all we need for now
   //the rest are inherited from group
   void setVal(int whichChildIsActive);

   cfdSwitch& operator=(const cfdSwitch& rhs);
   
protected:

};
#endif// CFD_SWITCH_H
