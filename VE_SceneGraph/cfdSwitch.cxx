#include "cfdSwitch.h"

//////////////////////////////////
//Constructors                  //
//////////////////////////////////
cfdSwitch::cfdSwitch()
:cfdGroup()
{
#ifdef _PERFORMER
   _switch = new pfSwitch();
   _group = dynamic_cast<pfSwitch*>(_switch);
#elif _OSG
   _switch = new osg::Switch();
   _group = dynamic_cast<osg::Switch*>(_switch);
#elif _OPENSG
#endif
   SetCFDNodeType(CFD_SWITCH);
   
}
//////////////////////////////////////////
cfdSwitch::cfdSwitch(const cfdSwitch& rhs)
:cfdGroup(rhs)
{
   _switch = rhs._switch;
#ifdef _PERFORMER
   _group = dynamic_cast<pfSwitch*>(_switch);
#elif _OSG
   _group = dynamic_cast<osg::Switch*>(_switch);
#elif _OPENSG
#endif
   SetCFDNodeType(CFD_SWITCH);
}
///////////////////////
cfdSwitch::~cfdSwitch()
{
#ifdef _PERFORMER
   if(_switch){
     pfDelete(_switch);
   }
#elif _OSG
   _switch->Delete();
#elif _OPENSG
#endif
}
//////////////////////////////////////////////
void cfdSwitch::setVal(int whichChildIsActive)
{
   if(_switch){
#ifdef _PERFORMER
      _switch->setVal(whichChildIsActive);
#elif _OSG
      _switch->setSingleChildOn(whichChildIsActive);
#elif _OPENSG
#endif
   }
}
/////////////////////////////////////////////////////
cfdSwitch& cfdSwitch::operator=(const cfdSwitch& rhs)
{
   if(&rhs != this){
      cfdGroup::operator=(rhs);
#ifdef _PERFORMER
      _switch = rhs._switch;
      _group = dynamic_cast<pfSwitch*>(_switch);
#elif _OSG
      _switch = rhs._switch;
      _group = dynamic_cast<osg::Switch*>(_switch);
#elif _OPENSG
#endif
     
   }
   return *this;
}
