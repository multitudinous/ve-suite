#include "cfdSwitch.h"
#include  <iostream>
#ifdef _PERFORMER
#include <Performer/pf/pfSwitch.h>
#endif
//////////////////////////////////
//Constructors                  //
//////////////////////////////////
cfdSwitch::cfdSwitch()
:cfdGroup()
{
#ifdef _PERFORMER
   _switch = new pfSwitch();
   if ( _group != NULL )
   {
      pfDelete( _group );
      _group = NULL;
   }
   _group = dynamic_cast<pfSwitch*>(_switch);
#elif _OSG
   _switch = new osg::Switch();
   _switch->setAllChildrenOff();
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
#elif _OPENSG
#endif
}
//////////////////////////////////////////////
void cfdSwitch::SetVal(int whichChildIsActive)
{
   if(_switch){
#ifdef _PERFORMER
      if(whichChildIsActive == OFF)
         _switch->setVal(-1);
      else
         _switch->setVal(whichChildIsActive);
#elif _OSG
      if(whichChildIsActive == OFF)
         _switch->setAllChildrenOff();
      else
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
