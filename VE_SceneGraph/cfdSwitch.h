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

   enum Value{OFF=-1,ON};

   //probably need more functions but
   //this is all we need for now
   //the rest are inherited from group
   void SetVal(int whichChildIsActive);
   int RemoveChild( cfdNode* child );
   int AddChild(cfdNode* child);
   int ReplaceChild(cfdNode* old,cfdNode* newNode);
   int GetNumChildren();
   void InsertChild(int index,cfdNode* node);
   
#ifdef _PERFORMER
   pfNode* GetRawNode( void );
#elif _OSG
   osg::Node* GetRawNode( void );
#elif _OPENSG
#endif

   cfdSwitch& operator=(const cfdSwitch& rhs);
   
protected:
#ifdef _PERFORMER
   pfSwitch* _switch;
#elif _OSG
   osg::ref_ptr<osg::Switch> _switch;
#elif _OPENSG
#endif

};
#endif// CFD_SWITCH_H
