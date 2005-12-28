#include "VE_Open/VE_XML/VE_CAD/CADClone.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////////////////////////////////////////
//Constructor                                                   //
//////////////////////////////////////////////////////////////////
CADClone::CADClone(DOMDocument* rootDocument,std::string name,
                 VE_CAD::CADNode* originalNode)
:VE_CAD::CADNode(rootDocument,name)
{
   _originalNode = originalNode;
}
/////////////////////
//Destructor       //
/////////////////////
CADClone::~CADClone()
{
}
//////////////////////////////////////////////////
void CADClone::_updateVEElement(std::string input)
{
}
/////////////////////////////////////////////////////
void CADClone::SetObjectFromXMLData( DOMNode* xmlNode)
{
}
///////////////////////////////////////
CADClone::CADClone(const CADClone& rhs)
:VE_CAD::CADNode(rhs)
{
   _originalNode = rhs._originalNode;
}
///////////////////////////////////////////////////
CADClone& CADClone::operator=(const CADClone& rhs)
{
   if ( this != &rhs )
   {
      VE_CAD::CADNode::operator =(rhs);
      _originalNode = rhs._originalNode;
   }
   return *this;
}