#include "VE_CAD/CADClone.h"
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
