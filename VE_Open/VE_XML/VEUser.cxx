#include "VE_Open/VE_XML/VEUser.h"
#include "VE_Open/VE_XML/VEStateInfo.h"

using namespace VE_XML;
////////////////
//Constructors//
////////////////
VEUser::VEUser(DOMDocument* rootDoc)
:VEXMLObject(rootDoc)
{
   _stateInfo = 0;
   _userId = std::string("VEUser_0");
   _controlStatus = std::string("MASTER");

}
/////////////////
VEUser::~VEUser()
{
   
}
//////////////////////////////////////
void VEUser::SetUserId(std::string id)
{
   _userId = id;
}
/////////////////////////////////////////////////////////
void VEUser::SetControlStatus(VEControlStatus cs)
{
   _controlStatus = cs;
}
////////////////////////////////////////////////////////
void VEUser::SetStateInfo(VE_XML::VEStateInfo* userState)
{
   _stateInfo = userState;
}
///////////////////////////////
std::string VEUser::GetUserId()
{
   return _userId;
}
//////////////////////////////////////////////////
VEUser::VEControlStatus VEUser::GetControlStatus()
{
   return _controlStatus;
}
///////////////////////////////////////////////
VE_XML::VEStateInfo* VEUser::GetUserStateInfo()
{
   return _stateInfo;
}
////////////////////////////////////////////////////////
void VEUser::SetObjectFromXMLData(DOMNode* xmlInput)
{
}
//////////////////////////////
void VEUser::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString( input ));
      
   }
   //Be sure to set the number of children either here or in the updating subElements code
   //we know this to be 3: 
   //stateInfo element;
   //userId element;
   //controlStatus element;
   _nChildren = 3;

   //Add code here to update the specific sub elements

}

