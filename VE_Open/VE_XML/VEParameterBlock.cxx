#include "VE_XML/VEParameterBlock.h"
using namespace VE_XML;
#include "VE_XML/VETransform.h"
#include "VE_XML/VEDataValuePair.h"


#include <xercesc/dom/DOM.hpp>
#include <iostream>
////////////////////////////////////////////////////
VEParameterBlock::VEParameterBlock(DOMDocument* rootDoc,unsigned int id)
:VEXMLObject(rootDoc)
{
   _id = id;
   _dcs = new VETransform( rootDoc );
}
/////////////////////////////////////
VEParameterBlock::~VEParameterBlock()
{
   delete _dcs;
   _dcs = 0;

   if(_properties.size())
   {
      size_t nProps = _properties.size();
      for(size_t i = nProps - 1; i > -1; i--)
      {
         delete _properties.at(i);
      }
      _properties.clear();
   }
}
//////////////////////////////////////////////
void VEParameterBlock::SetId(unsigned int id)
{
   _id = id;
}
///////////////////////////////////////////////////////////////////
void VEParameterBlock::SetTransform(VE_XML::VETransform* transform)
{
   *_dcs = *transform;
}
/////////////////////////////////////////////////////////////////
void VEParameterBlock::AddProperty(VE_XML::VEDataValuePair* prop)
{
   _properties.push_back(prop);
}
//////////////////////////////////////////////////////////////////
//set the data from an string representing the xml              //
//////////////////////////////////////////////////////////////////
void VEParameterBlock::SetObjectFromXMLData(DOMNode* xmlInput)
{
   //this will be tricky...

}
////////////////////////////////////////
void VEParameterBlock::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString("veParameterBlock"));
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //this will depend on the type of parameter block

   //Add code here to update the specific sub elements
}
//////////////////////////////////////
unsigned int VEParameterBlock::GetId()
{
   return _id;
}
/////////////////////////////////////////////////////
VE_XML::VETransform* VEParameterBlock::GetTransform()
{
   return _dcs;
}
///////////////////////////////////////////////////////////////////////
VE_XML::VEDataValuePair* VEParameterBlock::GetProperty(std::string name)
{
   size_t nProps = _properties.size();
   for ( size_t i = 0; i < nProps; i++)
   {
      if(_properties.at(i)->GetDataName() == name)
      {
         return _properties.at(i);
      }
   }
   return 0;
}
/////////////////////////////////////////////////////////////////////////
VE_XML::VEDataValuePair* VEParameterBlock::GetProperty(unsigned int index)
{
   return _properties.at(index);
}


