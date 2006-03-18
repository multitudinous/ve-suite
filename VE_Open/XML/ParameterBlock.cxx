/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: ParameterBlock.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
XERCES_CPP_NAMESPACE_USE
#include <iostream>

using namespace VE_XML;

////////////////////////////////////////////////////
ParameterBlock::ParameterBlock(unsigned int id)
:XMLObject()
{
   _id = id;
   _dcs = new Transform(  );
   
   SetObjectType("ParameterBlock");
}
/////////////////////////////////////
ParameterBlock::~ParameterBlock()
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
void ParameterBlock::SetId(unsigned int id)
{
   _id = id;
}
///////////////////////////////////////////////////////////////////
void ParameterBlock::SetTransform(VE_XML::Transform* transform)
{
   *_dcs = *transform;
}
/////////////////////////////////////////////////////////////////
void ParameterBlock::AddProperty(VE_XML::DataValuePair* prop)
{
   _properties.push_back(prop);
}
//////////////////////////////////////////////////////////////////
//set the data from an string representing the xml              //
//////////////////////////////////////////////////////////////////
void ParameterBlock::SetObjectFromXMLData(DOMNode* xmlInput)
{
   //this will be tricky...

}
////////////////////////////////////////
void ParameterBlock::_updateVEElement( std::string input )
{
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //this will depend on the type of parameter block

   //Add code here to update the specific sub elements
}
//////////////////////////////////////
unsigned int ParameterBlock::GetId()
{
   return _id;
}
/////////////////////////////////////////////////////
VE_XML::Transform* ParameterBlock::GetTransform()
{
   return _dcs;
}
///////////////////////////////////////////////////////////////////////
VE_XML::DataValuePair* ParameterBlock::GetProperty(std::string name)
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
VE_XML::DataValuePair* ParameterBlock::GetProperty(unsigned int index)
{
   return _properties.at(index);
}


