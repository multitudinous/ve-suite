/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: VEParameterBlock.cxx,v $
 * Date modified: $Date: 2005-08-24 19:07:18 -0500 (Wed, 24 Aug 2005) $
 * Version:       $Rev: 2970 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/VEParameterBlock.h"
#include "VE_Open/VE_XML/VETransform.h"
#include "VE_Open/VE_XML/VEDataValuePair.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>

using namespace VE_XML;

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


