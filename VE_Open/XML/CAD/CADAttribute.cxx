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
 * File:          $RCSfile: CADAttribute.h,v $
 * Date modified: $Date: 2006-01-21 10:25:02 -0600 (Sat, 21 Jan 2006) $
 * Version:       $Rev: 3544 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADMaterial.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/CAD/CADCreator.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/Shader/Program.h"
using namespace VE_Shader;
using namespace VE_CAD;
using namespace VE_XML;
/////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                      //
/////////////////////////////////////////////////////////////////////////////////////
CADAttribute::CADAttribute( )
:XMLObject()
{
   _attributeType = std::string("Material");
   _material = 0; 
   _blending = true;
   _glslProgram = 0;
   SetObjectType("CADAttribute");
   SetObjectNamespace("CAD");
   //This may need to be somewhere else
   if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("CAD"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("CAD",new CADCreator());
   }

   if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("Shader"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("Shader",new ShaderCreator());
   }
}
/////////////////////////////
//Destructor               //
/////////////////////////////
CADAttribute::~CADAttribute()
{
   if(_material)
   {
      delete _material;
      _material = 0;
   }
   if(_glslProgram)
   {
      delete _glslProgram;
      _glslProgram = 0;
   }
   _attributeType.clear();
}
///////////////////////////////////
void CADAttribute::EnableBlending()
{
   _blending = true;
}
////////////////////////////////////
void CADAttribute::DisableBlending()
{
   _blending = false;
}
//////////////////////////////////////////////////////////////
void CADAttribute::SetAttributeType(std::string attributeType)
{
   _attributeType = attributeType;
}
/////////////////////////////////////////////////////////////
void CADAttribute::SetMaterial(VE_CAD::CADMaterial material)
{
   if(_material)
   {
      delete _material;
      _material = 0;
   }
   _material = new CADMaterial(material);
   _attributeType = std::string("Material");
}
//////////////////////////////////////////////////////////////
void CADAttribute::SetProgram(VE_Shader::Program glslProgram)
{
   if(_glslProgram)
   {
      delete _glslProgram;
      _glslProgram = 0;
   }
   _glslProgram = new Program(glslProgram);
   _attributeType = std::string("Program");
}
//////////////////////////////////////////////////////////
void CADAttribute::SetObjectFromXMLData( DOMNode* xmlNode)
{
   DOMElement* currentElement = 0;
   const XMLCh* name;
   if(xmlNode->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      name = xmlNode->getNodeName();
      currentElement = dynamic_cast<DOMElement*>(xmlNode);
   }
   
   if(currentElement)
   {
      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            DOMElement* typeNode = GetSubElement(currentElement,std::string("type"),0);
            if(typeNode)
            {
              _attributeType = ExtractDataStringFromSimpleElement( typeNode );
            }

            DOMElement* blendNode = GetSubElement(currentElement,std::string("blending"),0);
            if(blendNode)
            {
              _blending = ExtractBooleanFromSimpleElement(blendNode);
            }
	    
            if(_attributeType == std::string("Material"))
            {
               DOMElement* materialNode = GetSubElement(currentElement,std::string("material"),0);
               if(materialNode)
               {
                  if(!_material)
	               {
                     _material = new CADMaterial();
                  }
	               _material->SetObjectFromXMLData(materialNode);
               }
            }
	         else if(_attributeType == std::string("Program"))
            {
               DOMElement* programNode = GetSubElement(currentElement,std::string("program"),0);
	            if(programNode)
               {
                  if(!_glslProgram)
                  {
                     _glslProgram = new Program();
                  }
	                _glslProgram->SetObjectFromXMLData(programNode);
               }
            }
         } 
      }
   }
}
////////////////////////////////////////////
std::string CADAttribute::GetAttributeType()
{
   return _attributeType;
}
////////////////////////////////////////////////
VE_CAD::CADMaterial* CADAttribute::GetMaterial()
{
   return _material;
}
//////////////////////////////////////////////////
VE_Shader::Program* CADAttribute::GetGLSLProgram()
{
   return _glslProgram;
}
///////////////////////////////////////////////////
CADAttribute::CADAttribute(const CADAttribute& rhs)
:XMLObject(rhs)
{
   _attributeType = rhs._attributeType;
   _material = 0;
   _glslProgram = 0;
   _blending = rhs._blending;

   if(_attributeType == std::string("Material"))
   {
      _material = new CADMaterial(*rhs._material); 
   }
   else if(_attributeType == std::string("Program"))
   {
      _glslProgram = new Program(*rhs._glslProgram);
   }
}
//////////////////////////////////////////////////////////////
CADAttribute& CADAttribute::operator=(const CADAttribute& rhs)
{
   if(this != &rhs)
   {
      XMLObject::operator =(rhs);
      _attributeType = rhs._attributeType;
      _material = 0;
      _glslProgram = 0;
      _blending = rhs._blending;
      if(_attributeType == std::string("Material"))
      {
         if(_material)
         {
            delete _material;
            _material = 0;
         }
         _material = new CADMaterial(*rhs._material); 
      }
      else if(_attributeType == std::string("Program"))
      {
         if(_glslProgram)
         {
            delete _glslProgram;
            _glslProgram = 0;
         }
         _glslProgram = new Program(*rhs._glslProgram);
         
      }
   }
   return *this;
}
//////////////////////////////////////////////////////
void CADAttribute::_updateVEElement(std::string input)
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString(input));
   }
   SetSubElement("type",_attributeType);
   SetSubElement("blending",_blending);

   if(_attributeType == std::string("Material"))
   {
      if(_material)
      {
         _material->SetOwnerDocument(_rootDocument);
         SetSubElement("material",_material);
      }
   }
   else if(_attributeType == std::string("Program"))
   {
      if(_glslProgram)
      {
         _glslProgram->SetOwnerDocument(_rootDocument);
         SetSubElement("program",_glslProgram);
      }
   }
}
////////////////////////////////////////////
std::string CADAttribute::GetAttributeName()
{
   if(_attributeType == std::string("Material"))
   {
      if(_material)
      {
         return _material->GetMaterialName();
      }
   }
   else if(_attributeType == std::string("Program"))
   {
      if(_glslProgram)
      {
         return _glslProgram->GetProgramName();
      }
   }
   return std::string("");
}
