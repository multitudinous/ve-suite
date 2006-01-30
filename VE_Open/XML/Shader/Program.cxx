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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/Shader/Shader.h"
using namespace VE_Shader;
//////////////////////////////////////////////////////////////////////////
//Constructor                                                           //
//////////////////////////////////////////////////////////////////////////
Program::Program()
:XMLObject()
{
   _name = std::string("VEProgram");
   _vertexShader = 0;
   _fragmentShader = 0;
}
///////////////////
//Destructor     //
///////////////////
Program::~Program()
{
   _name.clear();
   if(_vertexShader)
   {
      delete _vertexShader;
      _vertexShader = 0;
   }
   if(_fragmentShader)
   {
      delete _fragmentShader;
      _fragmentShader = 0;
   }
}
/////////////////////////////////////  
//Copy constructor                 //
/////////////////////////////////////
Program::Program(const Program& rhs)
:XMLObject(rhs)
{
   _name = std::string("VEProgram");
   _vertexShader = 0;
   _fragmentShader = 0;
   if(rhs._vertexShader)
   {
      _vertexShader = new Shader(*rhs._vertexShader);
   }
   if(rhs._fragmentShader)
   {
      _fragmentShader = new Shader(*rhs._fragmentShader);
   }
   _name = rhs._name;
}
/////////////////////////////////////////////////
void Program::SetVertexShader(Shader* vertShader)
{
   _vertexShader = vertShader;
}
///////////////////////////////////////////////////
void Program::SetFragmentShader(Shader* fragShader)
{
   _fragmentShader = fragShader;
}
//////////////////////////////////////////////
void Program::SetProgramName(std::string name)
{
   _name = name;
}
/////////////////////////////////////////////////////
void Program::SetObjectFromXMLData(DOMNode* xmlInput)
{
    DOMElement* currentElement = 0;

   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }
   
   if(currentElement)
   {
      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            //Get the source
            DOMElement* vertexShader = GetSubElement(currentElement,std::string("vertexShader"),0);
            if(vertexShader)
            {
               if(!_vertexShader)
               {
                  _vertexShader = new Shader();
               }
               _vertexShader->SetObjectFromXMLData(vertexShader);
            }
            DOMElement* fragShader = GetSubElement(currentElement,std::string("fragmentShader"),0);
            if(fragShader)
            {
               if(!_fragmentShader)
               {
                  _fragmentShader = new Shader();
               }
               _fragmentShader->SetObjectFromXMLData(fragShader);
            }
            DOMElement* nameNode = GetSubElement(currentElement,std::string("name"),0);
            if(nameNode)
            {
                _name = ExtractDataStringFromSimpleElement( nameNode );
            }
         }
      }
   }
}
////////////////////////////////////
Shader* Program::GetFragmentShader()
{
   return _fragmentShader;
}
//////////////////////////////////
Shader* Program::GetVertexShader()
{
   return _vertexShader;
}
/////////////////////////////////////
std::string Program::GetProgramName()
{
   return _name;
}
/////////////////////////////////////////////////
void Program::_updateVEElement(std::string input)
{
   if( !_veElement )
   {
      // name comes from verg.xsd
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   _updateProgramName();
   if(_vertexShader)
   {
      _vertexShader->SetOwnerDocument(_rootDocument);
      _veElement->appendChild(_vertexShader->GetXMLData("vertexShader"));
   }
   if(_fragmentShader)
   {
      _fragmentShader->SetOwnerDocument(_rootDocument);
      _veElement->appendChild(_fragmentShader->GetXMLData("fragmentShader"));
   }
}
/////////////////////////////////
void Program::_updateProgramName()
{
   DOMElement* nameElement = _rootDocument->createElement(xercesString("name"));
   DOMText* name = _rootDocument->createTextNode(xercesString(_name));
   nameElement->appendChild(name);
   _veElement->appendChild(nameElement);
}
///////////////////////////////////////////////
Program& Program::operator=(const Program& rhs)
{

   if(this != &rhs){
      XMLObject::operator=(rhs);
      if(_vertexShader)
      {
         delete _vertexShader;
         _vertexShader = 0;
      }
      if(_fragmentShader)
      {
         delete _fragmentShader;
         _fragmentShader = 0;
      }
      if(rhs._vertexShader)
      {
         _vertexShader = new Shader(*rhs._vertexShader);
      }
      if(rhs._fragmentShader)
      {
         _fragmentShader = new Shader(*rhs._fragmentShader);
      }
      _name = rhs._name;
   }
   return *this;
}
