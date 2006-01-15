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
 * File:          $RCSfile: CADXMLReaderWriter.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include "VE_Open/VE_XML/CAD/CADXMLReaderWriter.h"
#include "VE_Open/VE_XML/CAD/CADNode.h"
#include "VE_Open/VE_XML/CAD/CADPart.h"
#include "VE_Open/VE_XML/CAD/CADAssembly.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_CAD;
//////////////////////////////
//Constructor               //
//////////////////////////////
CADXMLReaderWriter::CADXMLReaderWriter()
:VE_XML::XMLReaderWriter()
{
   _rootNode = 0;
}
//////////////////////////////////////////////////
CADXMLReaderWriter::CADXMLReaderWriter(const CADXMLReaderWriter& fr)
:VE_XML::XMLReaderWriter(fr)
{
   _rootNode = new VE_CAD::CADNode(*fr._rootNode);
}
/////////////////////////////
CADXMLReaderWriter::~CADXMLReaderWriter()
{
  
   if(_rootNode)
   {
      delete _rootNode;
      _rootNode = 0;
   }
}
////////////////////////////////////////////
VE_CAD::CADNode* CADXMLReaderWriter::GetRootNode()
{
   return _rootNode;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void CADXMLReaderWriter::_populateStructureFromDocument( DOMDocument* rootDocument)
{

   //Get the first element and check it's type
   DOMNodeList* assemblies = rootDocument->getElementsByTagName( xercesString("CADAssembly") );
   
   unsigned int nAssemblies = assemblies->getLength();
   if(nAssemblies)
   {
      if(_rootNode)
      {
         if(_rootNode->GetNodeType() != std::string("Assembly"))
         {
            delete _rootNode;
            _rootNode = 0;
            _rootNode = new VE_CAD::CADAssembly(rootDocument);
         }else{
            _rootNode->SetOwnerDocument(rootDocument);
         }
      }else{
         _rootNode = new VE_CAD::CADAssembly(rootDocument);
      }
      _rootNode->SetObjectFromXMLData(assemblies->item(0));
   }
   else
   {
      ///check for single part
      DOMNodeList* parts = rootDocument->getElementsByTagName( xercesString("CADPart") );
      unsigned int nParts = parts->getLength();
      if(nParts)
      {
         
         //probably should create an assembly and then add all the parts but
         //assuming there is only one part file for now
         if(_rootNode)
         {
            if(_rootNode->GetNodeType() != std::string("Part"))
            {
               delete _rootNode;
               _rootNode = 0;
               _rootNode = new VE_CAD::CADPart(rootDocument);
            }else{
              _rootNode->SetOwnerDocument(rootDocument);
            }
         }else{
            _rootNode = new VE_CAD::CADPart(rootDocument);
         }
         _rootNode->SetObjectFromXMLData(parts->item(0)); 
      }
   }
}
//////////////////////////////////////////////////////////////
CADXMLReaderWriter& CADXMLReaderWriter::operator=(const CADXMLReaderWriter& rhs)
{
   if(&rhs != this){
      XMLReaderWriter::operator=(rhs);
      if(_rootNode)
      {
         delete _rootNode;
	       _rootNode = 0;
      }
      _rootNode = new VE_CAD::CADNode(*rhs._rootNode);
   }
   return *this;
}
