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
 * File:          $RCSfile: XMLReaderWriter.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/VE_XML/XMLReaderWriter.h"
#include "VE_Open/VE_XML/DOMDocumentManager.h"
#include <iostream>
using namespace VE_XML;
//////////////////////////////
//Constructor               //
//////////////////////////////
XMLReaderWriter::XMLReaderWriter()
{
   _domDocumentManager = 0;
   _standAloneDDM = false;
}
///////////////////////
XMLReaderWriter::~XMLReaderWriter()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->UnLoadParser();
      if(_standAloneDDM)
      {
         delete _domDocumentManager;
         _domDocumentManager = 0;
      }
   }
   
}
/////////////////////////////////////////////////
void XMLReaderWriter::UseStandaloneDOMDocumentManager()
{
   if(!_domDocumentManager)
   {
      _domDocumentManager = new VE_XML::DOMDocumentManager();
      _standAloneDDM = true;
   }
}
//////////////////////////////
void XMLReaderWriter::WriteToFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLFileOn();
   }
}
//////////////////////////////
void XMLReaderWriter::WriteToString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetWriteXMLStringOn();
   }
}
//////////////////////////////
void XMLReaderWriter::ReadFromFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////
void XMLReaderWriter::ReadFromString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::SetDOMDocumentManager(VE_XML::DOMDocumentManager* ddManager)
{
   _domDocumentManager = ddManager;
   _standAloneDDM = false;
}
////////////////////////////////////////////////////////////////////   
VE_XML::DOMDocumentManager* XMLReaderWriter::GetDOMDocumentManager()
{
   return _domDocumentManager;
}
//////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData(std::string xmlData)
{
   _domDocumentManager->Load( xmlData );
   //override this in derived classes
   _populateStructureFromDocument(_domDocumentManager->GetCommandDocument());
}
///////////////////////////////////////////////////////////
void XMLReaderWriter::WriteXMLDocument(std::string xmlData)
{
   if(_domDocumentManager){
      _domDocumentManager->SetOuputXMLFile(xmlData);
      _domDocumentManager->WriteAndReleaseCommandDocument();
      _domDocumentManager->UnLoadParser();
   }
}
