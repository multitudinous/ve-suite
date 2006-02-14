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

#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/XMLObjectFactory.h"

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
   _xmlObjects.clear();
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
////////////////////////////////////
void XMLReaderWriter::ReadFromFile()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLFileOn();
   }
}
//////////////////////////////////////
void XMLReaderWriter::ReadFromString()
{
   if(_domDocumentManager)
   {
      _domDocumentManager->SetParseXMLStringOn();
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
void XMLReaderWriter::ReadXMLData(std::string xmlData,
                                  std::string objectNamespace,
                                  std::string tagname)
{
   _domDocumentManager->Load( xmlData );
   //override this in derived classes
   _populateStructureFromDocument(_domDocumentManager->GetCommandDocument(),objectNamespace,tagname);
   _domDocumentManager->UnLoadParser();
}
//////////////////////////////////////////////////////////////////////
std::vector< VE_XML::XMLObject* > XMLReaderWriter::GetLoadedXMLObjects()
{
   return _xmlObjects;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::_populateStructureFromDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument,
                                                      std::string objectNamespace,
                                                      std::string tagName)
{

   //Get the first element and check it's type
   DOMNodeList* xmlObjects = rootDocument->getElementsByTagName( xercesString(tagName) );

   unsigned int nXMLObjects = xmlObjects->getLength();
   if ( nXMLObjects )
   {
      //In case we use the same readerwriter more than once for a read
      _xmlObjects.clear();
      for ( unsigned int i = 0; i < nXMLObjects; ++i )
      {
		  _xmlObjects.push_back( XMLObjectFactory::Instance()->CreateXMLObject(tagName,objectNamespace) );
         _xmlObjects.back()->SetObjectFromXMLData( xmlObjects->item(i) );
      }
   }
}

///////////////////////////////////////////////////////////
void XMLReaderWriter::WriteXMLDocument( std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes,
                                    std::string& xmlData,
                                    std::string documentType )
{
   if ( _domDocumentManager )
   {
      ///If you wanted to return a string you can pass in returnString for xmlData
      if ( xmlData.compare("returnString") )
      {
         _domDocumentManager->SetOuputXMLFile(xmlData);
         _domDocumentManager->SetWriteXMLFileOn();
      }
  
      _domDocumentManager->CreateCommandDocument( documentType );
      DOMDocument* doc = _domDocumentManager->GetCommandDocument();
      
      /// should put in an assert to verify tagnames == nodes
      ///Loop over all the objects that are passed in
      for ( size_t i = 0; i < nodes.size(); ++i )
      {   
         nodes.at( i ).first->SetOwnerDocument( doc );
         doc->getDocumentElement()->appendChild( nodes.at( i ).first->GetXMLData( nodes.at( i ).second ) );  
      }

      if( !xmlData.compare("returnString") )
      {
         xmlData = _domDocumentManager->WriteAndReleaseCommandDocument();
      }
      else
      {
         _domDocumentManager->WriteAndReleaseCommandDocument();
      }
      _domDocumentManager->UnLoadParser();
   }
}
