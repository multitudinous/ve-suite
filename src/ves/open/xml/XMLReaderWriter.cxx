/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/DOMDocumentManager.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLObjectFactory.h>

#include <iostream>
using namespace ves::open::xml;
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
    if( _domDocumentManager )
    {
        _domDocumentManager->UnLoadParser();
        if( _standAloneDDM )
        {
            delete _domDocumentManager;
            _domDocumentManager = 0;
        }
    }
    m_xmlObjects.clear();
}
/////////////////////////////////////////////////
void XMLReaderWriter::UseStandaloneDOMDocumentManager()
{
    if( !_domDocumentManager )
    {
        _domDocumentManager = new DOMDocumentManager();
        _standAloneDDM = true;
    }
}
//////////////////////////////
/*void XMLReaderWriter::WriteToFile()
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
}*/
////////////////////////////////////
void XMLReaderWriter::ReadFromFile()
{
    if( _domDocumentManager )
    {
        _domDocumentManager->SetParseXMLFileOn();
    }
}
//////////////////////////////////////
void XMLReaderWriter::ReadFromString()
{
    if( _domDocumentManager )
    {
        _domDocumentManager->SetParseXMLStringOn();
    }
}
//////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::SetDOMDocumentManager( DOMDocumentManager* ddManager )
{
    _domDocumentManager = ddManager;
    _standAloneDDM = false;
}
////////////////////////////////////////////////////////////////////
DOMDocumentManager* XMLReaderWriter::GetDOMDocumentManager()
{
    return _domDocumentManager;
}
////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData( std::string xmlData,
                                   std::string objectNamespace,
                                   std::string tagname )
{
    _domDocumentManager->Load( xmlData );
    //override this in derived classes
    _populateStructureFromDocument( _domDocumentManager->GetCommandDocument(),
                                    objectNamespace,
                                    tagname );
    _domDocumentManager->UnLoadParser();
    m_xmlObjects = m_internalXmlObjects;
}
////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData( std::string xmlData,
                                   std::vector< std::pair< std::string, std::string > > elementTypes )
{
    _domDocumentManager->Load( xmlData );
    //override this in derived classes
    m_xmlObjects.clear();
    for( size_t i = 0; i < elementTypes.size(); ++i )
    {
        _populateStructureFromDocument( _domDocumentManager->GetCommandDocument(),
                                        elementTypes.at( i ).first,
                                        elementTypes.at( i ).second );
        std::copy( m_internalXmlObjects.begin(),
                   m_internalXmlObjects.end(),
                   std::back_inserter( m_xmlObjects ) );
        m_internalXmlObjects.clear();
    }

    _domDocumentManager->UnLoadParser();
}
////////////////////////////////////////////////////////////////////////////////
std::vector< XMLObject* > XMLReaderWriter::GetLoadedXMLObjects()
{
    return m_xmlObjects;
}
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::_populateStructureFromDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument,
                                                      std::string objectNamespace,
                                                      std::string tagName )
{

    //Get the first element and check it's type
    if( !rootDocument )
    {
        return;
    }

    DOMNodeList* xmlObjects = rootDocument->getElementsByTagName(
                              Convert( tagName ).toXMLString() );

    unsigned int nXMLObjects = xmlObjects->getLength();
    //In case we use the same readerwriter more than once for a read
    m_internalXmlObjects.clear();

    if( nXMLObjects == 0 )
    {
        std::cerr << "XMLReaderWriter::_populateStructureFromDocument "
            << "number of xml objects = " << nXMLObjects << std::endl;
        std::cerr << "XMLReaderWriter::_populateStructureFromDocument "
            << "for " << objectNamespace << " " << tagName << std::endl;
        return;
    }

    for( unsigned int i = 0; i < nXMLObjects; ++i )
    {
        XMLObject* newXMLobj = XMLObjectFactory::Instance()->
                               CreateXMLObject( tagName, objectNamespace );
        if( newXMLobj != NULL )
        {
            m_internalXmlObjects.push_back( newXMLobj );
            m_internalXmlObjects[i]->SetObjectFromXMLData( xmlObjects->item( i ) );
        }
        /*
        else if(newXMLobj == include )
        {
           Call new creator mehtod which returns a vector of XMLObjects
           Append vector of objects to ones already asked for
        }
        */
        else
        {
            std::cerr << "VE-Open XMLReaderWriter Error : No creator "
                << "method for tagname = " << tagName << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::WriteMultipleXMLDocuments(
    std::vector< std::pair< XMLObject*, std::string > > nodes,
    std::string& xmlData )
{
    /*
    if(!_domDocumentManager )
    {
       return;
    }


    //loop over all nodes and figure out what types of documents to write   
       _domDocumentManager->SetOuputXMLFile(xmlData);
       _domDocumentManager->SetWriteXMLFileOn();
       _domDocumentManager->CreateCommandDocument( documentType );
       DOMDocument* doc = _domDocumentManager->GetCommandDocument();
       for(size_t i = 0; i < nodes.size(); ++i )
       {   
          nodes.at( i ).first->SetOwnerDocument( doc );
          doc->getDocumentElement()->appendChild( nodes.at( i ).first->
                                         GetXMLData( nodes.at( i ).second ) );  
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
    */
}
///////////////////////////////////////////////////////////
void XMLReaderWriter::WriteXMLDocument( std::vector< std::pair< XMLObject*, std::string > > nodes,
                                        std::string& xmlData,
                                        std::string documentType )
{
    if( !_domDocumentManager )
    {
        return;
    }

    ///If you wanted to return a string you can pass in returnString for xmlData
    if( xmlData.compare( "returnString" ) )
    {
        _domDocumentManager->SetOuputXMLFile( xmlData );
        _domDocumentManager->SetWriteXMLFileOn();
    }

    _domDocumentManager->CreateCommandDocument( documentType );
    DOMDocument* doc = _domDocumentManager->GetCommandDocument();

    /// should put in an assert to verify tagnames == nodes
    ///Loop over all the objects that are passed in
    for( size_t i = 0; i < nodes.size(); ++i )
    {
        nodes.at( i ).first->SetOwnerDocument( doc );
        doc->getDocumentElement()->appendChild( nodes.at( i ).first->GetXMLData( nodes.at( i ).second ) );
    }

    if( !xmlData.compare( "returnString" ) )
    {
        xmlData = _domDocumentManager->WriteAndReleaseCommandDocument();
    }
    else
    {
        _domDocumentManager->WriteAndReleaseCommandDocument();
    }
    _domDocumentManager->UnLoadParser();
}
