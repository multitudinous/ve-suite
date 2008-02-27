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
    mDomDocumentManager = 0;
    mStandAloneDDM = false;
}
///////////////////////
XMLReaderWriter::~XMLReaderWriter()
{
    if( mDomDocumentManager )
    {
        mDomDocumentManager->UnLoadParser();
        if( mStandAloneDDM )
        {
            delete mDomDocumentManager;
            mDomDocumentManager = 0;
        }
    }
    mXmlObjects.clear();
}
/////////////////////////////////////////////////
void XMLReaderWriter::UseStandaloneDOMDocumentManager()
{
    if( !mDomDocumentManager )
    {
        mDomDocumentManager = new DOMDocumentManager();
        mStandAloneDDM = true;
    }
}
//////////////////////////////
/*void XMLReaderWriter::WriteToFile()
{
   if(mDomDocumentManager)
   {
      mDomDocumentManager->SetWriteXMLFileOn();
   }
}
//////////////////////////////
void XMLReaderWriter::WriteToString()
{
   if(mDomDocumentManager)
   {
      mDomDocumentManager->SetWriteXMLStringOn();
   }
}*/
////////////////////////////////////
void XMLReaderWriter::ReadFromFile()
{
    if( mDomDocumentManager )
    {
        mDomDocumentManager->SetParseXMLFileOn();
    }
}
//////////////////////////////////////
void XMLReaderWriter::ReadFromString()
{
    if( mDomDocumentManager )
    {
        mDomDocumentManager->SetParseXMLStringOn();
    }
}
//////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::SetDOMDocumentManager( DOMDocumentManager* ddManager )
{
    mDomDocumentManager = ddManager;
    mStandAloneDDM = false;
}
////////////////////////////////////////////////////////////////////
DOMDocumentManager* XMLReaderWriter::GetDOMDocumentManager()
{
    return mDomDocumentManager;
}
////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData( const std::string& xmlData,
                                   std::string objectNamespace,
                                   std::string tagname )
{
    mDomDocumentManager->Load( xmlData );
    //override this in derived classes
    _populateStructureFromDocument( mDomDocumentManager->GetCommandDocument(),
                                    objectNamespace,
                                    tagname );
    mDomDocumentManager->UnLoadParser();
    mXmlObjects = mInternalXmlObjects;
}
////////////////////////////////////////////////////////////////////////////////
void XMLReaderWriter::ReadXMLData( const std::string& xmlData,
                                   std::vector< std::pair< std::string, std::string > > elementTypes )
{
    mDomDocumentManager->Load( xmlData );
    //override this in derived classes
    mXmlObjects.clear();
    for( size_t i = 0; i < elementTypes.size(); ++i )
    {
        _populateStructureFromDocument( mDomDocumentManager->GetCommandDocument(),
                                        elementTypes.at( i ).first,
                                        elementTypes.at( i ).second );
        std::copy( mInternalXmlObjects.begin(),
                   mInternalXmlObjects.end(),
                   std::back_inserter( mXmlObjects ) );
        mInternalXmlObjects.clear();
    }

    mDomDocumentManager->UnLoadParser();
}
////////////////////////////////////////////////////////////////////////////////
std::vector< XMLObjectPtr > XMLReaderWriter::GetLoadedXMLObjects()
{
    return mXmlObjects;
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
    mInternalXmlObjects.clear();

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
        XMLObjectPtr newXMLobj = XMLObjectFactory::Instance()->
                               CreateXMLObject( tagName, objectNamespace );
        if( newXMLobj != NULL )
        {
            mInternalXmlObjects.push_back( newXMLobj );
            mInternalXmlObjects[i]->SetObjectFromXMLData( xmlObjects->item( i ) );
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
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes,
    std::string& xmlData )
{
    /*
    if(!mDomDocumentManager )
    {
       return;
    }


    //loop over all nodes and figure out what types of documents to write   
       mDomDocumentManager->SetOuputXMLFile(xmlData);
       mDomDocumentManager->SetWriteXMLFileOn();
       mDomDocumentManager->CreateCommandDocument( documentType );
       DOMDocument* doc = mDomDocumentManager->GetCommandDocument();
       for(size_t i = 0; i < nodes.size(); ++i )
       {   
          nodes.at( i ).first->SetOwnerDocument( doc );
          doc->getDocumentElement()->appendChild( nodes.at( i ).first->
                                         GetXMLData( nodes.at( i ).second ) );  
       }
       
       if( !xmlData.compare("returnString") )
       {
          xmlData = mDomDocumentManager->WriteAndReleaseCommandDocument();
       }
       else
       {
          mDomDocumentManager->WriteAndReleaseCommandDocument();
       }
       mDomDocumentManager->UnLoadParser();
    */
}
///////////////////////////////////////////////////////////
void XMLReaderWriter::WriteXMLDocument( std::vector< std::pair< XMLObjectPtr, std::string > > nodes,
                                        std::string& xmlData,
                                        std::string documentType )
{
    if( !mDomDocumentManager )
    {
        return;
    }

    ///If you wanted to return a string you can pass in returnString for xmlData
    if( xmlData.compare( "returnString" ) )
    {
        mDomDocumentManager->SetOuputXMLFile( xmlData );
        mDomDocumentManager->SetWriteXMLFileOn();
    }

    mDomDocumentManager->CreateCommandDocument( documentType );
    DOMDocument* doc = mDomDocumentManager->GetCommandDocument();

    /// should put in an assert to verify tagnames == nodes
    ///Loop over all the objects that are passed in
    for( size_t i = 0; i < nodes.size(); ++i )
    {
        nodes.at( i ).first->SetOwnerDocument( doc );
        doc->getDocumentElement()->appendChild( nodes.at( i ).first->GetXMLData( nodes.at( i ).second ) );
    }

    if( !xmlData.compare( "returnString" ) )
    {
        xmlData = mDomDocumentManager->WriteAndReleaseCommandDocument();
    }
    else
    {
        mDomDocumentManager->WriteAndReleaseCommandDocument();
    }
    mDomDocumentManager->UnLoadParser();
}
