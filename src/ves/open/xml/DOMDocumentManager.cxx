/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
//This header must be first because the loki stuff includes headers that 
// conflict with xerces DOMDocument
#include <ves/open/xml/XMLObject.h>
//All other headers can start here
#include <ves/open/xml/DOMDocumentManager.h>

#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XercesVersion.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/XMLFormatter.hpp>

#include <iostream>
#include <fstream>

//using namespace VE_Conductor;
using namespace ves::open::xml;

//////////////////////////////////////////////////////
DOMDocumentManager::DOMDocumentManager( void )
{
    mCommandDocument = 0;
    mParameterDocument = 0;
    mModulesDocument = 0;
    mParseXMLFile = false;
    mWriteXMLFile = false;
    mParser = 0;
    mErrHandler = 0;
    mOutputXMLFile = std::string( "./output.veg" );
    mDocumentType[ "Network" ] = std::pair< std::string, std::string >( "Network", "network" );
    mDocumentType[ "Shader" ] = std::pair< std::string, std::string >( "Shader", "shader" );
    mDocumentType[ "Command" ] = std::pair< std::string, std::string >( "Commands", "commands" );
}
//////////////////////////////////////////////////////
DOMDocumentManager::~DOMDocumentManager( void )
{
    mOutputXMLFile.clear();
}
//////////////////////////////////////////////////////
void DOMDocumentManager::InitializeXerces( void )
{}
////////////////////////////////////////////
void DOMDocumentManager::SetWriteXMLFileOn()
{
    mWriteXMLFile = true;
}
//////////////////////////////////////////////
void DOMDocumentManager::SetWriteXMLStringOn()
{
    mWriteXMLFile = false;
}
////////////////////////////////////////////
void DOMDocumentManager::SetParseXMLFileOn()
{
    mParseXMLFile = true;
}
//////////////////////////////////////////////
void DOMDocumentManager::SetParseXMLStringOn()
{
    mParseXMLFile = false;
}
///////////////////////////////////////////////////////////////////
void DOMDocumentManager::SetOuputXMLFile( const std::string& xmlOutputFile )
{
    mOutputXMLFile = xmlOutputFile;
}
////////////////////////////////////////////////////////////////
void DOMDocumentManager::_readInputString( const std::string& xmlString )
{
    if( xmlString.empty() )
    {
        return;
    }

    std::string system_id( "input.xml" );
    MemBufInputSource inputXML(( const XMLByte* )xmlString.c_str(),
                               static_cast< const unsigned int >( xmlString.size() ), system_id.c_str() );
    mParser->parse( inputXML );
}
////////////////////////////////////////////////////////////
void DOMDocumentManager::_readInputFile( const std::string& xmlFile )
{
    if( !std::ifstream( xmlFile.c_str() ).good() )
    {
        std::cerr << "Could not open file : " << xmlFile.c_str() << std::endl;
        return;
    }

    //mParser->parse(inputXML);
    mParser->parse( xmlFile.c_str() );
}
//////////////////////////////////////////////////////
std::string DOMDocumentManager::WriteDocumentToString( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* document )
{
    std::string outputData;
    // do all the xerces studd to make a DOMWriter
    return outputData;
}
//////////////////////////////////////////////////////
XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* DOMDocumentManager::GetCommandDocument( void )
{
    return mCommandDocument;
}
//////////////////////////////////////////////////////
XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* DOMDocumentManager::GetParameterDocument( void )
{
    return mParameterDocument;
}
//////////////////////////////////////////////////////
XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* DOMDocumentManager::GetModulesDocument( void )
{
    return mModulesDocument;
}

///////////////////////////////////////////////////////////////
void DOMDocumentManager::Load( const std::string& inputCommand )
{
    //std::string system_id( "command.xml" );
    //MemBufInputSource inpsrc( (const XMLByte*)inputCommand.c_str(), inputCommand.size(), system_id.c_str());

    char* message;
    if( !mParser )
    {
        mParser = new XercesDOMParser();

        //Optional settings for the parser
        //XercesDOMParser::Val_Never; XercesDOMParser::Val_Auto; XercesDOMParser::Val_Always;
        mParser->setValidationScheme( XercesDOMParser::Val_Never );
        mParser->setDoNamespaces( false );
        mParser->setDoSchema( false );
        mParser->setValidationSchemaFullChecking( false );
        mParser->setCreateEntityReferenceNodes( false );

        mErrHandler = ( ErrorHandler* ) new HandlerBase();
        mParser->setErrorHandler( mErrHandler );
    }

    try
    {
        if( mParseXMLFile )
        {
            _readInputFile( inputCommand );
        }
        else
        {
            _readInputString( inputCommand );
        }

    }
    catch ( const XMLException& toCatch )
    {
        message = XMLString::transcode( toCatch.getMessage() );
        std::cerr << "Exception message is: \n" << message << "\n";
        XMLString::release( &message );
        delete mParser;
        mParser = 0;
        delete mErrHandler;
        mErrHandler = 0;

        return;
    }
    catch ( const DOMException& toCatch )
    {
        message = XMLString::transcode( toCatch.msg );
        std::cerr << "Exception message is: \n" << message << "\n";
        XMLString::release( &message );
        delete mParser;
        mParser = 0;
        delete mErrHandler;
        mErrHandler = 0;

        return;
    }
    catch ( ... )
    {
        std::cerr << "DOMDocumentManager::Load Unexpected Exception" 
            << std::endl << inputCommand  << std::endl;
        delete mParser;
        mParser = 0;
        delete mErrHandler;
        mErrHandler = 0;
        return;
    }

    mCommandDocument = mParser->getDocument(); //This is the rootNode;
}
//////////////////////////////////////////////////////
void DOMDocumentManager::UnLoadParser( void )
{
    if( mParser )
    {
        delete mParser;
        mParser = 0;
    }

    if( mErrHandler )
    {
        delete mErrHandler;
        mErrHandler = 0;
    }
}
/////////////////////////////////////////////////////
void DOMDocumentManager::CreateCommandDocument( const std::string& type )
{
    DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation(
                              Convert( "LS" ).toXMLString() );

    char* message = 0;
    try
    {
        typedef std::map<
                         std::string,
                         std::pair< std::string, std::string > >
                         ::iterator map_iter_type;

        map_iter_type iter = mDocumentType.find( type );

        std::string tempDoc = iter->second.second;
        mCommandDocument = impl->createDocument( 0,
                                                Convert( tempDoc ).toXMLString(),
                                                0 );

    }
    catch ( const XMLException &toCatch )
    {
        message = XMLString::transcode( toCatch.getMessage() );
        std::cerr << "XMLException Exception message is: \n"
        << message << std::endl;
        XMLString::release( &message );
        return;
    }
    catch ( const DOMException& toCatch )
    {
        message = XMLString::transcode( toCatch.msg );
        std::cout << "DOMException Exception message is: \n"
        << message << std::endl;
        XMLString::release( &message );
        return;
    }
    catch ( ... )
    {
        std::cerr << " ERROR : not a vaild document type : " << type << std::endl;
        return;
    }
#if _XERCES_VERSION >= 30001
    mCommandDocument->setXmlVersion( Convert( "1.0" ).toXMLString() );
    //mCommandDocument->setTextEncoding( Convert( "ISO-8859-1" ).toXMLString() );
#else
    mCommandDocument->setVersion( Convert( "1.0" ).toXMLString() );
    mCommandDocument->setEncoding( Convert( "ISO-8859-1" ).toXMLString() );
#endif
    DOMElement* root_elem = mCommandDocument->getDocumentElement(); //This is the root element

    root_elem->setAttribute( Convert( "name" ).toXMLString(),
               Convert( mDocumentType[ type ].first ).toXMLString() );

    root_elem->setAttribute( Convert( "xmlns:xsi" ).toXMLString(),
         Convert( "http://www.w3.org/2001/XMLSchema-instance" ).toXMLString() );
    //root_elem->setAttribute(
    //           Convert( "xsi:noNamespaceSchemaLocation" ).toXMLString(),
    //           Convert( "verg.xsd" ).toXMLString() );

    root_elem->setAttribute(
               Convert( "xsi:noNamespaceSchemaLocation" ).toXMLString(),
               Convert( "verg_model.xsd" ).toXMLString() );

    //root_elem->setAttribute(
    //           Convert( "xsi:noNamespaceSchemaLocation" ).toXMLString(),
    //           Convert( "vecad.xsd" ).toXMLString() );

    //root_elem->setAttribute(
    //           Convert( "xsi:noNamespaceSchemaLocation" ).toXMLString(),
    //           Convert( "veshader.xsd" ).toXMLString() );
}
/////////////////////////////////////////////////////
const std::string DOMDocumentManager::WriteAndReleaseCommandDocument( void )
{
    DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation(
                              Convert( "LS" ).toXMLString() );
#if _XERCES_VERSION >= 30001
    DOMLSSerializer* theSerializer = static_cast< DOMImplementationLS* >( impl )->createLSSerializer();
    DOMLSOutput* theOutputDesc = static_cast< DOMImplementationLS* >( impl )->createLSOutput();
    theOutputDesc->setEncoding( Convert( "ISO-8859-1" ).toXMLString() );
    //theSerializer->setFeature( XMLUni::fgDOMWRTFormatPrettyPrint, true );
    DOMConfiguration* serializerConfig=theSerializer->getDomConfig();
#else
    DOMWriter* theSerializer = static_cast< DOMImplementationLS* >( impl )->createDOMWriter();
    theSerializer->setFeature( XMLUni::fgDOMWRTFormatPrettyPrint, true );
#endif

    char* message = 0;
    char* tempResultString = 0;
    std::string result;

    try
    {
        if( mWriteXMLFile )
        {
            //take a string passed in as the filename to write out
            LocalFileFormatTarget outputXML( mOutputXMLFile.c_str() );
#if _XERCES_VERSION >= 30001
            theOutputDesc->setByteStream( &outputXML );
            theSerializer->write( mCommandDocument, theOutputDesc  );
#else
            theSerializer->writeNode( &outputXML, *mCommandDocument );
#endif
        }
        else
        {
            // do the serialization through DOMWriter::writeNode();
#if _XERCES_VERSION >= 30001
            serializerConfig->setParameter(XMLUni::fgDOMWRTFormatPrettyPrint, true);
            XMLCh* xXml = theSerializer->writeToString( mCommandDocument );
#else 
            XMLCh* xXml = theSerializer->writeToString( (*mCommandDocument) )
#endif
            tempResultString = XMLString::transcode( xXml );
            result = tempResultString;
            XMLString::release( &tempResultString );
            XMLString::release( &xXml );
        }
    }
    catch ( const XMLException& toCatch )
    {
        message = XMLString::transcode( toCatch.getMessage() );
        std::cout << "Exception message is: \n"
        << message << std::endl;
        XMLString::release( &message );
        //rv=false;
        return NULL;
    }
    catch ( const DOMException& toCatch )
    {
        message = XMLString::transcode( toCatch.msg );
        std::cout << "Exception message is: \n"
        << message << std::endl;
        XMLString::release( &message );
        //rv=false;
        return NULL;
    }
    catch ( ... )
    {
        std::cout << "Unexpected Exception " << std::endl;
        //rv=false;
        return NULL;
    }

    theSerializer->release();
#if _XERCES_VERSION >= 30001
    theOutputDesc->release();
#endif
    //rv=true;
    return result;
}
////////////////////////////////////////////////////////////////////////////////
/*void DOMDocumentManager::ProcessIncludes( void )
{
   //Process the include tags from documents
   //Does not matter whatorder includes are processed in
   // look at cppdom for example of how to process include
   // what have others done with xerces
   // the read and write of include tags will be handled by the
   //individual xmlobjects like veNetwork, veModel
   // for a document this manager may handle this for external includes
   //The xml readerwirter would handle the write for the venetwork
   //to process models seperately
}*/
