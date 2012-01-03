/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#include "DynSim.h"

#include <xercesc/util/XMLString.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/framework/XMLFormatter.hpp>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Port.h>

#include <fstream>
#include <cctype>
#define DSS_VERSION "C:/SIMSCI/DSS44/GUI/"

XERCES_CPP_NAMESPACE_USE

///////////////////////////////////////////////////////////////////////////////
DynSim::DynSim( std::string unitName )
{
    try
    {
       XMLPlatformUtils::Initialize();
    }
    catch ( const XMLException &toCatch )
    {
    //    XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
    //    << "  Exception message:"
    //    << XMLString::transcode( toCatch.getMessage() ) << XERCES_STD_QUALIFIER endl;
    //    return 1;
    }
    m_unitName = unitName;
}
///////////////////////////////////////////////////////////////////////////////
std::string DynSim::CreateNetwork( std::string filename )
{
    ParseTreeFile( filename + ".tree" );
    m_fileName = filename + ".xml";
    InitializeParser();
    ParseFlowsheets();
    //PopulateStreams();
    return CreateVESNetwork();
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::ParseTreeFile( std::string treeFilename )
{
    std::ifstream inFile( treeFilename.c_str(), std::ifstream::in );
    if(!inFile.is_open())
    {
        std::cerr << "Could not open file : " << treeFilename.c_str() << std::endl;
        return;
}
    
    std::string temp;
    std::string name;
    std::vector< std::string > entries;
    getline(inFile, temp);
    int opcEntryCount = 0;
    while( !inFile.eof() )
    {
        opcEntryCount++;
        
        //get the variable name
        std::stringstream opcTokenizer( temp );
        opcTokenizer >> name;

        //if( name.find(".") != std::string::npos )
        //{
            name = name.substr( 0, name.find(".") );
        //}
        entries.push_back( name );
        
        getline(inFile, temp);
    }

    //parse out opc sheet name and variables
    m_opcFlowsheetName = entries[0].substr( 0, entries[0].find_first_of("_") );

    int underScorePos;
    for( int i = 0; i < entries.size(); i++)
    {
        underScorePos = entries[i].find_first_of("_");
        m_opcBlocks.push_back( entries[i].substr( underScorePos + 1, 
            entries[0].size() - underScorePos ) );
        //m_opcBlocks.push_back( entries[i] );
    }
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::InitializeParser( )
{
    //initialize parser
    mParser = new XercesDOMParser();
    mParser->setValidationScheme( XercesDOMParser::Val_Always );  // optional.
    mParser->setDoNamespaces( true );  // optional
    mErrHandler = ( ErrorHandler* ) new HandlerBase();
    mParser->setErrorHandler( mErrHandler );

    if( !std::ifstream( m_fileName.c_str() ).good() )
    {
        std::cerr << "Could not open file : " << m_fileName.c_str() << std::endl;
        return;
    }

    char* message = 0;
    try
    {

        mParser->parse( m_fileName.c_str() );

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
            << std::endl;
        delete mParser;
        mParser = 0;
        delete mErrHandler;
        mErrHandler = 0;
        return;
    }
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::ParseFlowsheets( )
{
    //get flowsheet entry
    mCommandDocument = mParser->getDocument();
    root_elem = mCommandDocument->getDocumentElement();
    DOMNodeList* syslist = 
        root_elem->getElementsByTagName (
        XMLString::transcode("SYS")
        );
    
    int sysCount = syslist->getLength();
    for( int i = 0; i < sysCount; i++)
    {
        //get name
        DOMNode* node = syslist->item( i );
        DOMNodeList* childList = node->getChildNodes();
        DOMElement* nameElement =
            dynamic_cast< DOMElement* > ( childList->item( 1 ) );
        DOMText* rawText =
                dynamic_cast< DOMText* >
                ( nameElement->getFirstChild() );
        char* fUnicodeForm =
            XMLString::transcode( rawText->getData() );
        std::string name( fUnicodeForm );
        
        //parse objects
        DOMElement* sys_element = dynamic_cast< DOMElement* > ( node );
        ParseObjects( sys_element );
        
        //parse gmbe's
        PopulateBlocks( sys_element );

        flowsheets[name].name = name;
        flowsheets[name].id = i;
        flowsheets[name].blocks = blocks;
        flowsheets[name].streams = streams;
        flowsheets[name].others = others;
        flowsheets[name].cls = "Flowsheet";

        blocks.clear();
        streams.clear();
        others.clear();
    }
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::ParseObjects( DOMElement* sys_element )
{        
    //get obj entries
    DOMNodeList* mylist = 
        sys_element->getElementsByTagName (
        XMLString::transcode("OBJ")
        );

    //parse obj entries into streams and blocks
    int nodeCount = mylist->getLength();
    for( int i = 0; i < nodeCount; i++)
    {
        DOMNode* node = mylist->item( i );
        DOMNodeList* childList = node->getChildNodes();
        DOMElement* nameElement =
            dynamic_cast<DOMElement*> ( childList->item( 1 ) );
        DOMText* rawText =
                dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                ( nameElement->getFirstChild() );
        char* fUnicodeForm =
            XMLString::transcode( rawText->getData() );
        std::string name( fUnicodeForm );

        DOMElement* clsElement =
            dynamic_cast<DOMElement*> ( childList->item( 5 ) );
        rawText =
                dynamic_cast< DOMText* >
                ( clsElement->getFirstChild() );
        fUnicodeForm =
            XMLString::transcode( rawText->getData() );
        std::string cls( fUnicodeForm );

        if( cls.compare( "STREAM" ) != 0)
        {
            blocks[name].name = name;
            std::transform(cls.begin(), cls.end(), cls.begin(), std::tolower);
            blocks[name].cls = cls;
        }
        else
        {
            streams[name].name = name;
            streams[name].cls = cls;
        }
    }
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::PopulateBlocks( DOMElement* sys_element )
{
    //
    DOMNodeList *gmbeList = 
        sys_element->getElementsByTagName (
        XMLString::transcode("GMBE")
        );

    int gmbeCount = gmbeList->getLength();
    int portCount = 0;
    int blockCount = 0;

    for( int i = 0; i < gmbeCount; i++ )
    {
        DOMNode* node = gmbeList->item( i );
        DOMNodeList* childList = node->getChildNodes();
        DOMElement* element =
            dynamic_cast<DOMElement*> ( childList->item( 3 ) );
        XMLCh* nameEntry = XMLString::transcode("GMBE_NAME");

        //test for GMBE_NAME Entry - if not don't add to list
        if( XMLString::compareString( element->getTagName(), nameEntry ) == 0 )
        {
        DOMText* rawText =
                dynamic_cast< DOMText* >
                ( element->getFirstChild() );
        char* fUnicodeForm =
            XMLString::transcode( rawText->getData() );
        std::string name( fUnicodeForm );

        if( blocks.find( name ) != blocks.end() )  
        {
            //id
            element = dynamic_cast<DOMElement*> ( childList->item( 1 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int id = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            blocks[name].id = id;

            //loc x
            element = dynamic_cast<DOMElement*> ( childList->item( 9 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int x = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            blocks[name].x = x;
            
            //loc y
            element = dynamic_cast<DOMElement*> ( childList->item( 11 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int y = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            blocks[name].y = y;
            
            //width
            element = dynamic_cast<DOMElement*> ( childList->item( 13 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int width = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            blocks[name].width = width;
            
            //height
            element = dynamic_cast<DOMElement*> ( childList->item( 15 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int height = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            blocks[name].height = height;   
    
            //Ports
            DOMElement* tempElement = dynamic_cast<DOMElement*> ( node );
            DOMNodeList *gmbePortList = 
                tempElement->getElementsByTagName( 
                XMLString::transcode("GMBE_PORT") );
            int portNum = gmbePortList->getLength();
            for(int j = 0; j < portNum; j++)
            {
                DOMNode* tempNode = gmbePortList->item( j );
                DOMNodeList* childList = tempNode->getChildNodes();
                port tempPort;
                tempPort.id = portCount++;

                //model id / name
                tempPort.modelId = blocks[name].id;
                tempPort.modelName = name;

                //type / dataFlow
                element = dynamic_cast<DOMElement*> ( childList->item( 3 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                char* fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string dataFlow( fUnicodeForm );
                if( !dataFlow.compare( "FeedPort" ) )
                {
                    tempPort.dataFlow = "input";
                }
                else if( !dataFlow.compare( "ProductPort" ) )
                {
                    tempPort.dataFlow = "output";
                }
                else
                {
                    tempPort.dataFlow = "InvalidName";
                }

                //stream name
                element = dynamic_cast<DOMElement*> ( childList->item( 7 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string streamName( fUnicodeForm );
                tempPort.streamName = streamName;

                //loc x
                element = dynamic_cast<DOMElement*> ( childList->item( 13 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                int portx = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() );
                tempPort.x = portx - x;
                
                //loc y
                element = dynamic_cast<DOMElement*> ( childList->item( 15 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                int porty = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() );
                tempPort.y = porty - y;

                blocks[name].ports.push_back( tempPort );
                streams[streamName].ports.push_back( tempPort );
            }
            //Params
            //find imageType
            DOMNodeList *paramList = 
                tempElement->getElementsByTagName( 
                XMLString::transcode("PARAMS") );
            int paramNum = paramList->getLength();
            for(int j = 0; j < paramNum; j++)
            {
                //look for imageType
                DOMNode* tempNode = paramList->item( j );
                DOMNodeList* childList = tempNode->getChildNodes();

                //read "name" 
                element = dynamic_cast<DOMElement*> ( childList->item( 1 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string paramName( fUnicodeForm );

                // compare name with "imageType"
                if( !paramName.compare( "imageType" ) )
                {
                    //imageType
                    //Parse CDATA section
                    element = dynamic_cast<DOMElement*> ( childList->item( 3 ) );
                    DOMCDATASection * cdata = dynamic_cast< DOMCDATASection* >
                            ( element->getFirstChild() );
                    fUnicodeForm =
                        XMLString::transcode( cdata->getData() );
                    std::string imageType( fUnicodeForm );
                    
                    blocks[name].imageType = imageType;

                    //imageType located exit loop
                    break;
                }
            }
        }
        else if( streams.find( name ) != streams.end() )
        {
            //loc x
            element = dynamic_cast<DOMElement*> ( childList->item( 9 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int x = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            //streams[name].x = x;
            
            //loc y
            element = dynamic_cast<DOMElement*> ( childList->item( 11 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int y = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            //streams[name].y = y;
            
            //width
            element = dynamic_cast<DOMElement*> ( childList->item( 13 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int width = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            streams[name].width = width;
            
            //height
            element = dynamic_cast<DOMElement*> ( childList->item( 15 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int height = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            streams[name].height = height;

            //loop over vertex
            XERCES_CPP_NAMESPACE_QUALIFIER DOMNodeList* vtxList = 
                 dynamic_cast<DOMElement*>( gmbeList->item( i ) )->getElementsByTagName (
                XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode("GMBE_VTX"));
            for( int i = 0; i < vtxList->getLength(); i++)
            {
                XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* vtxNode = vtxList->item( i );
                XERCES_CPP_NAMESPACE_QUALIFIER DOMNodeList* vtxChildList = 
                    vtxNode->getChildNodes();

                XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* idxElement = 
                    dynamic_cast<DOMElement*> ( vtxChildList->item( 1 ) );
                rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( idxElement->getFirstChild() );
                int idx = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() );

                XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* xElement = 
                    dynamic_cast<DOMElement*> ( vtxChildList->item( 3 ) );
                rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( xElement->getFirstChild() );
                int vtx_x = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() ) + x;

                XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* yElement = 
                    dynamic_cast<DOMElement*> ( vtxChildList->item( 5 ) );
                rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( yElement->getFirstChild() );
                int vtx_y = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() ) + y;

                std::pair< int, int > tempVtx( vtx_x, vtx_y );
                streams[name].vtx.push_back( tempVtx );
            }
        }
        else  
        {
            //id
            element = dynamic_cast<DOMElement*> ( childList->item( 1 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int id = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            others[name].id = id;

            element =
                dynamic_cast<DOMElement*> ( childList->item( 5 ) );
            rawText =
                    dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            fUnicodeForm =
                XMLString::transcode( rawText->getData() );
            std::string cls( fUnicodeForm );

            others[name].name = name;
            std::transform(cls.begin(), cls.end(), cls.begin(), std::tolower);
            others[name].cls = cls;

            //loc x
            element = dynamic_cast<DOMElement*> ( childList->item( 9 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int x = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            others[name].x = x;
            
            //loc y
            element = dynamic_cast<DOMElement*> ( childList->item( 11 ) );
            rawText = dynamic_cast< DOMText* >
                    ( element->getFirstChild() );
            int y = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            others[name].y = y;
            
            //width
            element = dynamic_cast<DOMElement*> ( childList->item( 13 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int width = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            others[name].width = width;
            
            //height
            element = dynamic_cast<DOMElement*> ( childList->item( 15 ) );
            rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >
                    ( element->getFirstChild() );
            int height = XERCES_CPP_NAMESPACE_QUALIFIER
                XMLString::parseInt( rawText->getData() );
            others[name].height = height;   
    
            DOMElement* tempElement = dynamic_cast<DOMElement*> ( node );
            /*//Ports
            DOMNodeList *gmbePortList = 
                tempElement->getElementsByTagName( 
                XMLString::transcode("GMBE_PORT") );
            int portNum = gmbePortList->getLength();
            for(int j = 0; j < portNum; j++)
            {
                DOMNode* tempNode = gmbePortList->item( j );
                DOMNodeList* childList = tempNode->getChildNodes();
                port tempPort;
                tempPort.id = portCount++;

                //model id / name
                tempPort.modelId = blocks[name].id;
                tempPort.modelName = name;

                //type / dataFlow
                element = dynamic_cast<DOMElement*> ( childList->item( 3 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                char* fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string dataFlow( fUnicodeForm );
                if( !dataFlow.compare( "FeedPort" ) )
                {
                    tempPort.dataFlow = "input";
                }
                else if( !dataFlow.compare( "ProductPort" ) )
                {
                    tempPort.dataFlow = "output";
                }
                else
                {
                    tempPort.dataFlow = "InvalidName";
                }

                //stream name
                element = dynamic_cast<DOMElement*> ( childList->item( 7 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string streamName( fUnicodeForm );
                tempPort.streamName = streamName;

                //loc x
                element = dynamic_cast<DOMElement*> ( childList->item( 13 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                int portx = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() );
                tempPort.x = portx - x;
                
                //loc y
                element = dynamic_cast<DOMElement*> ( childList->item( 15 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                int porty = XERCES_CPP_NAMESPACE_QUALIFIER
                    XMLString::parseInt( rawText->getData() );
                tempPort.y = porty - y;

                blocks[name].ports.push_back( tempPort );
                streams[streamName].ports.push_back( tempPort );
            }*/
            //Params
            //find imageType
            DOMNodeList *paramList = 
                tempElement->getElementsByTagName( 
                XMLString::transcode("PARAMS") );
            int paramNum = paramList->getLength();
            for(int j = 0; j < paramNum; j++)
            {
                //look for imageType
                DOMNode* tempNode = paramList->item( j );
                DOMNodeList* childList = tempNode->getChildNodes();

                //read "name" 
                element = dynamic_cast<DOMElement*> ( childList->item( 1 ) );
                rawText = dynamic_cast< DOMText* >
                        ( element->getFirstChild() );
                fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string paramName( fUnicodeForm );

                // compare name with "imageType"
                if( !paramName.compare( "imageType" ) )
                {
                    //imageType
                    //Parse CDATA section
                    element = dynamic_cast<DOMElement*> ( childList->item( 3 ) );
                    DOMCDATASection * cdata = dynamic_cast< DOMCDATASection* >
                            ( element->getFirstChild() );
                    fUnicodeForm =
                        XMLString::transcode( cdata->getData() );
                    std::string imageType( fUnicodeForm );
                    
                    others[name].imageType = imageType;

                    //imageType located exit loop
                    break;
                }
            }
        }
        }
    }
}

//void DynSim::PopulateStreams()
//{
//}
///////////////////////////////////////////////////////////////////////////////
std::string DynSim::CreateVESNetwork()
{
    //loop over blocks

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    ves::open::xml::model::NetworkPtr
        mainNetwork( new ves::open::xml::model::Network() );
    ves::open::xml::model::SystemPtr
        veSystem( new ves::open::xml::model::System() );

    // create default state info section
    ves::open::xml::DataValuePairPtr dvpPtr;
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_xUserScale", 1.0 );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_yUserScale", 1.0 );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixX", static_cast< long int >( 20 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixY", static_cast< long int >( 20 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitX", static_cast< long int >( 200 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitY", static_cast< long int >( 200 ) );
    mainNetwork->AddDataValuePair( dvpPtr );
    veSystem->AddNetwork( mainNetwork );

    std::map< std::string, flowsheet >::iterator sheetIter;
    for( sheetIter = flowsheets.begin();
        sheetIter != flowsheets.end();
        ++sheetIter )
    {
        //create a model for the flowsheet
        ves::open::xml::model::ModelPtr
        flowSheetModel( new ves::open::xml::model::Model() );
        flowSheetModel->SetModelID( sheetIter->second.id );
        flowSheetModel->SetPluginName( sheetIter->second.name );
        flowSheetModel->SetPluginType( "SimUOPlugin" );
        flowSheetModel->SetVendorName( m_unitName );
        std::string DSS_V (DSS_VERSION);
        flowSheetModel->SetIconFilename( DSS_V + "Images/ClassIcons/" + sheetIter->second.cls + ".gif" );
        //flowSheetModel->SetIconFilename( sheetIter->second.cls );
        flowSheetModel->SetIconRotation( 0 );
        flowSheetModel->SetIconScale( 1 );
        flowSheetModel->SetIconMirror( 0 );
        flowSheetModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >(
            0, ( 60 * sheetIter->second.id ) ) );
        flowSheetModel->SetIconHiddenFlag( 0 );

        //create a sub system to attach to the flowsheet plugin
        ves::open::xml::model::SystemPtr
            subSystem( new ves::open::xml::model::System() );
        ves::open::xml::model::NetworkPtr
            subNetwork( new ves::open::xml::model::Network() );
        subSystem->AddNetwork(subNetwork);

        //create links
        std::map< std::string, stream >::iterator streamIter;
        for ( streamIter = sheetIter->second.streams.begin();
            streamIter != sheetIter->second.streams.end();
            ++streamIter )
        {
            if( !streamIter->second.cls.empty() &&
                streamIter->second.ports.size() > 0 )
            {
                ves::open::xml::model::LinkPtr
                    xmlLink( new ves::open::xml::model::Link() );
                if( !streamIter->second.ports[0].dataFlow.compare("input") )
                {
                    if( streamIter->second.ports.size() > 1 )
                    {
                        xmlLink->GetToModule()->SetData(
                            streamIter->second.ports[0].modelName,
                            static_cast< long int >( streamIter->second.ports[0].modelId ) );
                        xmlLink->GetFromModule()->SetData(
                            streamIter->second.ports[1].modelName,
                            static_cast< long int >( streamIter->second.ports[1].modelId ) );
                        *(xmlLink->GetFromPort()) = static_cast< long int >( streamIter->second.ports[1].id );
                        *(xmlLink->GetToPort()) = static_cast< long int >( streamIter->second.ports[0].id );
                    }
                    //if it only has one port
                    /*else
                    {
                        xmlLink->GetToModule()->SetData(
                            streamIter->second.ports[0].modelName,
                            static_cast< long int >( streamIter->second.ports[0].modelId ) );
                        xmlLink->GetFromModule()->SetData(
                            streamIter->second.ports[1].modelName,
                            static_cast< long int >( streamIter->second.ports[1].modelId ) );
                        *(xmlLink->GetFromPort()) = static_cast< long int >( streamIter->second.ports[1].id );
                        *(xmlLink->GetToPort()) = static_cast< long int >( streamIter->second.ports[0].id );
                    }*/
                }
                else if( !streamIter->second.ports[0].dataFlow.compare("output") )
                {
                    if( streamIter->second.ports.size() > 1 )
                    {
                        xmlLink->GetToModule()->SetData(
                            streamIter->second.ports[1].modelName,
                            static_cast< long int >( streamIter->second.ports[1].modelId ) );
                        xmlLink->GetFromModule()->SetData(
                            streamIter->second.ports[0].modelName,
                            static_cast< long int >( streamIter->second.ports[0].modelId ) );
                        *(xmlLink->GetFromPort()) = static_cast< long int >( streamIter->second.ports[0].id );
                        *(xmlLink->GetToPort()) = static_cast< long int >( streamIter->second.ports[1].id );
                    }
                    //if it only has one port
                    /*else
                    {
                        xmlLink->GetToModule()->SetData(
                            streamIter->second.ports[1].modelName,
                            static_cast< long int >( streamIter->second.ports[1].modelId ) );
                        xmlLink->GetFromModule()->SetData(
                            streamIter->second.ports[0].modelName,
                            static_cast< long int >( streamIter->second.ports[0].modelId ) );
                        *(xmlLink->GetFromPort()) = static_cast< long int >( streamIter->second.ports[0].id );
                        *(xmlLink->GetToPort()) = static_cast< long int >( streamIter->second.ports[1].id );
                    }*/
                }

                xmlLink->SetLinkName( streamIter->first );
                xmlLink->SetLinkType( 0 );
                
                for ( int i = 0; i < streamIter->second.vtx.size(); i++ )
                {
                    xmlLink->GetLinkPoint( i )->SetPoint( streamIter->second.vtx[i] );
                }

                //add the link
                subNetwork->AddLink( xmlLink );
            }
        }

        //create blocks
        std::map< std::string, block >::iterator blockIter;
        for ( blockIter = sheetIter->second.blocks.begin();
            blockIter != sheetIter->second.blocks.end();
            ++blockIter )
        {
            ves::open::xml::model::ModelPtr
                tempModel( new ves::open::xml::model::Model() );
            tempModel->SetModelID( blockIter->second.id );
            tempModel->SetPluginName( blockIter->second.name );        
            
            //check opc vector
            std::vector< std::string >::iterator iter;
            iter = find( m_opcBlocks.begin(), m_opcBlocks.end(),
                blockIter->second.name );
            if ( iter == m_opcBlocks.end() )    
            {
                tempModel->SetPluginType( "SimUOPlugin" );
            }
            else
            {
                tempModel->SetPluginType( "OpcUOPlugin" );
            }
            
            tempModel->SetVendorName( m_unitName );
            //tempModel->SetIconFilename( blockIter->second.cls );
            tempModel->SetIconFilename( GetDynSimIconPath( blockIter->second.cls, blockIter->second.imageType ) );
            tempModel->SetIconRotation( 0 );
            tempModel->SetIconScale( 1 );
            tempModel->SetIconMirror( 0 );
            tempModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >(
                blockIter->second.x, blockIter->second.y ) );
            tempModel->SetIconHiddenFlag( 0 );
            
            int portNum = blockIter->second.ports.size();
            for( size_t i = 0; i < portNum; i++)
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);
                // inputs are to ports
                tempPort->
                    SetPortNumber( blockIter->second.ports[i].id );
                tempPort->SetDataFlowDirection( blockIter->second.ports[i].dataFlow );
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( blockIter->second.ports[i].x, blockIter->second.ports[i].y) );
            }
            subSystem->AddModel(tempModel);
        
        }

        //create other
        std::map< std::string, block >::iterator otherIter;
        for ( otherIter = sheetIter->second.others.begin();
            otherIter != sheetIter->second.others.end();
            ++otherIter )
        {
            ves::open::xml::model::ModelPtr
                tempModel( new ves::open::xml::model::Model() );
            tempModel->SetModelID( otherIter->second.id );
            tempModel->SetPluginName( otherIter->second.name );

            //check opc vector
            std::vector< std::string >::iterator iter;
            iter = find( m_opcBlocks.begin(), m_opcBlocks.end(),
                otherIter->second.name );
            if ( iter == m_opcBlocks.end() )    
            {
                tempModel->SetPluginType( "SimUOPlugin" );
            }
            else
            {
                tempModel->SetPluginType( "OpcUOPlugin" );
            }
            
            tempModel->SetVendorName( m_unitName );
            tempModel->SetIconFilename( "sim");//.xpm" );
            tempModel->SetIconRotation( 0 );
            tempModel->SetIconScale( 1 );
            tempModel->SetIconMirror( 0 );
            tempModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >(
                otherIter->second.x, otherIter->second.y ) );
            tempModel->SetIconHiddenFlag( 0 );
            
            /*int portNum = blockIter->second.ports.size();
            for( size_t i = 0; i < portNum; i++)
            {
                ves::open::xml::model::PortPtr tempPort =
                    tempModel->GetPort(-1);
                // inputs are to ports
                tempPort->
                    SetPortNumber( blockIter->second.ports[i].id );
                tempPort->SetDataFlowDirection( blockIter->second.ports[i].dataFlow );
                tempPort->
                    GetPortLocation()->SetPoint( std::pair< unsigned int, unsigned int >
                    ( blockIter->second.ports[i].x, blockIter->second.ports[i].y) );
            }
            */
            subSystem->AddModel(tempModel);
        }

        //add the sub system to the flowsheet model
        flowSheetModel->SetSubSystem(subSystem);

        //add the flowsheet model to the top system / plugin
        veSystem->AddModel( flowSheetModel );
    }

    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( veSystem, "veSystem" ) );

    //generate xml packet
    std::string fileName( "returnString" );
    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    //std::ofstream output("xmlpacket.txt");
    //output << fileName;
    //output.close();
    return fileName;
}
///////////////////////////////////////////////////////////////////////////////
std::string DynSim::GetDynSimIconPath( std::string xmlName, std::string imageType )
{
    std::string dynsimInstallPath( DSS_VERSION );
    std::string dynsimXMLPath = dynsimInstallPath + std::string("IconPalette/DynsimEngine/" );
    std::string dynsimIconPath = dynsimInstallPath + std::string("Images/ClassIcons/");
    std::string xmlFilePath = dynsimXMLPath + xmlName + std::string( ".xml" );
    
    std::string gifName;
    std::ifstream inFile( xmlFilePath.c_str(), std::ifstream::in );
    
    //test that the file exists
    if(inFile.is_open())
    {
        //if( imageType.empty() )
        //{
        //    imageType = "ImageNormal";
        //}

        mParser->parse( xmlFilePath.c_str() );
       
        mCommandDocument = mParser->getDocument();
        root_elem = mCommandDocument->getDocumentElement();
        DOMNodeList* iconList = root_elem->getElementsByTagName(
            XMLString::transcode( "Icon" ) );

        int iconEntryCount = iconList->getLength();
        //loop over all icon entries and grab those that have the ImageNormal icon type
        for( int i = 0; i < iconEntryCount; i++ )
        {
            //xmlIcon tempIcon;
            DOMElement* iconElement =
                dynamic_cast<DOMElement*> ( iconList->item( i ) );
            char* fUnicodeForm = XMLString::transcode( 
                iconElement->getAttribute( XMLString::transcode( "Type" ) ) );
            std::string type( fUnicodeForm );

            //change this ImageNormal to the value of the entry from the xml file
            //once we parse it out
            if( !type.compare( imageType ) )
            {            
                //gif
                DOMNodeList* filenameList = iconElement->getElementsByTagName (
                    XMLString::transcode("FileName") );
                DOMElement* fileElement =
                    dynamic_cast<DOMElement*> ( filenameList->item( 0 ) );
                DOMText* rawText = dynamic_cast< DOMText* > ( fileElement->getFirstChild() );
                fUnicodeForm =
                    XMLString::transcode( rawText->getData() );
                std::string gif( fUnicodeForm );

                //scheme
                char* fUnicodeForm = XMLString::transcode(
                    iconElement->getAttribute( XMLString::transcode( "Scheme" ) ) );
                std::string scheme( fUnicodeForm );

                //if scheme returns empty there is no scheme attr so end the search
                if( scheme.empty() )
                {
                    gifName = gif;
                    break;
                }
                else
                {
                    //scheme = 3D end the search
                    if( !scheme.compare( "3D" ) )
                    {
                        gifName = gif;
                        break;
                    }
                    //else scheme = 2D continue
                }
            }

        }
    }
    else
    {
        gifName = xmlName + ".gif";
    }

    //return the full path
    return ( dynsimIconPath + gifName );
}
///////////////////////////////////////////////////////////////////////////////
/*void DynSim::ConnectWithList( std::vector< std::string > list )
{
    HRESULT hr;
    //server
    IOPCAutoServerPtr server( __uuidof(OPCServer) );
    server.AddRef();
    hr = server->Connect(_T("OPC.Gateway.Server.DA") );
    
    //if( FAILED( hr ) )
    //{
    //    return false;
    //}
    
    //groups
    groups = server->GetOPCGroups();
    groups.AddRef();
    groups->DefaultGroupIsActive = true;
    groups->DefaultGroupDeadband = 0;

    //group
    group = groups->Add( _T( "Group" ) );
    group.AddRef();
    group->UpdateRate = 100;

    //Browser
    OPCBrowserPtr browser = server->CreateBrowser();
    browser.AddRef();
    browser->ShowLeafs();

    //items
    items = group->GetOPCItems();
    items.AddRef();
    items->DefaultIsActive = true;

    itemIDs = new CComSafeArray<BSTR>( list.size() + 1 );
    clientID = new CComSafeArray<long>( list.size() + 1 );
    serverID = new CComSafeArray<long>();
    serverID->Create();
    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    for( int i = 1; i <= list.size(); i++)
    {
        std::string modnameOPC = m_opcFlowsheetName + "_" + list[i-1]+".POS";
        _bstr_t itemName = modnameOPC.c_str();
        itemIDs->SetAt( i, browser->GetItemID( modnameOPC.c_str() ) );
        clientID->SetAt( i, i );
    }

    hr = items->AddItems(list.size(), itemIDs->GetSafeArrayPtr(), clientID->GetSafeArrayPtr(),
        serverID->GetSafeArrayPtr(), errors->GetSafeArrayPtr());

    if( FAILED( hr ) )
    {
        return;
    }

    group->IsSubscribed = true;
    group->IsActive = true;

    //server->Disconnect();
}*/
///////////////////////////////////////////////////////////////////////////////
/*bool DynSim::ConnectToOPC()
{
        // TODO: Add your control notification handler code here
    //OnOK();
    HRESULT hr;
    //server
    IOPCAutoServerPtr server( __uuidof(OPCServer) );
    //_variant_t servCount = server->GetOPCServers();
    server.AddRef();
    hr = server->Connect(_T("OPC.Gateway.Server.DA") );
    
    if( FAILED( hr ) )
    {
        return false;
    }

    
    //groups
    groups = server->GetOPCGroups();
    groups.AddRef();
    groups->DefaultGroupIsActive = true;
    groups->DefaultGroupDeadband = 0;

    //group
    group = groups->Add( _T( "Group" ) );
    group.AddRef();
    group->UpdateRate = 100;

    //Browser
    OPCBrowserPtr browser = server->CreateBrowser();
    browser.AddRef();
    browser->ShowLeafs();
    long browserCount = browser->GetCount();

    //items
    items = group->GetOPCItems();
    items.AddRef();
    items->DefaultIsActive = true;

    //CComSafeArray<BSTR> * itemIDs;
    itemIDs = new CComSafeArray<BSTR>( browserCount + 1 );
    
    CComSafeArray<long> * clientID;
    clientID = new CComSafeArray<long>( browserCount + 1 );

    //CComSafeArray<long> * serverID;
    serverID = new CComSafeArray<long>();
    serverID->Create();

    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    //add items
    //CComboBox* pLB = (CComboBox*) GetDlgItem( IDC_COMBO1 );
    for( long i = 1; i <= browserCount; i++ )
    {
        _bstr_t itemName = browser->Item( i );
        itemIDs->SetAt( i, browser->GetItemID(itemName) );
        clientID->SetAt( i, i );
        //items->AddItem( browser->GetItemID(itemName), i);
        //pLB->InsertString( -1 , itemName );
    }

    hr = items->AddItems(browserCount, itemIDs->GetSafeArrayPtr(), clientID->GetSafeArrayPtr(),
        serverID->GetSafeArrayPtr(), errors->GetSafeArrayPtr());

    if( FAILED( hr ) )
    {
        return false;
    }

    group->IsSubscribed = true;
    group->IsActive = true;

    return true;
    
    //An atttempt to set up the evt handler for auto reading
    //OPCServer *m_pSrc;
    //m_pSink = new MFCSink;

    //hr=
    //    CoCreateInstance(__uuidof(OPCServer),NULL,CLSCTX_INPROC_SERVER,IID_IDispatch,(void**) &m_pSrc);
        //LPUNKNOWN m_pUnk=m_pSink->GetIDispatch(FALSE);
        //LPUNKNOWN pUnkSrc;
        //hr = pUnkSrc->QueryInterface(__uuidof(OPCGroup), (void**)&group);
    //m_dwCookie=1;
    //hr = AfxConnectionAdvise(group, __uuidof(DIOPCGroupEvent), m_pUnk, FALSE,
    //    &m_dwCookie);



    //server->Disconnect();
    //server.Release();
}*/
///////////////////////////////////////////////////////////////////////////////
bool DynSim::ConnectToOPCServer()
{
    //Connect to the OPC Server
    HRESULT hr;
    IOPCAutoServerPtr server( __uuidof(OPCServer) );
    m_server = server;
    m_server.AddRef();
    hr = m_server->Connect(_T("OPC.Gateway.Server.DA") );
    
    if( FAILED( hr ) )
    {
        return false;
    }

    //initialize necessary OPC structures
    groups = m_server->GetOPCGroups();
    groups.AddRef();
    groups->DefaultGroupIsActive = true;
    groups->DefaultGroupDeadband = 0;

    group = groups->Add( _T( "Group" ) );
    group.AddRef();
    group->UpdateRate = 100;

    items = group->GetOPCItems();
    items.AddRef();
    items->DefaultIsActive = true;

    return true;
}
///////////////////////////////////////////////////////////////////////////////
std::string DynSim::GetAllOPCVariables( const std::string& modname )
{
    //Get a list of the available OPC variables for a given unit op
    browser = m_server->CreateBrowser();
    browser.AddRef();
    browser->ShowLeafs();
    long browserCount = browser->GetCount();
    std::vector< std::string > tempVars;
    for( long i = 1; i <= browserCount; i++ )
    {
        _bstr_t itemName = browser->Item( i );
        std::string temp = itemName;

        if( temp.find( modname ) != std::string::npos )
        {
            tempVars.push_back( temp );
        }
    }

    itemIDs = new CComSafeArray<BSTR>( tempVars.size() + 1 );
    clientID = new CComSafeArray<long>( tempVars.size() + 1 );
    serverID = new CComSafeArray<long>( );
    serverID->Create();
    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    //base 1 for safearray
    for( long i = 0; i < tempVars.size(); i++)
    {
            //add all the new variables to the itemIDs for reading values
            itemIDs->SetAt( i + 1, browser->GetItemID( tempVars[i].c_str() ) );
            clientID->SetAt( i + 1, i + 1 );
    }

    //HRESULT hr = items->AddItems(opcVariables.size(), itemIDs->GetSafeArrayPtr(),
    //    clientID->GetSafeArrayPtr(), serverID->GetSafeArrayPtr(),
    //    errors->GetSafeArrayPtr());
    HRESULT hr = items->AddItems( tempVars.size(), itemIDs->GetSafeArrayPtr(),
        clientID->GetSafeArrayPtr(), serverID->GetSafeArrayPtr(),
        errors->GetSafeArrayPtr());

    CComSafeArray<VARIANT> * values;
    values = new CComSafeArray<VARIANT>();
    values->Create();

    VARIANT quality;
    VariantInit(&quality);

    VARIANT timestamp;
    VariantInit(&timestamp);

    long count = serverID->GetUpperBound();
    //This function reads the value, quality and timestamp information for one
    //or more items in a group.
    group->SyncRead( OPCDataSource::OPCDevice, count, serverID->GetSafeArrayPtr(),
        values->GetSafeArrayPtr(), errors->GetSafeArrayPtr(), &quality, &timestamp );

    std::vector< std::pair< std::string, std::string > > varsAndVals;
    for( int i = 1; i <= count; i++)
    {
        values->GetAt(i).ChangeType(VT_BSTR);
        std::pair< std::string, std::string > varAndVal;
        std::string temp = _bstr_t( itemIDs->GetAt(i) );

        //remove everything but the variable ie remove the unit name
        //everything before the "."
        varAndVal.first = temp.substr( temp.find(".") + 1, temp.size() - temp.find(".") + 1 );
        varAndVal.second = _bstr_t( values->GetAt(i).bstrVal );
        varsAndVals.push_back( varAndVal );
    }

    items->Remove( count, serverID->GetSafeArrayPtr(), errors->GetSafeArrayPtr());
    //append the flowsheet name
    ves::open::xml::CommandPtr varsAndValues( new ves::open::xml::Command() );
    varsAndValues->SetCommandName("AllOPCData");
    //compose return packet
    //loop over the variables and add as dvps
    for( int i = 0; i < varsAndVals.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData( varsAndVals[i].first, varsAndVals[i].second );
        varsAndValues->AddDataValuePair( entry );
    }
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( varsAndValues, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::AddOPCVariable( const std::string& var )
{
    //std::vector<std::string>::const_iterator pos = 
    //    lower_bound( m_opcVariables.begin(), m_opcVariables.end(), var );
    bool found = binary_search( m_opcVariables.begin(), m_opcVariables.end(), var );

    //if( pos != m_opcVariables.end() || m_opcVariables.empty() )
    if( !found || m_opcVariables.empty() )
    {
        m_opcVariables.push_back( var );
        UpdateOPCList();
    }
}
///////////////////////////////////////////////////////////////////////////////
void DynSim::UpdateOPCList( )
{
    //groups = m_server->GetOPCGroups();
    //groups.AddRef();
    //groups->DefaultGroupIsActive = true;
    //groups->DefaultGroupDeadband = 0;

    //group = groups->Add( _T( "Group" ) );
    //group.AddRef();
    //group->UpdateRate = 100;

    //OPCBrowserPtr browser = m_server->CreateBrowser();
    //browser.AddRef();
    //browser->ShowLeafs();

    //items = group->GetOPCItems();
    //items.AddRef();
    //items->DefaultIsActive = true;

    itemIDs = new CComSafeArray<BSTR>( m_opcVariables.size() + 1 );
    clientID = new CComSafeArray<long>( m_opcVariables.size() + 1 );
    serverID = new CComSafeArray<long>();
    serverID->Create();
    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    for( int i = 1; i <= m_opcVariables.size(); i++)
    {
        std::string modnameOPC =
            m_opcFlowsheetName + "_" + m_opcVariables[i-1];//+".POS";
        _bstr_t itemName = modnameOPC.c_str();
        itemIDs->SetAt( i, browser->GetItemID( modnameOPC.c_str() ) );
        clientID->SetAt( i, i );
    }

    HRESULT hr = items->AddItems(m_opcVariables.size(), itemIDs->GetSafeArrayPtr(),
        clientID->GetSafeArrayPtr(), serverID->GetSafeArrayPtr(),
        errors->GetSafeArrayPtr());

    if( FAILED( hr ) )
    {
        return;
    }

    group->IsSubscribed = true;
    group->IsActive = true;
}

///////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > > DynSim::ReadVars()
//std::map< std::string, std::pair< std::string, VARTYPE > > DynSim::ReadVars()
{
    UpdateOPCList();

    CComSafeArray<VARIANT> * values;
    values = new CComSafeArray<VARIANT>();
    values->Create();

    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    VARIANT quality;
    VariantInit(&quality);

    VARIANT timestamp;
    VariantInit(&timestamp);

    long count = serverID->GetUpperBound();
    //This function reads the value, quality and timestamp information for one
    //or more items in a group.
    group->SyncRead( OPCDataSource::OPCDevice, count, serverID->GetSafeArrayPtr(),
        values->GetSafeArrayPtr(), errors->GetSafeArrayPtr(), &quality, &timestamp );

    //std::vector< std::pair< std::string, std::string > > nameAndValues;
    //nameValVar.clear();
    nameAndValues.clear();

    for( int i = 1; i <= count; i++)
    {
        std::pair< std::string, std::string > nameNval;
        //std::pair< std::string, VARTYPE > tempValVar;
        //std::pair< std::string, std::pair< std::string, VARTYPE> > tempNameValVar;

        //tempValVar.second = values->GetAt(i).vt;
        values->GetAt(i).ChangeType(VT_BSTR);

        //this entry has the opc prefix appended
        //nameNval.first = _bstr_t( itemIDs->GetAt(i) );
        //this one doesn't - +1 is for the "_"
        std::string temp = _bstr_t( itemIDs->GetAt(i) );
        nameNval.first = temp.substr( m_opcFlowsheetName.size() + 1,
            temp.size() - (m_opcFlowsheetName.size() + 1) - (temp.size() - temp.find(".") ) );        
        nameNval.second = _bstr_t( values->GetAt(i).bstrVal );
        
        //nameValVar[
        //temp.substr( m_opcFlowsheetName.size() + 1,
        //    temp.size() - (m_opcFlowsheetName.size() + 1) - (temp.size() - temp.find(".") ) )] = tempValVar;


        //tempNameValVar.second = tempValVar;
        nameAndValues.push_back( nameNval );
    }

    return nameAndValues;
}

///////////////////////////////////////////////////////////////////////////////
/*std::string DynSim::GetOPCValue( const std::string& modname )
{
    ReadVars();

    //append the flowsheet name
    std::string modnameOPC = m_opcFlowsheetName + "_" + modname;

    ves::open::xml::CommandPtr params( new ves::open::xml::Command() );
    std::vector<std::string> paramList;
    //input variables;
    params->SetCommandName((modnameOPC+"OPCValue").c_str());
    
    std::vector< std::pair <std::string, std::string> >::iterator nameValueIter;
    std::string opcValue = "ERROR";
    for( nameValueIter = nameAndValues.begin();
        nameValueIter != nameAndValues.end();
        ++nameValueIter )
    {
        if( !nameValueIter->first.compare( modnameOPC ) )
        {
            opcValue = nameValueIter->second;
            break;
        }
    }

    ves::open::xml::DataValuePairPtr
        inpParams( new ves::open::xml::DataValuePair() );
    inpParams->SetData("opcvalue", opcValue );
    params->AddDataValuePair( inpParams );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( params, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}*/
///////////////////////////////////////////////////////////////////////////////
//depending on the end desires for the code this function can be combine with ReadVars()
std::string DynSim::GetOPCValues( )
{
    if( m_opcVariables.empty() )
    {
        return "NULL";
    }

    ReadVars();

    if( nameAndValues.empty() )
    {
        return "NULL";
    }

    //append the flowsheet name
    ves::open::xml::CommandPtr varAndValues( new ves::open::xml::Command() );
    varAndValues->SetCommandName("OPCData");

    //loop over the variables and add as dvps
    for( int i = 0; i < nameAndValues.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData( nameAndValues[i].first, nameAndValues[i].second );
        varAndValues->AddDataValuePair( entry );
    }

    //std::map< std::string, std::pair< std::string, VARTYPE > >::iterator iter;
    //for( iter = nameValVar.begin();
    //    iter != nameValVar.end();
    //    ++iter )
    //{
    //    ves::open::xml::DataValuePairPtr
    //         entry( new ves::open::xml::DataValuePair() );
    //    entry->SetData( iter->first, iter->second.first );
    //    varAndValues->AddDataValuePair( entry );
    //}

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( varAndValues, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return status;
}

///////////////////////////////////////////////////////////////////////////////
void DynSim::SetOPCValues( std::vector< std::pair < std::string, std::string > > varAndValues )
{
    long count = varAndValues.size();

    browser = m_server->CreateBrowser();
    browser.AddRef();
    browser->ShowLeafs();

    CComSafeArray<VARIANT> * values;
    values = new CComSafeArray<VARIANT>( count + 1 );
    values->Create();

    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    IOPCGroupPtr setGroup;
    setGroup = groups->Add( _T( "SetGroup" ) );
    setGroup.AddRef();
    
    OPCItemsPtr setItems;
    setItems = setGroup->GetOPCItems();
    setItems.AddRef();
    setItems->DefaultIsActive = true;
    
    CComSafeArray<BSTR> * setItemIDs;
    setItemIDs= new CComSafeArray<BSTR>( count + 1 );
    CComSafeArray<long> * setClientID;
    setClientID = new CComSafeArray<long>( count + 1 );
    
    CComSafeArray<long> * setServerID;
    setServerID= new CComSafeArray<long>( );
    setServerID->Create();

    //base 1 for safearray
    for( long i = 0; i < count; i++)
    {
        //add all the new variables to the itemIDs for reading values
        std::string temp = m_opcFlowsheetName + "_" +  varAndValues[i].first;
        setItemIDs->SetAt( i + 1, browser->GetItemID( temp.c_str() ) );
        setClientID->SetAt( i + 1, i + 1 );
    }

    //HRESULT hr = items->AddItems(opcVariables.size(), itemIDs->GetSafeArrayPtr(),
    //    clientID->GetSafeArrayPtr(), serverID->GetSafeArrayPtr(),
    //    errors->GetSafeArrayPtr());
    HRESULT hr = setItems->AddItems( count, setItemIDs->GetSafeArrayPtr(),
        setClientID->GetSafeArrayPtr(), setServerID->GetSafeArrayPtr(),
        errors->GetSafeArrayPtr());
    
    VARIANT tempValue;
    VariantInit( &tempValue );

    for( int i = 1; i <= count; i++)
    {
        tempValue.vt = VT_BSTR;
        _bstr_t bstrValue = varAndValues[i-1].second.c_str();
        tempValue.bstrVal = bstrValue;

        //NEED TO ADD CONVERSION TO THE PROPER TYPE using ChangeType()
        //currently hardcoded to Integer
        values->SetAt( i, tempValue );
        
        //get the correct vartype and set it before send data
        //values->GetAt(i).ChangeType( nameValVar[varAndValues[i-1].first].second );

        //temporary very bloated way of getting the vartype
            
        
        //Get a list of the available OPC variables for a given unit of
            std::string tempVar;
            long browserCount = browser->GetCount();
            for( long j = 1; j <= browserCount; j++ )
            {
                _bstr_t itemName = browser->Item( j );
                std::string temp = itemName;
                if( temp.find( varAndValues[i-1].first ) != std::string::npos )
                {
                    tempVar = temp;
                    break;
                }
            }

            CComSafeArray<BSTR> * tempItemIDs;
            tempItemIDs = new CComSafeArray<BSTR>( 2);
            
            CComSafeArray<long> * tempClientID;
            tempClientID = new CComSafeArray<long>( 2 );
            
            CComSafeArray<long> * tempServerID;
            tempServerID = new CComSafeArray<long>( );
            tempServerID->Create();
            
            CComSafeArray<long> * tempErrors;
            tempErrors = new CComSafeArray<long>();
            tempErrors->Create();

            tempItemIDs->SetAt( 1, browser->GetItemID( tempVar.c_str() ) );
            tempClientID->SetAt( 1, 1 );
            
            OPCItemsPtr tempItems;
            HRESULT hr = items->AddItems( 1,tempItemIDs->GetSafeArrayPtr(),
                tempClientID->GetSafeArrayPtr(), tempServerID->GetSafeArrayPtr(),
                tempErrors->GetSafeArrayPtr());

            CComSafeArray<VARIANT> * tempValue;
            tempValue = new CComSafeArray<VARIANT>();
            tempValue->Create();

            VARIANT tempQuality;
            VariantInit(&tempQuality);

            VARIANT tempTimestamp;
            VariantInit(&tempTimestamp);

            long count = tempServerID->GetUpperBound();
            //This function reads the value, quality and timestamp information for one
            //or more items in a group.
            group->SyncRead( OPCDataSource::OPCDevice, 1, tempServerID->GetSafeArrayPtr(),
                tempValue->GetSafeArrayPtr(), tempErrors->GetSafeArrayPtr(), &tempQuality, &tempTimestamp );
            
        values->GetAt(i).ChangeType( tempValue->GetAt(1).vt );

        //This function writes the value more items in a group.
        setGroup->SyncWrite( count, setServerID->GetSafeArrayPtr(),
            values->GetSafeArrayPtr(), errors->GetSafeArrayPtr() );
    }
    groups->Remove( _T( "SetGroup" ) );
}

///////////////////////////////////////////////////////////////////////////////
bool DynSim::IsOPCVarsEmpty()
{
    return m_opcVariables.empty();
}