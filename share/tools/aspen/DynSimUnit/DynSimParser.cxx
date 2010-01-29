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

#include "DynSimParser.h"

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

XERCES_CPP_NAMESPACE_USE

DynSimParser::DynSimParser()
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
}

std::string DynSimParser::CreateNetwork( std::string filename )
{
    m_fileName = filename + ".xml";
    InitializeParser();
    ParseFlowsheets();
    //PopulateStreams();
    return CreateVESNetwork();
}


int DynSimParser::OpenFile( std::string filename )
{
    std::string tempFilename = filename;
    //find and replace \ with /
    size_t pos = tempFilename.find( "\\" );
    while( pos != std::string::npos )
    {
        tempFilename.replace(pos, 1, "/");
        pos = tempFilename.find( "\\" );
    }

    //create the command
    std::string command =
        "C:/SIMSCI/DSS43/GUI/Bin/runSIM4ME_Dynsim.bat \"C:\\Documents and Settings\\tjordan\\Desktop\\VES_DynSim\\amon\\Ammonia_Reactor.s4m\"";

    //make the call
    return system( command.c_str() );
}

void DynSimParser::InitializeParser( )
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

void DynSimParser::ParseFlowsheets( )
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
        flowsheets[name].cls = "Flowsheet";

        blocks.clear();
        streams.clear();
    }
}

void DynSimParser::ParseObjects( DOMElement* sys_element )
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

void DynSimParser::PopulateBlocks( DOMElement* sys_element )
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
        DOMText* rawText =
                dynamic_cast< DOMText* >
                ( element->getFirstChild() );
        char* fUnicodeForm =
            XMLString::transcode( rawText->getData() );
        std::string name( fUnicodeForm );

        if( blocks.find( name ) != blocks.end() )  
        {
            //id
            blocks[name].id = blockCount++;

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
    }
}

//void DynSimParser::PopulateStreams()
//{
//}

std::string DynSimParser::CreateVESNetwork()
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
        flowSheetModel->SetPluginType( "SDPlugin" );
        flowSheetModel->SetVendorName( "DYNSIMUNIT" );
        flowSheetModel->SetIconFilename( "C:/SIMSCI/DSS43/GUI/Images/ClassIcons/" + sheetIter->second.cls + ".gif" );
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
            if( !streamIter->second.cls.empty() )
            {
                ves::open::xml::model::LinkPtr
                    xmlLink( new ves::open::xml::model::Link() );
                if( !streamIter->second.ports[0].dataFlow.compare("input") )
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
                else
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
            tempModel->SetPluginType( "SDPlugin" );
            tempModel->SetVendorName( "DYNSIMUNIT" );
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
    std::ofstream output("xmlpacket.txt");
    output << fileName;
    output.close();
    return fileName;
}

std::string DynSimParser::GetDynSimIconPath( std::string xmlName, std::string imageType )
{
    std::string dynsimInstallPath( "C:/SIMSCI" );
    std::string dynsimXMLPath = dynsimInstallPath + std::string("/DSS43/GUI/IconPalette/DynsimEngine/" );
    std::string dynsimIconPath = dynsimInstallPath + std::string("/DSS43/GUI/Images/ClassIcons/");
    std::string xmlFilePath = dynsimXMLPath + xmlName + std::string( ".xml" );
    mParser->parse( xmlFilePath.c_str() );
    
    mCommandDocument = mParser->getDocument();
    root_elem = mCommandDocument->getDocumentElement();
    DOMNodeList* iconList = root_elem->getElementsByTagName(
        XMLString::transcode( "Icon" ) );

    std::string gifName;

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

    //return the full path
    return ( dynsimIconPath + gifName );
}

bool DynSimParser::ConnectToOPC()
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
}

std::vector< std::pair< std::string, std::string > > DynSimParser::ReadVars()
{

    //VARIANT bItem;
	//::VariantInit(&bItem);
    //bItem.vt = VT_BSTR;
    //bItem.bstrVal = text.AllocSysString();

    //items->DefaultIsActive = true;

    //OPCItemPtr item = items->Item( bItem );

   // CComSafeArray<long> * handles;
   // handles = new CComSafeArray<long>(2, 0);
   // handles->SetAt(1, item->ServerHandle);

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

    std::vector< std::pair< std::string, std::string > > nameAndValues;

    for( int i = 1; i <= count; i++)
    {
        //itemIDs->GetAt(i).ChangeType(VT_BSTR);
        values->GetAt(i).ChangeType(VT_BSTR);
        std::pair< std::string, std::string > nameNval;
        nameNval.first = _bstr_t( itemIDs->GetAt(i) );
        nameNval.second = _bstr_t( values->GetAt(i).bstrVal );

        nameAndValues.push_back( nameNval );
    }

    return nameAndValues;


    //CEdit* e1 = (CEdit*) GetDlgItem( IDC_EDIT1 );
    //values->GetAt(1).ChangeType(VT_BSTR);
    //e1->SetWindowTextW( values->GetAt(1).bstrVal );
}
