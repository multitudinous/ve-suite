/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include "OPC.h"

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

//#define OPC_SERVER_NAME L"Softing.OPCToolboxDemo_ServerDA.1"
#define OPC_SERVER_NAME L"Woodward.ServLinkOpcDa.1"
#define ITEM_ID L"maths.sin"
#define VT VT_R4
#define XVAL fltVal

#define LOG_OPC_VARS 0

XERCES_CPP_NAMESPACE_USE

///////////////////////////////////////////////////////////////////////////////
OPC::OPC( std::string unitName )
{   
    try
    {
       XMLPlatformUtils::Initialize();
    }
    catch ( const XMLException &toCatch )
    {
    //XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
    //<< "  Exception message:"
    //<< XMLString::transcode( toCatch.getMessage() ) << XERCES_STD_QUALIFIER endl;
    //return 1;
    }
    m_unitName = unitName;
}

///////////////////////////////////////////////////////////////////////////////
//OPC::~OPC( std::string unitName )
//{
	// Remove the groups
    //for
	//HRESULT hr = m_Server->RemoveGroup(hServerGroup, FALSE);
	//_ASSERT(!hr);
//}

///////////////////////////////////////////////////////////////////////////////
bool OPC::ConnectToOPCServer()
{   
    m_AllItemMgt = 0;
    m_SetItemMgt = 0;
    m_MonitorItemMgt = 0;

    if( m_serverName.empty() )
    {
        return false;
    }
 
	// Instantiante the IOPCServer
	m_Server = InstantiateServer(NULL);//OPC_SERVER_NAME);
    //add a group that contains all branches and leaves
	AddGroup(m_Server, m_AllItemMgt, m_AllGroup, "All");
    //add the set group
	AddGroup(m_Server, m_SetItemMgt, m_SetGroup, "Set");
    //add the monitor group
	AddGroup(m_Server, m_MonitorItemMgt, m_MonitorGroup, "Monitor");

    return true;
}
///////////////////////////////////////////////////////////////////////////////
std::string OPC::GetAllOPCVariables( const std::string& modname )
{
    //clear previous requests
    m_AllVarsAndVals.clear();

    //attach the browser to the server
    m_Server->QueryInterface(&browse);

    //Parse through all the available items
    Parse( "" );

    //release the browser
    m_Server->Release();

    //define local variales
    OPCITEMRESULT *pResults = NULL;
    HRESULT *pErrors = NULL;
    int count = m_AllVarsAndVals.size();
    
    //create the required OPCITEMDEF array
    OPCITEMDEF *pItemArray =
        (OPCITEMDEF *) CoTaskMemAlloc ( count * sizeof (OPCITEMDEF) );
    for( int i = 0; i < count; i++ )
    {        
        pItemArray [i].szAccessPath = NULL;
        //pItemArray [i].szItemID = CA2W( m_AllVarsAndVals[i].first.c_str() );
        pItemArray [i].szItemID = m_szItemID[i];
        pItemArray [i].bActive = true;
        pItemArray [i].hClient = 1; //we need a better client id
        pItemArray [i].dwBlobSize = 0;
        pItemArray [i].pBlob = NULL;
        pItemArray [i].vtRequestedDataType = VT_BSTR;//pItem->GetDataType ();
    }

    //add that array of items to the group
    m_AllItemMgt->AddItems( count, pItemArray, &pResults, &pErrors );

    //create an array of the server's id for the items
    OPCHANDLE *hServerItem =
        (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = pResults[i].hServer;
    }
	//get a pointer to the IOPCSyncIOInterface:
	OPCITEMSTATE* pValue = NULL;
	IOPCSyncIO* pIOPCSyncIO;
	m_AllItemMgt->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

    if( m_opcReadIO == "device" )
    {
        //device - slower but more up to date
        HRESULT hr =
            pIOPCSyncIO->Read(OPC_DS_DEVICE, count, hServerItem, &pValue, &pErrors);
    }
    else if( m_opcReadIO == "cache" )
    {
        //cache - faster but older data
	    HRESULT hr =
            pIOPCSyncIO->Read(OPC_DS_CACHE, count, hServerItem, &pValue, &pErrors);
    }
	
    //Error handling
    _ASSERT(!hr);
	_ASSERT(pValue!=NULL);

    //loop through all the values and add them to the data to be sent to VES
    for( int i = 0; i < m_AllVarsAndVals.size(); i++ )
    {
        m_AllVarsAndVals[i].second = _bstr_t(pValue[i].vDataValue);
    }

    //Remove the items
    m_AllItemMgt->RemoveItems( count, hServerItem, &pErrors );
       
	// release the reference to the IOPCSyncIO interface:
	//pIOPCSyncIO->Release();
    m_AllItemMgt->Release();
	
    //cleanup
	CoTaskMemFree(pErrors);
	CoTaskMemFree(pValue);
	CoTaskMemFree(pItemArray);
    CoTaskMemFree(hServerItem);
    CoTaskMemFree(pIOPCSyncIO);
	pErrors = NULL;
	pValue = NULL;
	pValue = NULL;
    hServerItem = NULL;
    pIOPCSyncIO = NULL;
    
    //compose return packet
    /*ves::open::xml::CommandPtr varsAndValues( new ves::open::xml::Command() );
    varsAndValues->SetCommandName("AllOPCData");

    //loop over the variables and add as dvps
    for( int i = 0; i < m_AllVarsAndVals.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->
            SetData( m_AllVarsAndVals[i].first, m_AllVarsAndVals[i].second );
        varsAndValues->AddDataValuePair( entry );
    }
    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( varsAndValues, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );*/
    return std::string();
}
///////////////////////////////////////////////////////////////////////////////
void OPC::AddOPCVariable( const std::string& var )
{
    //std::vector<std::string>::const_iterator pos = 
    //    lower_bound( m_opcVariables.begin(), m_opcVariables.end(), var );
    bool found =
        binary_search( m_opcVariables.begin(), m_opcVariables.end(), var );

    //if( pos != m_opcVariables.end() || m_opcVariables.empty() )
    if( !found || m_opcVariables.empty() )
    {
        m_opcVariables.push_back( var );
        //UpdateOPCList();

        //define local variales
        OPCITEMRESULT *pResults = NULL;
        HRESULT *pErrors = NULL;
        //create the required OPCITEMDEF array
        OPCITEMDEF pItemArray;
        pItemArray.szAccessPath = NULL;
        pItemArray.szItemID = CA2W( var.c_str() );
        pItemArray.bActive = true;
        pItemArray.hClient = 1;
        pItemArray.dwBlobSize = 0;
        pItemArray.pBlob = NULL;
        pItemArray.vtRequestedDataType = VT_BSTR;
        //add that array of items to the group
        m_MonitorItemMgt->AddItems( 1, &pItemArray, &pResults, &pErrors );
        m_MonitorServerItem.push_back( pResults[0].hServer );
    }
}
///////////////////////////////////////////////////////////////////////////////
/*void OPC::UpdateOPCList( )
{
    ///
    ///put new items to be monitored in list
    ///
}*/

///////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > > OPC::ReadVars()
{
    ///
    ///Read the values being monitored and put them back in the vector
    ///

    //get a pointer to the IOPCSyncIOInterface:
	OPCITEMSTATE* pValue = NULL;
	HRESULT* pErrors = NULL;
	IOPCSyncIO* pIOPCSyncIO;
	m_MonitorItemMgt->
        QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

    
    //create an array of the server's id for the items
    int count = m_MonitorServerItem.size();
    OPCHANDLE *hServerItem =
        (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = m_MonitorServerItem[i];
    }

	// read the item value from the device:
	HRESULT hr = pIOPCSyncIO->Read(OPC_DS_DEVICE, m_opcVariables.size(),
        hServerItem, &pValue, &pErrors);
    
    //Error handling
    _ASSERT(!hr);
	_ASSERT(pValue!=NULL);
    
    m_MonitorVarsAndVals.clear();
    for( int i = 0; i < m_opcVariables.size(); i++)
    {
        std::pair< std::string, std::string > nameNval;
        nameNval.first = m_opcVariables[i];
        nameNval.second = _bstr_t(pValue[i].vDataValue);
        m_MonitorVarsAndVals.push_back( nameNval );
    }

	// release the reference to the IOPCSyncIO interface:
	//pIOPCSyncIO->Release();
    m_MonitorItemMgt->Release();

	//cleanup
	CoTaskMemFree(pErrors);
	CoTaskMemFree(pValue);
    CoTaskMemFree(pIOPCSyncIO);
    CoTaskMemFree(hServerItem);
	pErrors = NULL;
	pValue = NULL;
    pIOPCSyncIO = NULL;
    hServerItem = NULL;
    
    return m_MonitorVarsAndVals;
}

///////////////////////////////////////////////////////////////////////////////
//depending on the end desires for the code this function can be combine
//with ReadVars()
std::string OPC::GetOPCValues( )
{
    if( m_opcVariables.empty() )
    {
        return "NULL";
    }

    ReadVars();

    if( m_MonitorVarsAndVals.empty() )
    {
        return "NULL";
    }

    //append the flowsheet name
    /*ves::open::xml::CommandPtr varAndValues( new ves::open::xml::Command() );
    varAndValues->SetCommandName("OPCData");

    //loop over the variables and add as dvps
    for( int i = 0; i < m_MonitorVarsAndVals.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData(
            m_MonitorVarsAndVals[i].first, m_MonitorVarsAndVals[i].second );
        varAndValues->AddDataValuePair( entry );
    }

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( 
    std::pair< ves::open::xml::XMLObjectPtr, std::string >
    ( varAndValues, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status="returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );*/

    return std::string();
}

///////////////////////////////////////////////////////////////////////////////
void OPC::SetOPCValues(
    std::vector< std::pair < std::string, std::string > > varAndValues )
{
    ///
    ///Set the values of the requested variables
    ///
    OPCITEMRESULT *pResults = NULL;
	HRESULT* pErrors = NULL;
    int count = varAndValues.size();
    
    //create the required OPCITEMDEF array
    OPCITEMDEF *pItemArray =
        (OPCITEMDEF *) CoTaskMemAlloc ( count * sizeof (OPCITEMDEF) );
    for( int i = 0; i < count; i++ )
    {   
        std::string temp = varAndValues[i].first.substr( 
            varAndValues[i].first.find_first_of(".") + 1,
            varAndValues[i].first.size() -
            ( varAndValues[i].first.find_first_of(".")+1 ) );
        pItemArray [i].szAccessPath = NULL;
        pItemArray [i].szItemID = CA2W( temp.c_str() );
        //pItemArray [i].szItemID = m_szItemID[117];
        pItemArray [i].bActive = true;
        pItemArray [i].hClient = 1; //we need a better client id
        pItemArray [i].dwBlobSize = 0;
        pItemArray [i].pBlob = NULL;
        pItemArray [i].vtRequestedDataType = VT_BSTR;//pItem->GetDataType ();
    }
    
    //add that array of items to the group
    m_SetItemMgt->AddItems( count, pItemArray, &pResults, &pErrors );

	//CoTaskMemFree(pErrors);
	//pErrors = NULL;
            
    //create an array of the server's id for the items
    OPCHANDLE *hServerItem =
        (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
	VARIANT* pValue =
        (VARIANT *) CoTaskMemAlloc ( count * sizeof (VARIANT) );
	
    //IOPCSyncIO2
    //OPCITEMVQT *pValue =
        //(OPCITEMVQT *) CoTaskMemAlloc ( count * sizeof (OPCITEMVQT) );

    //loop over and create a list of server ids and put the values in a list
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = pResults[i].hServer;

        pValue[i].vt = VT_BSTR;
        pValue[i].bstrVal = _bstr_t( varAndValues[i].second.c_str() );
        
        //IOPCSyncIO2
        //pValue[i].vDataValue.vt = VT_BSTR;
        //pValue[i].vDataValue.bstrVal =
            //_bstr_t( varAndValues[i].second.c_str() );
        //pValue[i].bQualitySpecified = false;
        //pValue[i].bTimeStampSpecified = false;
    }

	//get a pointer to the IOPCSyncIOInterface:
	IOPCSyncIO* pIOPCSyncIO;
	//IOPCSyncIO2* pIOPCSyncIO;

    //assign io interface to item management
	m_SetItemMgt->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);
        
	// write the item value(s)
	HRESULT hr = pIOPCSyncIO->Write( count, hServerItem, pValue, &pErrors);
	
    //IOPCSyncIO2
    //HRESULT hr=pIOPCSyncIO->WriteVQT( count, hServerItem, pValue, &pErrors);
    
    //Remove the items
    m_SetItemMgt->RemoveItems( count, hServerItem, &pErrors );
    m_SetItemMgt->Release();

    //cleanup
	CoTaskMemFree(pErrors);
    CoTaskMemFree(pResults);
    CoTaskMemFree(pItemArray);
    CoTaskMemFree(hServerItem);
    CoTaskMemFree(pValue);
    CoTaskMemFree(pIOPCSyncIO);
	pErrors = NULL;
	pResults = NULL;
    pItemArray = NULL;
    hServerItem = NULL;
    pValue = NULL;
    pIOPCSyncIO = NULL;

    return;
}

///////////////////////////////////////////////////////////////////////////////
bool OPC::IsOPCVarsEmpty() const
{
    return m_opcVariables.empty();
}
///////////////////////////////////////////////////////////////////////////////
IOPCServer* OPC::InstantiateServer(wchar_t ServerName[])
{
	CLSID CLSID_OPCServer;
	HRESULT hr;

	// get the CLSID from the OPC Server Name:
	//std::wstring serverName = L(m_serverName.c_str());
	wchar_t* wServerName = CA2W(m_serverName.c_str());
	hr = CLSIDFromString(wServerName, &CLSID_OPCServer);
	_ASSERT(!FAILED(hr));


	//queue of the class instances to create
	LONG cmq = 1; // nbr of class instance to create.
	MULTI_QI queue[1] =
		//{{&IID_IOPCServer,
        {{&__uuidof(IOPCServer),
		NULL,
		0}};

	// create an instance of the IOPCServer
	hr = CoCreateInstanceEx(CLSID_OPCServer, NULL, CLSCTX_SERVER,
		/*&CoServerInfo*/NULL, cmq, queue);
	_ASSERT(!hr);

	// return a pointer to the IOPCServer interface:
	return(IOPCServer*) queue[0].pItf;
}

///////////////////////////////////////////////////////////////////////////////
void OPC::AddGroup(IOPCServer* pIOPCServer, IOPCItemMgt* &pIOPCItemMgt, 
				 OPCHANDLE& hServerGroup, char* name)
{
	DWORD dwUpdateRate = 0;
	OPCHANDLE hClientGroup = 0;

	// Add an OPC group and get a pointer to the IUnknown I/F:
    HRESULT hr = pIOPCServer->AddGroup( CA2W(name),FALSE, dwUpdateRate,
        hClientGroup, 0, 0, 0, &hServerGroup, &dwUpdateRate,
        &GUID(__uuidof(IOPCItemMgt)),(IUnknown**) &pIOPCItemMgt);

	_ASSERT(!FAILED(hr));
}

///////////////////////////////////////////////////////////////////////////////
void OPC::Parse( std::string name )
{
    OPCHANDLE hServerGroup;
        
    //read the available vars
    LPWSTR pszContinuationPoint = NULL; 
    OPCBROWSEELEMENT *pBrowseElements = NULL;  
    long moreElements = 0; 
    //BOOL moreElements = false; 
    DWORD count = 0; 
    DWORD pdwPropertyIDs = 2; 

    HRESULT hr = browse->Browse( CA2W(name.c_str()), &pszContinuationPoint,
        0, OPC_BROWSE_FILTER_ALL, CA2W(""), CA2W(""), TRUE, TRUE, 1,
        &pdwPropertyIDs, &moreElements, &count, &pBrowseElements ); 
 
	if( SUCCEEDED(hr) ) { 
#if LOG_OPC_VARS
    std::ofstream output ( (name+".log").c_str() );
        output<<count<<std::endl;
#endif
        for( DWORD i=0; i < count; ++i )
        { 
#if LOG_OPC_VARS
            output << "Name: " << CW2A(pBrowseElements[i].szName) << std::endl;
#endif
            //std::wstring wtemp(pBrowseElements[i].szName);
            std::string temp( CW2A(pBrowseElements[i].szItemID) );
            
            //if element is an item add it to the available variables
            if( pBrowseElements[i].dwFlagValue & OPC_BROWSE_ISITEM )
            {
                //guard against an item that is a hint
                if( pBrowseElements[i].szItemID )
                {
                    std::pair< std::string, std::string > varAndVal;
                    varAndVal.first = temp;
                    //VARIANT value = 
                        //pBrowseElements[i].ItemProperties.
                        //pItemProperties[1].vValue;
                    //varAndVal.second = _bstr_t( value.bstrVal );
                    varAndVal.second = "missing";
                    m_AllVarsAndVals.push_back( varAndVal );
                    m_szItemID.push_back(pBrowseElements[i].szItemID);
                }
            } 
            
            //if it has children parse it
            if( pBrowseElements[i].dwFlagValue & OPC_BROWSE_HASCHILDREN )
            {
                //if( !name.empty() )
                {
                    Parse( temp );
                }
            }

		} 
#if LOG_OPC_VARS
        output.close();
#endif
    }
}
///////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > > OPC::GetAllRawOPCData() const
{
    return m_AllVarsAndVals;
}
///////////////////////////////////////////////////////////////////////////////
void OPC::SetOPCServerName( std::string const& opcServer )
{
    m_serverName = opcServer;
}
///////////////////////////////////////////////////////////////////////////////
void OPC::SetDeviceOrHardwareFlag( std::string const& opcDevice )
{
    m_opcReadIO = opcDevice;
}
///////////////////////////////////////////////////////////////////////////////
    