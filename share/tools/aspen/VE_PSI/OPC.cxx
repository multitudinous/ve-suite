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

//#define DSS_VERSION "C:/SIMSCI/DSS44/GUI/"

#define OPC_SERVER_NAME L"Softing.OPCToolboxDemo_ServerDA.1"
//#define OPC_SERVER_NAME L"Woodward.ServLinkOpcDa.1"
#define ITEM_ID L"maths.sin"
#define VT VT_R4
#define XVAL fltVal

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
    //    XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
    //    << "  Exception message:"
    //    << XMLString::transcode( toCatch.getMessage() ) << XERCES_STD_QUALIFIER endl;
    //    return 1;
    }
    m_unitName = unitName;
}

///////////////////////////////////////////////////////////////////////////////
bool OPC::ConnectToOPCServer()
{   
	//IOPCItemMgt* pIOPCItemMgt = NULL; //pointer to IOPCItemMgt interface
	//OPCHANDLE hServerGroup; // server handle to the group
	//OPCHANDLE hServerItem;  // server handle to the item
    
	// Instantiante the IOPCServer
	m_Server = InstantiateServer(OPC_SERVER_NAME);
    //add a group that contains all branches and leaves
	AddGroup(m_Server, m_AllItemMgt, m_AllGroup, "All");
    //add the set group
	AddGroup(m_Server, m_SetItemMgt, m_SetGroup, "Set");
    //add the monitor group
	AddGroup(m_Server, m_MonitorItemMgt, m_MonitorGroup, "Monitor");

    //m_Server->QueryInterface(&browse);
    //Parse( "" );

	//IOPCServer* pIOPCServer = NULL;   //pointer to IOPServer interface
    //IOPCAutoServerPtr server( __uuidof(OPCServer) );
    //CComPtr<IOPCServer> server( );//__uuidof(IOPCServer) );
    //OPCSERVERSTATUS *serverStatus;
    //m_server->GetStatus(&serverStatus);
    //std::string test( serverStatus->szVendorInfo );
    //pIOPCServer = InstantiateServer(OPC_SERVER_NAME);
    //IOPCBrowse* browse = NULL;
    
    /*        std::ofstream output("log.txt");
        output<<"browse"<<std::endl;
    //HRESULT hr = browse->Browse( CA2W(name.c_str()),  
    hr = browse->Browse( CA2W(""),  
			&pszContinuationPoint,  
			0,  
			OPC_BROWSE_FILTER_ALL 
			, CA2W("") // [in, string] LPWSTR szElementNameFilter 
			, CA2W("") //[in, string] LPWSTR szVendorFilter 
			, TRUE //[in] BOOL bReturnAllProperties 
			, FALSE //[in] BOOL bReturnPropertyValues 
			, 1 //[in] DWORD dwPropertyCount 
			, &pdwPropertyIDs //[in, size_is(dwPropertyCount)] DWORD * pdwPropertyIDs 
			, &moreElements //[out] BOOL * pbMoreElements 
			, &count  //[out] DWORD * pdwCount 
			, &pBrowseElements //[out, size_is(,*pdwCount)] OPCBROWSEELEMENT ** ppBrowseElements  
			); 
        output<<count<<std::endl;
 
	//if( SUCCEEDED(hr) ) { 
		for( DWORD i=0; i < count; ++i )
        { 
            output << "Name: " << CW2A(pBrowseElements[i].szName) << std::endl;
		} 
        output.close();
    //}
    */
	// Add the OPC group the OPC server and get an handle to the IOPCItemMgt
	//interface:
	//AddTheGroup(pIOPCServer, pIOPCItemMgt, hServerGroup);
//    AddTheGroup(m_Server, pIOPCItemMgt, hServerGroup);

	// Add the OPC item
//    AddTheItem(pIOPCItemMgt, hServerItem);

	//Read the value of the item from device:
//	VARIANT varValue; //to stor the read value
//	VariantInit(&varValue);
//	ReadItem(pIOPCItemMgt, hServerItem, varValue);

	// print the read value:
//	std::ofstream output("log.txt");
//    output << "Read value: " << varValue.XVAL << std::endl;
//    output.close();

	// Remove the OPC item:
//	RemoveItem(pIOPCItemMgt, hServerItem);

	// Remove the OPC group: 
//    RemoveGroup(pIOPCServer, hServerGroup);

	// release the interface references:
//	pIOPCItemMgt->Release();
	//pIOPCServer->Release();
//    m_Server->Release();

/////////////////////////////////////////////////////////////////////////
    // have to be done before using microsoft COM library:
	//CoInitialize(NULL);

	// Let's instantiante the IOPCServer interface and get a pointer of it:
	//IOPCServer* pServer = InstantiateServer(OPC_SERVER_NAME);

	// release IOPServer interface:
	//pServer->Release();

	//close the COM library:
	//CoUninitialize();
//////////////////////////////////////////////////////////////////////

    /*m_server = server;
    m_server.AddRef();
    //hr = m_server->Connect(_T("Softing.OPCToolboxDemo_ServerDA.1") );
    //hr = m_server->Connect(_T("OPC.Gateway.Server.DA") );
    //hr = m_server->Connect(_T("Woodward.ServLinkOpcDa.1") );
    hr = m_server->Connect(_T("Graybox.Simulator.1") );
    
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

    //set update rate in ms
    group->UpdateRate = 50;

    items = group->GetOPCItems();
    items.AddRef();
    items->DefaultIsActive = true;
    */
    return true;
}
///////////////////////////////////////////////////////////////////////////////
std::string OPC::GetAllOPCVariables( const std::string& modname )
{
    logFile.open("log.txt");

    //clear previous requests
    m_AllVarsAndVals.clear();
    
    //attach the browser to the server
    m_Server->QueryInterface(&browse);

    //Parse through all the available items
    Parse( "" );

    //define local variales
    OPCITEMRESULT *pResults = NULL;
    HRESULT *pErrors = NULL;
    int count = m_AllVarsAndVals.size();
    
    //create the required OPCITEMDEF array
    OPCITEMDEF *pItemArray = (OPCITEMDEF *) CoTaskMemAlloc ( count * sizeof (OPCITEMDEF) );
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
    OPCHANDLE *hServerItem = (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = pResults[i].hServer;
    }
    
	//get a pointer to the IOPCSyncIOInterface:
	OPCITEMSTATE* pValue = NULL;
	IOPCSyncIO* pIOPCSyncIO;
	m_AllItemMgt->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

	// read the item value from the device:
	//HRESULT* pErrors = NULL; //to store error code(s)
    //device - slower but more up to date
	HRESULT hr = pIOPCSyncIO->Read(OPC_DS_DEVICE, count, hServerItem, &pValue, &pErrors);
    //cache - faster but older data
	//HRESULT hr = pIOPCSyncIO->Read(OPC_DS_CACHE, m_AllVarsAndVals.size(), hServerItem, &pValue, &pErrors);
	
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

	//Release memeory allocated by the OPC server:
	CoTaskMemFree(pErrors);
	pErrors = NULL;

	CoTaskMemFree(pValue);
	pValue = NULL;

	// release the reference to the IOPCSyncIO interface:
	pIOPCSyncIO->Release();
    
    //append the flowsheet name
    ves::open::xml::CommandPtr varsAndValues( new ves::open::xml::Command() );
    varsAndValues->SetCommandName("AllOPCData");
    //compose return packet
    //loop over the variables and add as dvps
    for( int i = 0; i < m_AllVarsAndVals.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData( m_AllVarsAndVals[i].first, m_AllVarsAndVals[i].second );
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
void OPC::AddOPCVariable( const std::string& var )
{
    //std::vector<std::string>::const_iterator pos = 
    //    lower_bound( m_opcVariables.begin(), m_opcVariables.end(), var );
    bool found = binary_search( m_opcVariables.begin(), m_opcVariables.end(), var );

    //if( pos != m_opcVariables.end() || m_opcVariables.empty() )
    if( !found || m_opcVariables.empty() )
    {
        m_opcVariables.push_back( var );
        //UpdateOPCList();

        //define local variales
        OPCITEMRESULT *pResults = NULL;
        HRESULT *pErrors = NULL;
        //create the required OPCITEMDEF array
        //OPCITEMDEF *pItemArray = (OPCITEMDEF *) CoTaskMemAlloc ( sizeof (OPCITEMDEF) );
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
void OPC::UpdateOPCList( )
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

    /*itemIDs = new CComSafeArray<BSTR>( m_opcVariables.size() + 1 );
    clientID = new CComSafeArray<long>( m_opcVariables.size() + 1 );
    serverID = new CComSafeArray<long>();
    serverID->Create();
    CComSafeArray<long> * errors;
    errors = new CComSafeArray<long>();
    errors->Create();

    for( int i = 1; i <= m_opcVariables.size(); i++)
    {
        std::string modnameOPC = m_opcVariables[i-1];
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
    group->IsActive = true;*/


    ///
    ///put new items to be monitored in list
    ///


}

///////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > > OPC::ReadVars()
//std::map< std::string, std::pair< std::string, VARTYPE > > DynSim::ReadVars()
{
    /*UpdateOPCList();
    
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

    //std::vector< std::pair< std::string, std::string > > m_MonitorVarsAndVals;
    //nameValVar.clear();
    m_MonitorVarsAndVals.clear();

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
        m_MonitorVarsAndVals.push_back( nameNval );
    }
    */

    ///
    ///Read the values being monitored and put them back in the vector
    ///

    //get a pointer to the IOPCSyncIOInterface:
	OPCITEMSTATE* pValue = NULL;
	HRESULT* pErrors = NULL;
	IOPCSyncIO* pIOPCSyncIO;
	m_MonitorItemMgt->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

    
    //create an array of the server's id for the items
    int count = m_MonitorServerItem.size();
    OPCHANDLE *hServerItem = (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = m_MonitorServerItem[i];
    }

	// read the item value from the device:
	HRESULT hr = pIOPCSyncIO->Read(OPC_DS_DEVICE, m_opcVariables.size(), hServerItem, &pValue, &pErrors);
    
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

	//Release memeory allocated by the OPC server:
	CoTaskMemFree(pErrors);
	pErrors = NULL;

	CoTaskMemFree(pValue);
	pValue = NULL;

	// release the reference to the IOPCSyncIO interface:
	pIOPCSyncIO->Release();
    return m_MonitorVarsAndVals;
}

///////////////////////////////////////////////////////////////////////////////
/*std::string OPC::GetOPCValue( const std::string& modname )
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
    for( nameValueIter = m_MonitorVarsAndVals.begin();
        nameValueIter != m_MonitorVarsAndVals.end();
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
    ves::open::xml::CommandPtr varAndValues( new ves::open::xml::Command() );
    varAndValues->SetCommandName("OPCData");

    //loop over the variables and add as dvps
    for( int i = 0; i < m_MonitorVarsAndVals.size(); i++ )
    {
         ves::open::xml::DataValuePairPtr
             entry( new ves::open::xml::DataValuePair() );
        entry->SetData( m_MonitorVarsAndVals[i].first, m_MonitorVarsAndVals[i].second );
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
void OPC::SetOPCValues( std::vector< std::pair < std::string, std::string > > varAndValues )
{
    /*long count = varAndValues.size();

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
    groups->Remove( _T( "SetGroup" ) );*/

    ///
    ///Set the values of the requested variables
    ///
    OPCITEMRESULT *pResults = NULL;
	HRESULT* pErrors = NULL;
    int count = varAndValues.size();
    
    //create the required OPCITEMDEF array
    OPCITEMDEF *pItemArray = (OPCITEMDEF *) CoTaskMemAlloc ( count * sizeof (OPCITEMDEF) );
    for( int i = 0; i < count; i++ )
    {   
        std::string temp = varAndValues[i].first.substr( varAndValues[i].first.find_first_of(".") + 1, varAndValues[i].first.size() - ( varAndValues[i].first.find_first_of(".")+1 ) );
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

	CoTaskMemFree(pErrors);
	pErrors = NULL;
            
    //create an array of the server's id for the items
    OPCHANDLE *hServerItem = (OPCHANDLE *) CoTaskMemAlloc ( count * sizeof (OPCHANDLE) );
	VARIANT* pValue = (VARIANT *) CoTaskMemAlloc ( count * sizeof (VARIANT) );
	//OPCITEMVQT *pValue = (OPCITEMVQT *) CoTaskMemAlloc ( count * sizeof (OPCITEMVQT) );
    //loop over and create a list of server ids and put the values in a list
    for( int i = 0; i < count; i++ )
    {
        hServerItem[i] = pResults[i].hServer;

        pValue[i].vt = VT_BSTR;
        pValue[i].bstrVal = _bstr_t( varAndValues[i].second.c_str() );

        //pValue[i].vDataValue.vt = VT_BSTR;
        //pValue[i].vDataValue.bstrVal = _bstr_t( varAndValues[i].second.c_str() );
        //pValue[i].bQualitySpecified = false;
        //pValue[i].bTimeStampSpecified = false;
    }

	//get a pointer to the IOPCSyncIOInterface:
	IOPCSyncIO* pIOPCSyncIO;
	//IOPCSyncIO2* pIOPCSyncIO;
	m_SetItemMgt->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

    //DWORD cancelID;
    
	//OPCITEMSTATE* pValue = NULL;
	// write the item value(s)
	HRESULT hr = pIOPCSyncIO->Write( count, hServerItem, pValue, &pErrors);
	//HRESULT hr = pIOPCSyncIO->WriteVQT( count, hServerItem, pValue, &pErrors);
    
	CoTaskMemFree(pErrors);
	pErrors = NULL;

    //Remove the items
    m_SetItemMgt->RemoveItems( count, hServerItem, &pErrors );

	CoTaskMemFree(pResults);
	pResults = NULL;
    
	CoTaskMemFree(pErrors);
	pErrors = NULL;

    return;
}

///////////////////////////////////////////////////////////////////////////////
bool OPC::IsOPCVarsEmpty()
{
    return m_opcVariables.empty();
}
/*
void OPC::ParseBranch( _bstr_t name, std::string prefix )
{
    //browser->MoveTo( name );
    browser->MoveDown( name );
    //hierachical
    //browser->ShowLeafs(false);
    //flat
    browser->ShowLeafs(true);
    long leafCount = browser->GetCount();
    if( leafCount > 0 )
    {
        //std::vector< std::string > tempVars;
                    logFile<<"leafcount of "<<prefix<<"."<<name<<":"<< leafCount << std::endl;
        for( long i = 1; i <= leafCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            std::string temp = prefix + std::string( name ) + "." + std::string( itemName );

            //if( temp.find( modname ) != std::string::npos )
            {
                //for some reason the values from the "time slot" from the softing ex
                //do not work
                //if( temp.find("time slot") == std::string::npos )
                
                    logFile<<i<<":"<<temp << std::endl;
                {
                    tempVars.push_back( temp );
                }
                //bItemIDs.push_back( browser->GetItemID( itemName ) );
            }
        }
    }

    browser->ShowBranches();
    long branchCount = browser->GetCount();
                    logFile<<"branchcount of "<<prefix<<"."<<name<<":"<< branchCount << std::endl;
    if( branchCount > 0 )
    {
        prefix = prefix + std::string( name ) + ".";
        //std::vector< std::string > tempVars;
        for( long i = 1; i <= branchCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            //browser->MoveTo( itemName );
            //browser->MoveDown( itemName );
            //std::string temp = itemName;
            ParseBranch( itemName, prefix );
            browser->MoveUp();
            browser->ShowBranches();
        }
    }

}*/

////////////////////////////////////////////////////////////////////
// Instantiate the IOPCServer interface of the OPCServer
// having the name ServerName. Return a pointer to this interface
//
IOPCServer* OPC::InstantiateServer(wchar_t ServerName[])
{
	CLSID CLSID_OPCServer;
	HRESULT hr;

	// get the CLSID from the OPC Server Name:
	hr = CLSIDFromString(ServerName, &CLSID_OPCServer);
	_ASSERT(!FAILED(hr));


	//queue of the class instances to create
	LONG cmq = 1; // nbr of class instance to create.
	MULTI_QI queue[1] =
		//{{&IID_IOPCServer,
        {{&__uuidof(IOPCServer),
		NULL,
		0}};

	//Server info:
	//COSERVERINFO CoServerInfo =
    //{
	//	/*dwReserved1*/ 0,
	//	/*pwszName*/ REMOTE_SERVER_NAME,
	//	/*COAUTHINFO*/  NULL,
	//	/*dwReserved2*/ 0
    //}; 

	// create an instance of the IOPCServer
	hr = CoCreateInstanceEx(CLSID_OPCServer, NULL, CLSCTX_SERVER,
		/*&CoServerInfo*/NULL, cmq, queue);
	_ASSERT(!hr);

	// return a pointer to the IOPCServer interface:
	return(IOPCServer*) queue[0].pItf;
}

/////////////////////////////////////////////////////////////////////
// Add group "Group1" to the Server whose IOPCServer interface
// is pointed by pIOPCServer. 
// Returns a pointer to the IOPCItemMgt interface of the added group
// and a server opc handle to the added group.
//
void OPC::AddGroup(IOPCServer* pIOPCServer, IOPCItemMgt* &pIOPCItemMgt, 
				 OPCHANDLE& hServerGroup, char* name)
{
	DWORD dwUpdateRate = 0;
	OPCHANDLE hClientGroup = 0;

	// Add an OPC group and get a pointer to the IUnknown I/F:
    HRESULT hr = pIOPCServer->AddGroup(/*szName*/ CA2W(name),
		/*bActive*/ FALSE,
		/*dwRequestedUpdateRate*/ dwUpdateRate,
		/*hClientGroup*/ hClientGroup,
		/*pTimeBias*/ 0,
		/*pPercentDeadband*/ 0,
		/*dwLCID*/0,
		/*phServerGroup*/&hServerGroup,
		&dwUpdateRate,
		/*riid*/ &GUID(__uuidof(IOPCItemMgt)),//IID_IOPCItemMgt,
		/*ppUnk*/ (IUnknown**) &pIOPCItemMgt);
	_ASSERT(!FAILED(hr));
}



//////////////////////////////////////////////////////////////////
// Add the Item ITEM_ID to the group whose IOPCItemMgt interface
// is pointed by pIOPCItemMgt pointer. Return a server opc handle
// to the item.
 
/*void OPC::AddTheItem(IOPCItemMgt* pIOPCItemMgt, OPCHANDLE& hServerItem)
{
	HRESULT hr;

	// Array of items to add:
	OPCITEMDEF ItemArray[1] =
	{{
	L"",
	ITEM_ID,
	FALSE,
	1,
	0,
	NULL,
	VT,
	0
	}};

	//Add Result:
	OPCITEMRESULT* pAddResult=NULL;
	HRESULT* pErrors = NULL;

	// Add an Item to the previous Group:
	hr = pIOPCItemMgt->AddItems(1, ItemArray, &pAddResult, &pErrors);
	_ASSERT(!hr);

	// Server handle for the added item:
	hServerItem = pAddResult[0].hServer;

	// release memory allocated by the server:
	CoTaskMemFree(pAddResult->pBlob);

	CoTaskMemFree(pAddResult);
	pAddResult = NULL;
    
	CoTaskMemFree(pErrors);
	pErrors = NULL;
}*/

///////////////////////////////////////////////////////////////////////////////
// Read from device the value of the item having the "hServerItem" server 
// handle and belonging to the group whose one interface is pointed by
// pGroupIUnknown. The value is put in varValue. 
//
void OPC::ReadItem(IUnknown* pGroupIUnknown, OPCHANDLE hServerItem, VARIANT& varValue)
{
    /*
	// value of the item:
	OPCITEMSTATE* pValue = NULL;

	//get a pointer to the IOPCSyncIOInterface:
	IOPCSyncIO* pIOPCSyncIO;
	pGroupIUnknown->QueryInterface(__uuidof(pIOPCSyncIO), (void**) &pIOPCSyncIO);

	// read the item value from the device:
	HRESULT* pErrors = NULL; //to store error code(s)
	HRESULT hr = pIOPCSyncIO->Read(OPC_DS_DEVICE, m_AllVarsAndVals.size(), &hServerItem, &pValue, &pErrors);
	_ASSERT(!hr);
	_ASSERT(pValue!=NULL);

	//varValue = pValue[0].vDataValue;

    for( int i = 0; i < m_AllVarsAndVals.size(); i++ )
    {
        m_AllVarsAndVals[i].second = _bstr_t(pValue[i].vDataValue);
    }

	//Release memeory allocated by the OPC server:
	CoTaskMemFree(pErrors);
	pErrors = NULL;

	CoTaskMemFree(pValue);
	pValue = NULL;

	// release the reference to the IOPCSyncIO interface:
	pIOPCSyncIO->Release();
    */
}


///////////////////////////////////////////////////////////////////////////
// Remove the item whose server handle is hServerItem from the group
// whose IOPCItemMgt interface is pointed by pIOPCItemMgt
//
void OPC::RemoveItem(IOPCItemMgt* pIOPCItemMgt, OPCHANDLE hServerItem)
{
	// server handle of items to remove:
	OPCHANDLE hServerArray[1];
	hServerArray[0] = hServerItem;
	
	//Remove the item:
	HRESULT* pErrors; // to store error code(s)
	HRESULT hr = pIOPCItemMgt->RemoveItems(1, hServerArray, &pErrors);
	_ASSERT(!hr);

	//release memory allocated by the server:
	CoTaskMemFree(pErrors);
	pErrors = NULL;
}


////////////////////////////////////////////////////////////////////////
// Remove the Group whose server handle is hServerGroup from the server
// whose IOPCServer interface is pointed by pIOPCServer
//
void OPC::RemoveGroup (IOPCServer* pIOPCServer, OPCHANDLE hServerGroup)
{
	// Remove the group:
	HRESULT hr = pIOPCServer->RemoveGroup(hServerGroup, FALSE);
	_ASSERT(!hr);
}

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
       HRESULT hr = browse->Browse( CA2W(name.c_str()),  
			&pszContinuationPoint,  
			0,  
			OPC_BROWSE_FILTER_ALL//OPC_BROWSE_FILTER_ALL 
			, CA2W("") // [in, string] LPWSTR szElementNameFilter 
			, CA2W("") //[in, string] LPWSTR szVendorFilter 
			, TRUE //[in] BOOL bReturnAllProperties 
			, TRUE //[in] BOOL bReturnPropertyValues 
			, 1 //[in] DWORD dwPropertyCount 
			, &pdwPropertyIDs //[in, size_is(dwPropertyCount)] DWORD * pdwPropertyIDs 
			, &moreElements //[out] BOOL * pbMoreElements 
			, &count  //[out] DWORD * pdwCount 
			, &pBrowseElements //[out, size_is(,*pdwCount)] OPCBROWSEELEMENT ** ppBrowseElements  
			); 
 
	if( SUCCEEDED(hr) ) { 
    std::ofstream output ( (name+".log").c_str() );
        output<<count<<std::endl;

        for( DWORD i=0; i < count; ++i )
        { 
            output << "Name: " << CW2A(pBrowseElements[i].szName) << std::endl;
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
                    //VARIANT value = pBrowseElements[i].ItemProperties.pItemProperties[1].vValue;
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
                //else
                //{
                //    Parse(temp);
                //}
            }

		} 
        output.close();
    }
        
  /*  // Don't bother if memory allocation failed:
                if (pItemArray != NULL)
                        {
                        // Fill the item definition array:
                        for (dwIndex = 0; dwIndex < dwCount; dwIndex++)
                                {
                                // Get pointer to item in input array:
                                pItem = (CKItem *) cItemList [dwIndex];
                                ASSERT (pItem != NULL);

                                // COM requires that all string be in wide character format.  The
                                // access path and Item ID properties are strings, so we may have
                                // to convert their format.

                                // First get the length of access path string:
                                dwLen = lstrlen (pItem->GetAccessPath ());

                                if (dwLen)
                                        {
                                        // Allocate memory for string:
                                        pItemArray [dwIndex].szAccessPath = (WCHAR *) CoTaskMemAlloc ((dwLen + 1) * sizeof (WCHAR));

#ifdef _UNICODE
                                        // If Unicode build, string will already be in wide character
                                        // format, so copy into allocated memory as is:
                                        lstrcpyn (pItemArray [dwIndex].szAccessPath,  pItem->GetAccessPath (), dwLen + 1);
#else
                                        // If ANSI build, then string format needs to be converted.  Place
                                        // result of conversion into allocated memory:
                                        MultiByteToWideChar (CP_ACP, 0, pItem->GetAccessPath (), -1, pItemArray [dwIndex].szAccessPath, dwLen + 1);
#endif
                                        }
                                else
                                        {
                                        // Access path string length is zero, so set output to NULL:
                                        pItemArray [dwIndex].szAccessPath = NULL;
                                        }

                                // Multibyte to wide character conversion for Item ID:
                                dwLen = lstrlen (pItem->GetItemID ()); // This can't be zero, so no test as above
                                pItemArray [dwIndex].szItemID = (WCHAR *) CoTaskMemAlloc ((dwLen + 1) * sizeof (WCHAR));

#ifdef _UNICODE
                                lstrcpyn (pItemArray [dwIndex].szItemID, pItem->GetItemID (), dwLen + 1);
#else
                                MultiByteToWideChar (CP_ACP, 0, pItem->GetItemID (), -1, pItemArray [dwIndex].szItemID, dwLen + 1);
#endif

                                // Set remaining structure members:
                                // (If requested data type is NULL, the OPC Server should return
                                // the default type.  The returned canonical data type may not be
                                // the same as the requested data type.)
                                pItemArray [dwIndex].bActive = pItem->IsActive ();      // active state
                                pItemArray [dwIndex].hClient = (OPCHANDLE) pItem;       // our handle to item
                                pItemArray [dwIndex].dwBlobSize = 0;                            // no blob support
                                pItemArray [dwIndex].pBlob = NULL;
                                pItemArray [dwIndex].vtRequestedDataType = pItem->GetDataType (); // Requested data type
                                }
*/

//typedef struct {
//[string] LPWSTR szAccessPath;
//[string] LPWSTR szItemID;
//BOOL bActive ;
//OPCHANDLE hClient;
//DWORD dwBlobSize;
//[size_is(dwBlobSize)] BYTE * pBlob;
//VARTYPE vtRequestedDataType;
//WORD wReserved;
//} OPCITEMDEF;

}