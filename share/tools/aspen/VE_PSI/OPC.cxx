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
#define DSS_VERSION "C:/SIMSCI/DSS44/GUI/"

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
    //Connect to the OPC Server
    HRESULT hr;
    IOPCAutoServerPtr server( __uuidof(OPCServer) );
    m_server = server;
    m_server.AddRef();
    hr = m_server->Connect(_T("Softing.OPCToolboxDemo_ServerDA.1") );
    //hr = m_server->Connect(_T("OPC.Gateway.Server.DA") );
    
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
std::string OPC::GetAllOPCVariables( const std::string& modname )
{
    //Get a list of the available OPC variables for a given unit op
    browser = m_server->CreateBrowser();
    browser.AddRef();
    
    //Get the items from the first level
    browser->ShowLeafs();
    long leafCount = browser->GetCount();
    if( leafCount > 0 )
    {
        //std::vector< std::string > tempVars;
        for( long i = 1; i <= leafCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            std::string temp = itemName;

            //if( temp.find( modname ) != std::string::npos )
            {
                //for some reason the values from the "time slot" from the softing ex
                //do not work
                if( temp.find("time slot") == std::string::npos )
                {
                    tempVars.push_back( temp );
                }
                //bItemIDs.push_back( browser->GetItemID( itemName ) );
            }
        }
    }

    //recurse through the other branches
    browser->ShowBranches();
    long branchCount = browser->GetCount();
    if( branchCount > 0 )
    {
        //std::vector< std::string > tempVars;
        for( long i = 1; i <= branchCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            //browser->MoveTo( itemName );
            //browser->MoveDown( itemName );
            //std::string temp = itemName;
            ParseBranch( itemName, "" );
            browser->MoveUp();
            browser->ShowBranches();
        }
    }

    //this call is necessary to correctly obtain the itemIDs using the path
    //"The server converts the name to an ItemID based on the current
    //"position" of the browser. It will not correctly translate a name if
    //MoveUp, MoveDown, etc. has been called since the name was obtained."
    browser->MoveToRoot();

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
        BSTR tempId = browser->GetItemID( tempVars[i].c_str() );
            itemIDs->SetAt( i + 1, tempId );//bItemIDs[i] );
            //itemIDs->SetAt( i + 1, browser->GetItemID( tempVars[i].c_str() ) );//bItemIDs[i] );
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
        VARTYPE myVT = values->GetAt(i).vt;
        //the current implementation doesn't handle arrays properly
        if( myVT <= VT_ARRAY &&
            myVT != VT_EMPTY )
        {
            values->GetAt(i).ChangeType(VT_BSTR);
            std::pair< std::string, std::string > varAndVal;
            std::string temp = _bstr_t( itemIDs->GetAt(i) );

            //remove everything but the variable ie remove the unit name
            //everything before the "."
            //varAndVal.first = temp.substr( temp.find(".") + 1, temp.size() - temp.find(".") + 1 );

            varAndVal.first = temp;
            varAndVal.second = _bstr_t( values->GetAt(i).bstrVal );
            varsAndVals.push_back( varAndVal );
        }
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
void OPC::AddOPCVariable( const std::string& var )
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
std::vector< std::pair< std::string, std::string > > OPC::ReadVars()
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
std::string OPC::GetOPCValues( )
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
void OPC::SetOPCValues( std::vector< std::pair < std::string, std::string > > varAndValues )
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
bool OPC::IsOPCVarsEmpty()
{
    return m_opcVariables.empty();
}

void OPC::ParseBranch( _bstr_t name, std::string prefix )
{
    //browser->MoveTo( name );
    browser->MoveDown( name );
    browser->ShowLeafs();
    long leafCount = browser->GetCount();
    if( leafCount > 0 )
    {
        //std::vector< std::string > tempVars;
        for( long i = 1; i <= leafCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            std::string temp = prefix + std::string( name ) + "." + std::string( itemName );

            //if( temp.find( modname ) != std::string::npos )
            {
                //for some reason the values from the "time slot" from the softing ex
                //do not work
                if( temp.find("time slot") == std::string::npos )
                {
                    tempVars.push_back( temp );
                }
                //bItemIDs.push_back( browser->GetItemID( itemName ) );
            }
        }
    }

    browser->ShowBranches();
    long branchCount = browser->GetCount();
    if( branchCount > 0 )
    {
        //std::vector< std::string > tempVars;
        for( long i = 1; i <= branchCount; i++ )
        {
            _bstr_t itemName = browser->Item( i );
            //browser->MoveTo( itemName );
            //browser->MoveDown( itemName );
            //std::string temp = itemName;
            ParseBranch( itemName, std::string( name ) + "." );
            browser->MoveUp();
            browser->ShowBranches();
        }
    }

}