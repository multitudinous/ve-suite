#pragma warning (disable: 4786)

#include "AspenDynamicsInterface.h"
#include <fstream>
#include <sstream>
#include <comutil.h>
#include <string>

#ifdef YANGDEBUG
extern FILE* test;
#endif

namespace AspenDynamicsInterface
{

///////////////////////////////////////////////////////////////////////////////
//Constructor
AspenDynamicsInterface::AspenDynamicsInterface()
{
	simOpened = false;
	ADApplication = NULL;
	ADDocument = NULL;
}

///////////////////////////////////////////////////////////////////////////////
//Deconstructor
AspenDynamicsInterface::~AspenDynamicsInterface()
{
}

///////////////////////////////////////////////////////////////////////////////
//Open an Aspen Document
void AspenDynamicsInterface::Open(CString filename)
{
    if (simOpened)
          Close();
    
    ADApplication = AspenDynamicsLibrary::IAspenModelerPtr( "AD Application" );
    
    VARIANT strVar;
	::VariantInit(&strVar);
    strVar.vt = VT_BSTR;
    BSTR bstr = filename.AllocSysString();
    strVar.bstrVal = bstr;

    ADDocument = ADApplication->OpenDocument(strVar);
    std::ofstream output("type.txt");

    VARIANT strName;
    ::VariantInit(&strName);
    strName.vt = VT_BSTR;
    CString temp = "GASIFIER";
    BSTR bstr2 = temp.AllocSysString();
    strName.bstrVal = bstr2;

    output.close();

    VARIANT cv;
	::VariantInit(&cv);
    cv.vt=VT_BOOL;                      //Type is BOOL
    cv.boolVal=VARIANT_TRUE;      //value is True (VARIANT_TRUE)

    ADApplication->PutVisible(cv);
    ::VariantClear(&strVar);
    ::VariantClear(&cv);
    //needs error handling

	simOpened = true;
    BSTR bstrDesc;
    
    try
    {
        //Initialize COM
        CoInitialize(NULL);
    }
    catch(_com_error &e)
    {
        bstrDesc = e.Description();
    }   
}

///////////////////////////////////////////////////////////////////////////////
std::vector< std::vector< std::string > > AspenDynamicsInterface::GetVariableList( CString itemPath, bool block )
{
    //get the count of the blocks
    std::stringstream tokenizer;
    tokenizer << itemPath;
    std::string temp;
    std::vector< std::string > levels;
    while( std::getline( tokenizer, temp, '.' ) )
    {
        levels.push_back( temp );
    }

    std::vector< std::vector < std::string > > blocks;

    // get document/simulation object      
    COleVariant vDisp(ADApplication->GetSimulation());

    // get the flowsheet object
    vDisp = ADDocument->GetFlowsheet();
    IDispatch* pDispFlow = vDisp.pdispVal;

    OLECHAR* szName;
    DISPID dispid;

    
    //loop over subblocks
    DISPPARAMS params = {NULL, NULL, 0, 0};
    VARIANT result;
    VARIANT index;
    IDispatch* pDispBlocksColl;

    int levelCount = levels.size();
    for( int i = 0; i < levelCount; i++ )
    {
        CString itemName( levels[i].c_str() );
        if( block )
        {
            // Get the dispatch ID for the Blocks property
            szName = OLESTR("Blocks");
        }
        else
        {
            if( i == levelCount - 1 )
            {
                // Get the dispatch ID for the Streams property
                szName = OLESTR("Streams");
            }
            else
            {
                szName = OLESTR("Blocks");
            }
        }
        pDispFlow->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);

        // Call Blocks to get collection of blocks
        params.cArgs = 0;
        params.rgvarg = NULL;
        ::VariantInit(&result);
        pDispFlow->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        pDispBlocksColl = result.pdispVal;

        szName = OLESTR("Item");
        pDispBlocksColl->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid );
        ::VariantInit(&index);
        index.vt = VT_BSTR;

        // get next block in collection
        index.bstrVal = itemName.AllocSysString();
        params.cArgs = 1;
        params.rgvarg = &index;
        ::VariantInit(&result);
        pDispBlocksColl->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        pDispFlow = result.pdispVal;  // get block dispatch
    }

    //get All Variables
    szName = OLESTR("FindMatchingVariables");
    pDispFlow->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
    params.cArgs = 0;
    params.rgvarg = NULL;
    ::VariantInit(&result);
    pDispFlow->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    IDispatch* pDispVariables = result.pdispVal;

    szName = OLESTR("Count");
    pDispVariables->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
    ::VariantInit(&result);
    pDispVariables->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    int nVars = result.lVal;

    DISPID dispidItem;
    szName = OLESTR("Item");
    pDispVariables->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispidItem);
    ::VariantInit(&index);
    index.vt = VT_I4;

    std::vector< std::string > tempVarInfo;
    for (int j = 0; j < nVars; j++)
    {
        // get variable list
        index.lVal = j + 1;   // collection index is "one based"
        params.cArgs = 1;
        params.rgvarg = &index;
        ::VariantInit(&result);
        pDispVariables->Invoke(dispidItem, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        IDispatch* pDispVariable = result.pdispVal;  // get block dispatch

        // get variable name
        szName = OLESTR("Name");
        pDispVariable->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
        params.cArgs = 0;
        params.rgvarg = NULL;
        ::VariantInit(&result);
        pDispVariable->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        //listbox->InsertString(i, OLE2T(result.bstrVal));
        CString temp = result.bstrVal;
        char * ctemp = temp.GetBuffer();
        if( temp.IsEmpty() )
        {
            tempVarInfo.push_back( "N/A" );
        }
        else
        {
            tempVarInfo.push_back( ctemp );
        }

        // get variable description
        szName = OLESTR("Description");
        pDispVariable->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
        params.cArgs = 0;
        params.rgvarg = NULL;
        ::VariantInit(&result);
        pDispVariable->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        //listbox->InsertString(i, OLE2T(result.bstrVal));
        temp = result.bstrVal;
        ctemp = temp.GetBuffer();
        if( temp.IsEmpty() )
        {
            tempVarInfo.push_back( "N/A" );
        }
        else
        {
            tempVarInfo.push_back( ctemp );
        }

        // get variable value
        szName = OLESTR("Value");
        pDispVariable->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
        params.cArgs = 0;
        params.rgvarg = NULL;
        ::VariantInit(&result);
        pDispVariable->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        //listbox->InsertString(i, OLE2T(result.bstrVal));
        if( result.vt == VT_BSTR )
        {
            temp = result.bstrVal;
        }
        else 
        {
            temp.Format(_T("%f"), result.dblVal);
        }
        ctemp = temp.GetBuffer();
        if( temp.IsEmpty() )
        {
            tempVarInfo.push_back( "N/A" );
        }
        else
        {
            tempVarInfo.push_back( ctemp );
        }

        // get variable units
        szName = OLESTR("Units");
        pDispVariable->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
        params.cArgs = 0;
        params.rgvarg = NULL;
        ::VariantInit(&result);
        pDispVariable->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        //listbox->InsertString(i, OLE2T(result.bstrVal));
        temp = result.bstrVal;
        ctemp = temp.GetBuffer();
        if( temp.IsEmpty() || ctemp[0] == '?' )
        {
            tempVarInfo.push_back( "N/A" );
        }
        else
        {
            tempVarInfo.push_back( ctemp );
        }

            //tempVarInfo.push_back( "N/A" );
            //tempVarInfo.push_back( "N/A" );
            //tempVarInfo.push_back( "N/A" );
            //tempVarInfo.push_back( "N/A" );

        //blocks.push_back( ctemp );
        blocks.push_back( tempVarInfo );
        tempVarInfo.clear();
    }
    return blocks;
}

///////////////////////////////////////////////////////////////////////////////
void AspenDynamicsInterface::SetVariableValue( CString itemName,
    CString variableName, CString value  )
{
    // get document/simulation object      
    COleVariant vDisp(ADApplication->GetSimulation());

    // get the flowsheet object
    vDisp = ADDocument->GetFlowsheet();
    IDispatch* pDispFlow = vDisp.pdispVal;

    // Get the dispatch ID for the Blocks property
    DISPID dispid;
    OLECHAR* szName = OLESTR("Blocks");
    pDispFlow->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);

    // Call Blocks to get collection of blocks
    DISPPARAMS params = {NULL, NULL, 0, 0};
    VARIANT result;
    ::VariantInit(&result);
    pDispFlow->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    IDispatch* pDispBlocksColl = result.pdispVal;

    DISPID dispidItem;
    szName = OLESTR("Item");
    pDispBlocksColl->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispidItem);
    VARIANT index;
    ::VariantInit(&index);
    index.vt = VT_BSTR;

    // get next block in collection
    //index.lVal = i + 1;   // collection index is "one based"
    index.bstrVal = itemName.AllocSysString();
    params.cArgs = 1;
    params.rgvarg = &index;
    ::VariantInit(&result);
    pDispBlocksColl->Invoke(dispidItem, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    IDispatch* pDispBlock = result.pdispVal;  // get block dispatch

    // get block name
    szName = OLESTR("FindMatchingVariables");
    pDispBlock->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
    params.cArgs = 0;
    params.rgvarg = NULL;
    ::VariantInit(&result);
    pDispBlock->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    //listbox->InsertString(i, OLE2T(result.bstrVal));
    IDispatch* pDispVariables = result.pdispVal;

    szName = OLESTR("Item");
    pDispVariables->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispidItem);
    ::VariantInit(&index);
    index.vt = VT_BSTR;
    //index.vt = VT_I4;
    // get variable list
    index.bstrVal = variableName.AllocSysString();
    //index.lVal = 1;

    params.cArgs = 1;
    params.rgvarg = &index;
    ::VariantInit(&result);
    pDispVariables->Invoke(dispidItem, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
    IDispatch* pDispVariable = result.pdispVal;  // get block dispatch

    // get variable value
    szName = OLESTR("Value");
    pDispVariable->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
    index.vt = VT_BSTR;
    index.bstrVal = value.AllocSysString();
    params.cArgs = 1;
    params.rgvarg = &index;
    ::VariantInit(&result);
    pDispVariable->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYPUT, &params, &result, NULL, NULL);
}

///////////////////////////////////////////////////////////////////////////////
//Close the file, clear up 
void AspenDynamicsInterface::Close()
{
	if (simOpened)
	{
        VARIANTARG reserved;
        ::VariantInit(&reserved);
        reserved.vt=VT_BOOL;
        //reserved.boolVal=VARIANT_TRUE;
        reserved.boolVal=VARIANT_FALSE;

		ADApplication->CloseDocument(reserved);
        //delete ihRoot;
        //delete hAPsim;
		//ihRoot = NULL;
        //hAPsim = NULL;

		simOpened = false;
        ::VariantClear(&reserved);
	}
}

///////////////////////////////////////////////////////////////////////////////
//Close the file, clear up
void AspenDynamicsInterface::Quit() 
{
    ADApplication->Quit();
}

///////////////////////////////////////////////////////////////////////////////
//Save the document back;
void AspenDynamicsInterface::Save()
{
	if (simOpened)
    {
	    ADApplication->SaveDocument();
    }
}

///////////////////////////////////////////////////////////////////////////////
//save this as another document
void AspenDynamicsInterface::SaveAs(CString filename)
{	
	if (simOpened)
    {	
	    VARIANTARG overwrite;
	    ::VariantInit(&overwrite);
        overwrite.vt=VT_BOOL;
        overwrite.boolVal=VARIANT_TRUE;

        VARIANT strVar;
	    ::VariantInit(&strVar);
        strVar.vt = VT_BSTR;
        BSTR bstr = filename.AllocSysString();
        strVar.bstrVal = bstr;

	    ADApplication->SaveDocumentAs(strVar, overwrite);
	    ::VariantClear(&overwrite);	
	    ::VariantClear(&strVar);
    }
}

///////////////////////////////////////////////////////////////////////////////
//hide/show dynamics application
void AspenDynamicsInterface::SetVisibility(bool status)
{
	if (simOpened)
    {
        VARIANT cv;
	    ::VariantInit(&cv);
        cv.vt=VT_BOOL;
        if(status == true )
        {
            cv.boolVal=VARIANT_TRUE;
        }
        else if (status == false)
        {
            cv.boolVal=VARIANT_FALSE;
        }
        
        ADApplication->PutVisible(cv);
	    ::VariantClear(&cv);
    }
}

///////////////////////////////////////////////////////////////////////////////
//run the simulation
void AspenDynamicsInterface::RunSolver( )
{
	if (simOpened)
    {
	    VARIANTARG flag;
	    ::VariantInit(&flag);
        flag.vt=VT_BOOL;
        flag.boolVal=VARIANT_TRUE;

        ADDocument->Run(flag);
	    ::VariantClear(&flag);	
    }
}

///////////////////////////////////////////////////////////////////////////////
//return the current simulation to the default values 
void AspenDynamicsInterface::ResetSimulation( )
{
    ADDocument->Reset( );
}

} //namespace ASPENDYNAMICSINTERFACE