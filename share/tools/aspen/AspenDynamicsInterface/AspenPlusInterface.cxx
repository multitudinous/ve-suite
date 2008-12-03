#pragma warning (disable: 4786)

#include "AspenPlusInterface.h"
#include <fstream>
#include <comutil.h>
#include <string>

#ifdef YANGDEBUG
extern FILE* test;
#endif
namespace AspenPlusInterface
{

///////////////////////////////////////////////////////////////////////////////
//Constructor
	AspenPlusInterface::AspenPlusInterface()
	{
		simOpened = false;
		ADApplication = NULL;
		ADDocument = NULL;
	}

///////////////////////////////////////////////////////////////////////////////
//Deconstructor
	AspenPlusInterface::~AspenPlusInterface()
	{
	}

	//IHNode AspenPlusInterface::getRoot()
	//{
	//	return *ihRoot;
	//}
	//File operating functions

///////////////////////////////////////////////////////////////////////////////
//Open an Aspen Document
    void AspenPlusInterface::Open(CString filename)
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
        /*_variant_t temp = ADDocument->GetFlowsheet();
        VARIANT temp2 = temp.GetVARIANT();
        VARIANT temp3;
        ::VariantInit(&temp3);
        temp3 = temp.GetVARIANT();
        output << temp.vt<<" "<<temp2.vt<<" "<<temp3.vt<<std::endl; 
        
        _bstr_t bstr_t(temp.bstrVal);
        std::string str(bstr_t);

        output<<str<<std::endl;
        output<<VARENUM::VT_BSTR<<std::endl;
        output<<VARENUM::<<std::endl;
        output<<str<<std::endl;
        output<<str<<std::endl;
        output<<str<<std::endl;
        */
        //needs error handling

        //ADProperties = ADDocument->Properties();
        
        //output<<ADProperties->GetComponentListNames().vt;
        
        //ADResults = ADDocument->Results;
        //output<<ADResults->GetResult(2).vt;

        VARIANT strName;
	    ::VariantInit(&strName);
        strName.vt = VT_BSTR;
        CString temp = "GASIFIER";
        BSTR bstr2 = temp.AllocSysString();
        strName.bstrVal = bstr2;

        //ADProperties = ADDocument->Properties();
        //AspenDynamicsLibrary::IAspenModelerComponentListPtr ADCompList = ADProperties->ComponentList(strName);

        //output<<ADCompList->GetComponents().vt<<std::endl;
        //output<<ADCompList->GetName().vt<<std::endl;
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
            //Create an instance of the VB lib
            //AspenDynamicsQueryLib.
            //    CreateInstance(__uuidof(ADQueryVB::AspenDynamicsQueryLib));
            //Connect the VB lib to the currently opened aspen simulation
            //AspenDynamicsQueryLib->ConnectToDynamics( &bstr );
        }
        catch(_com_error &e)
        {
            bstrDesc = e.Description();
        }
/*		

        //the default constructor sets auto release of memory so there is no 
        //need to delete this mmeory later. ReleaseDispatch must be called to
        //make this happen.
		hAPsim = new IHapp(); 
		BOOL bSuccess = hAPsim->CreateDispatch(_T("apwn.document"));
		if (!bSuccess) 
        {
            //clean up
            hAPsim->ReleaseDispatch();
            delete hAPsim;
            hAPsim = NULL;

            //throw error
			AfxMessageBox("hAPsim initialization failed.");
            return;

        }
		CString sim = filename;
		BSTR bstr = sim.AllocSysString();
	
		
		VARIANT args[7];
		for (int i=0; i<7; i++)
			::VariantInit(&args[i]);
        
        hAPsim->InitFromArchive2(&bstr, args[0], args[1], args[2], args[3], args[4], args[5]);
		
		hAPsim->SetVisible(TRUE);

		nodePath=_T("");

        //the default constructor sets auto release of memory so there is no 
        //need to delete this mmeory later. ReleaseDispatch must be called to
        //make this happen.
		ihRoot = new IHNode();
		(*ihRoot) = hAPsim->GetTree();


		for (int i=0; i<7; i++)
			::VariantClear(&args[i]);

		//std::vector<DASIObj> testblocks;
		//std::vector<DASIObj> teststreams;
#ifdef YANGDEBUG
		CreateDummyDesignSpec();
#endif
*/        
	}

///////////////////////////////////////////////////////////////////////////////
    //SAFEARRAY * AspenPlusInterface::GetVariableList( CString itemName )
    std::vector< std::vector< std::string > > AspenPlusInterface::GetVariableList( CString itemName )
    //std::vector< std::string > AspenPlusInterface::GetVariableList( CString itemName )
    {
        //Convert itemname to BSTR due to requiremens of VB
        //BSTR iName = itemName.AllocSysString();
        
        //Get the list of variables
        //SAFEARRAY * list = AspenDynamicsQueryLib->GetVariableList( &iName );
        //return list;

        std::vector< std::vector < std::string > > blocks;
        //std::vector < std::string > blocks;

        // get document/simulation object      
        COleVariant vDisp(ADApplication->GetSimulation());
        //ADDocument->AttachDispatch(vDisp.pdispVal);
        //ASSERT(ADDocument->m_lpDispatch != NULL);

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

        // get count of blocks in collection
        //szName = OLESTR("Count");
        //pDispBlocksColl->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
        //::VariantInit(&result);
        //pDispBlocksColl->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
        //int nBlocks = result.lVal;

        // get the block names and put into listbox
        //CListBox *listbox = (CListBox *)GetDlgItem(IDC_LST_BLOCKS);
        //listbox->ResetContent();

        DISPID dispidItem;
        szName = OLESTR("Item");
        pDispBlocksColl->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispidItem);
        VARIANT index;
        ::VariantInit(&index);
        index.vt = VT_BSTR;

        //int i = 0;
        //for (int i = 0; i < nBlocks; i++)
        //{
            // get next block in collection
            //index.lVal = i + 1;   // collection index is "one based"
            index.bstrVal = itemName.AllocSysString();
            params.cArgs = 1;
            params.rgvarg = &index;
            ::VariantInit(&result);
            pDispBlocksColl->Invoke(dispidItem, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
            IDispatch* pDispBlock = result.pdispVal;  // get block dispatch

            // get block name
            //szName = OLESTR("Name");
            //pDispBlock->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
            //params.cArgs = 0;
            //params.rgvarg = NULL;
            //::VariantInit(&result);
            //pDispBlock->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
            //listbox->InsertString(i, OLE2T(result.bstrVal));
            //CString temp = result.bstrVal;
            //char * ctemp = temp.GetBuffer();
            //blocks.push_back( ctemp );

            // get block name
            szName = OLESTR("FindMatchingVariables");
            pDispBlock->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
            params.cArgs = 0;
            params.rgvarg = NULL;
            ::VariantInit(&result);
            pDispBlock->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
            //listbox->InsertString(i, OLE2T(result.bstrVal));
            IDispatch* pDispVariables = result.pdispVal;

            szName = OLESTR("Count");
            pDispVariables->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispid);
            ::VariantInit(&result);
            pDispVariables->Invoke(dispid, IID_NULL, ::GetUserDefaultLCID(), DISPATCH_PROPERTYGET, &params, &result, NULL, NULL);
            int nVars = result.lVal;

            szName = OLESTR("Item");
            pDispVariables->GetIDsOfNames(IID_NULL, &szName, 1, ::GetUserDefaultLCID(), &dispidItem);
            //VARIANT index;
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
                temp.Format(_T("%f"), result.dblVal);
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

                //blocks.push_back( ctemp );
                blocks.push_back( tempVarInfo );
                tempVarInfo.clear();
             }
        //}
        return blocks;
    }

///////////////////////////////////////////////////////////////////////////////
    void AspenPlusInterface::SetVariableValue( CString itemName,
        CString variableName, CString value  )
    {
        //Convert CStrings to BSTR due to requirements of VB
        //BSTR iName = itemName.AllocSysString();
        //BSTR vName = variableName.AllocSysString();
        //BSTR v = value.AllocSysString();

        //Set the value
        //AspenDynamicsQueryLib->SetVariableValue( &iName, &vName, &v );

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
	void AspenPlusInterface::Close()
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
    void AspenPlusInterface::Quit() 
	{
		if (simOpened)
        {
            ADApplication->Quit();
        }
    }

///////////////////////////////////////////////////////////////////////////////
//Save the document back;
	void AspenPlusInterface::Save()
	{
		if (simOpened)
        {
		    ADApplication->SaveDocument();
        }
	}

///////////////////////////////////////////////////////////////////////////////
//save this as another document
	void AspenPlusInterface::SaveAs(CString filename)
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
	void AspenPlusInterface::SetVisibility(bool status)
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
	void AspenPlusInterface::RunSolver( )
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
	void AspenPlusInterface::ResetSimulation( )
    {
        ADDocument->Reset( );
	}

    /*
	//Block, Stream and variable access functions
	int AspenPlusInterface::getNumOfBlocks() //get number of blocks in this document
	{
		CString blocknodepath;
		
		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		//blocknodepath.Insert(slength,".Blocks");
			
		IHNode node;

		node=nodeNav(*ihRoot,  blocknodepath);

		return getChildNum(node);
	}
	
	int AspenPlusInterface::getNumOfStreams() //get number of streams in the this document
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		//blocknodepath.Insert(slength,".Streams");
			
		IHNode node;
		
		node=nodeNav(*ihRoot,  blocknodepath);

		return getChildNum(node);
	}
		
	CString* AspenPlusInterface::getStreamNames() //get the string list of the stream names
	{
		int total= getNumOfStreams();
		int i;

		CString* results = new CString[total];
		DASIObj curobj;

		for (i=0;i<total;i++ )
		{
			curobj = getStreamByIndex(i);
			results[i] = curobj.getName();
		}
	
		return results;
		
	}

	CString* AspenPlusInterface::getBlockNames() //get the string list of the block names
	{
		int total= getNumOfBlocks();
		int i;

		CString* results = new CString[total];
		DASIObj curobj;

		for (i=0;i<total;i++ )
		{
			curobj = getBlockByIndex(i);
			results[i] = curobj.getName();
		}
	
		return results;
	}

	DASIObj AspenPlusInterface::getUpStreamBlock(DASIObj stream) //get a stream's upstream block
	{
		CString siPortName = stream.getSIPortName(0);
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,".Blocks");
		slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,siPortName);

		DASIObj result = DASIObj(*ihRoot, blockNodePath, BLOCK);

		return result;
	}
	
	DASIObj AspenPlusInterface::getDownStreamBlock(DASIObj stream) //get a stream's downstream block
	{
		CString doPortName = stream.getDOPortName(0);
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,".Blocks.");
		slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,doPortName);

		DASIObj result(*ihRoot, blockNodePath, BLOCK);
		
		return result;
	}

	DASIObj AspenPlusInterface::getBlockInletStream(DASIObj block, int portIndex, int streamIndex) //get a block's inlet stream
	{
		CString siPortName = block.getSIPortName(portIndex);
		CString blockPortPath = block.getNodePath();

		int slength = blockPortPath.GetLength();
		blockPortPath.Insert(slength,".Ports.");
		slength = blockPortPath.GetLength();
		blockPortPath.Insert(slength,siPortName);


		Variable portVar(*ihRoot, blockPortPath);

		Variable streamVar = portVar.getChild(streamIndex);

		if (!streamVar.valid())
			return DASIObj(*ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		DASIObj result(*ihRoot, streamNodePath, STREAM);

		return result;
	}
	
	DASIObj AspenPlusInterface::getBlockOutletStream(DASIObj block, int portIndex, int streamIndex) //get a block's outlet stream
	{
		CString doPortName = block.getDOPortName(portIndex);
		CString blockPortPath = block.getNodePath();

		int slength = blockPortPath.GetLength();
		blockPortPath.Insert(slength,".Ports.");
		slength = blockPortPath.GetLength();
		blockPortPath.Insert(slength,doPortName);

		Variable portVar(*ihRoot, blockPortPath);

		Variable streamVar = portVar.getChild(streamIndex);

		if (!streamVar.valid())
			return DASIObj(*ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		DASIObj result(*ihRoot, streamNodePath, STREAM);

		return result;
	}
	
	DASIObj AspenPlusInterface::getBlockByName(CString blockName) //get a block pointer by name
	{
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		//blockNodePath.Insert(slength,".Blocks.");
		//slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,blockName);

		DASIObj result(*ihRoot, blockNodePath, BLOCK);
		return result;
	}
	
	DASIObj AspenPlusInterface::getBlockByIndex(int index) //get a block pointer by index
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		blocknodepath.Insert(slength,".Blocks");
		Variable blocks(*ihRoot, blocknodepath);

		Variable blocki=blocks.getChild(index);

		DASIObj result(*ihRoot,blocki.getNodePath(), BLOCK);
		return result;
	}

	DASIObj AspenPlusInterface::getStreamByName(CString streamName) //get a stream pointer by the stream 
	{
		CString streamNodePath = nodePath;

		int slength = streamNodePath.GetLength();
		//streamNodePath.Insert(slength,".Streams.");
		//slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		DASIObj result(*ihRoot, streamNodePath, STREAM);
		return result;
	}

	DASIObj AspenPlusInterface::getStreamByIndex(int index) //get a stream pointer by index
	{
		CString streamnodepath;

		int slength = nodePath.GetLength();
		streamnodepath=nodePath;

		streamnodepath.Insert(slength,".Streams");
		Variable streams(*ihRoot, streamnodepath);

		Variable streami = streams.getChild(index);

		DASIObj result(*ihRoot, streami.getNodePath(), STREAM);
		return result;
	}

	Variable AspenPlusInterface::getVarByNodePath(CString path) //get a viable pointer by the node path in the variable expoloer
	{
		Variable result(*ihRoot, path);

		return result;
	}

	Variable AspenPlusInterface::getVarByAlias(CString alias) //get a pointer to a variable by alias name
	{
		std::map<CString, CString>::iterator iter;
		CString varnodepath;

		if ((iter=aliasMapping.find(alias))!=aliasMapping.end())
			varnodepath= iter->second;
		else 
			return Variable(*ihRoot, "");

		Variable result(*ihRoot, varnodepath);		
		return result;
	}
		
		//simulation control fucntions
*/		


 /*   void AspenPlusInterface::reinitializeBlock( CString block ) //Initializes the current solution to its initial state. May purge the current results form the problem.
	{
		//hAPsim->Reinit();
        
		VARIANT name;
        ::VariantInit(&name);
        name.vt = VT_BSTR;
        name.bstrVal = block.AllocSysString();

        VARIANT enumer;
        ::VariantInit(&enumer);
        enumer.vt = VT_I4;
        enumer.lVal = IAP_REINIT_BLOCK;
        
		IHAPEngine engine;
		engine = hAPsim->GetEngine();
        engine.Reinit( enumer, name  );

        ::VariantClear( &name );
        ::VariantClear( &enumer );
	}

	void AspenPlusInterface::runSolver(bool async)
	{
		VARIANTARG vasync;
		::VariantInit(&vasync);
		
		vasync.vt= VT_BOOL;
		if(async)
			vasync.boolVal = VARIANT_TRUE;
		else
			vasync.boolVal = VARIANT_FALSE;

		hAPsim->Run2(vasync);
        ::VariantClear( &vasync );
	}
	
	
	void AspenPlusInterface::solved()  //callback when a problem is solved
	{
		return;	
	}

	void AspenPlusInterface::failed()  //callback when a problem solving is failed
	{
		return;
	}	

	void AspenPlusInterface::step()
	{
		IHAPEngine engine;
		engine = hAPsim->GetEngine();
		engine.Step();
	}

	void AspenPlusInterface::CreateDummyDesignSpec()
	{
#ifdef YANGDEBUG
		test = fopen("Test.txt", "w");
#endif
		//"Data\Flowsheeting Options\Design-Spec"
		CString designspecpath;

		int slength = nodePath.GetLength();
		designspecpath=nodePath;

		designspecpath.Insert(slength,".Flowsheeting Options.Design-Spec");
			
		IHNode node;
		
		node=nodeNav(*ihRoot,  designspecpath);

		//Now inserting a dummy node
		CString dummy="DUMMY";
		BSTR bstr = dummy.AllocSysString();
		node.NewChild(&bstr);
		::SysFreeString( bstr );
		
		slength = designspecpath.GetLength();
		designspecpath.Insert(slength,".DUMMY.Input.FVN_VARIABLE");
		node=nodeNav(*ihRoot,  designspecpath);

		IHNodeCol ihcol = node.GetElements();
		int d = ihcol.GetDimension(); //d is not suppose to > 5 a
		long *rc = new long[d];
		int i, j, ind, total=0;
		VARIANTARG arg[5];
		VARIANT val, force;
		CString nodename;
		IHNode cnode;

		VARIANT val1, val2;
		
		::VariantInit(&val1);
		::VariantInit(&val2);
		
		stringToVariant("VARNAME", val1);
		//stringToVariant("", val2);
		val2.vt=VT_INT;
		V_INT(&val2)=0;
		cnode=ihcol.GetItem(val2,val2, val2, val2 , val2);
		cnode=ihcol.GetItem(val1,val2, val2, val2 , val2);
		cnode=ihcol.GetLabelNode(0,0,&val1);
		long k = ihcol.GetCount();
		
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}

		//total = ihcol.GetCount();
		for (i=0; i<total; i++)
		{
			ind = i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=ind%rc[j];
				ind = ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
			
			nodename = cnode.GetName(force);
		}
		
        ::VariantClear( &val1 );
        ::VariantClear( &val2 );
		//readBranch(node);
#ifdef YANGDEBUG		
		fclose(test);
#endif
	}*/
} //namespace DASI