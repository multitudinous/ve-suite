#pragma warning (disable: 4786)
//#include "stdafx.h"
#include "CASI.h"

#ifdef YANGDEBUG
extern FILE* test;
#endif
namespace CASI
{

	CASIDocument::CASIDocument()
	{
		simOpened = false;
		ihRoot=NULL;
		hAPsim=NULL;
	}

	CASIDocument::~CASIDocument()
	{
	}

	IHNode CASIDocument::getRoot()
	{
		return *ihRoot;
	}
	//File operating functions
	void CASIDocument::open(CString filename) //Open an Aspen Document
	{
		if (simOpened)
              close();

        //the default constructor sets auto release of memory so there is no 
        //need to delete this mmeory later. ReleaseDispatch must be called to
        //make this happen.
		hAPsim = new IHapp(); 
		BOOL bSuccess = hAPsim->CreateDispatch(_T("apwn.document.IP"));
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

		simOpened = true;

		//std::vector<CASIObj> testblocks;
		//std::vector<CASIObj> teststreams;
#ifdef YANGDEBUG
		CreateDummyDesignSpec();
#endif
        
	}
	
	void CASIDocument::close() //Close the file, clear up 
	{
		if (simOpened)
		{
            VARIANTARG reserved;
            ::VariantInit(&reserved);
            reserved.vt=VT_BOOL;
            reserved.boolVal=VARIANT_TRUE;

			hAPsim->Close(reserved);
			ihRoot->ReleaseDispatch();
            hAPsim->ReleaseDispatch();
            delete ihRoot;
            delete hAPsim;
			ihRoot = NULL;
            hAPsim = NULL;

			simOpened = false;
		}
	}
	
	void CASIDocument::save() //Save the document back;
	{
		hAPsim->Save();
	}
	
	void CASIDocument::saveAs(CString filename) //save this as another document
	{
		CString cfname = _T(filename);
		
		VARIANTARG overwrite;
		::VariantInit(&overwrite);	

		BSTR bsfname = cfname.AllocSysString();
		hAPsim->SaveAs(&bsfname, overwrite);
	}

	void CASIDocument::showAspen(bool status)
	{
		if (simOpened)
				hAPsim->SetVisible(status);
	}
	//Block, Stream and variable access functions
	int CASIDocument::getNumOfBlocks() //get number of blocks in this document
	{
		CString blocknodepath;
		
		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		//blocknodepath.Insert(slength,".Blocks");
			
		IHNode node;

		node=nodeNav(*ihRoot,  blocknodepath);

		return getChildNum(node);
	}
	
	int CASIDocument::getNumOfStreams() //get number of streams in the this document
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		//blocknodepath.Insert(slength,".Streams");
			
		IHNode node;
		
		node=nodeNav(*ihRoot,  blocknodepath);

		return getChildNum(node);
	}
		
	CString* CASIDocument::getStreamNames() //get the string list of the stream names
	{
		int total= getNumOfStreams();
		int i;

		CString* results = new CString[total];
		CASIObj curobj;

		for (i=0;i<total;i++ )
		{
			curobj = getStreamByIndex(i);
			results[i] = curobj.getName();
		}
	
		return results;
		
	}

	CString* CASIDocument::getBlockNames() //get the string list of the block names
	{
		int total= getNumOfBlocks();
		int i;

		CString* results = new CString[total];
		CASIObj curobj;

		for (i=0;i<total;i++ )
		{
			curobj = getBlockByIndex(i);
			results[i] = curobj.getName();
		}
	
		return results;
	}

	CASIObj CASIDocument::getUpStreamBlock(CASIObj stream) //get a stream's upstream block
	{
		CString siPortName = stream.getSIPortName(0);
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,".Blocks");
		slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,siPortName);

		CASIObj result = CASIObj(*ihRoot, blockNodePath, BLOCK);

		return result;
	}
	
	CASIObj CASIDocument::getDownStreamBlock(CASIObj stream) //get a stream's downstream block
	{
		CString doPortName = stream.getDOPortName(0);
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,".Blocks.");
		slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,doPortName);

		CASIObj result(*ihRoot, blockNodePath, BLOCK);
		
		return result;
	}

	CASIObj CASIDocument::getBlockInletStream(CASIObj block, int portIndex, int streamIndex) //get a block's inlet stream
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
			return CASIObj(*ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(*ihRoot, streamNodePath, STREAM);

		return result;
	}
	
	CASIObj CASIDocument::getBlockOutletStream(CASIObj block, int portIndex, int streamIndex) //get a block's outlet stream
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
			return CASIObj(*ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(*ihRoot, streamNodePath, STREAM);

		return result;
	}
	
	CASIObj CASIDocument::getBlockByName(CString blockName) //get a block pointer by name
	{
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		//blockNodePath.Insert(slength,".Blocks.");
		//slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,blockName);

		CASIObj result(*ihRoot, blockNodePath, BLOCK);
		return result;
	}
	
	CASIObj CASIDocument::getBlockByIndex(int index) //get a block pointer by index
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		blocknodepath.Insert(slength,".Blocks");
		Variable blocks(*ihRoot, blocknodepath);

		Variable blocki=blocks.getChild(index);

		CASIObj result(*ihRoot,blocki.getNodePath(), BLOCK);
		return result;
	}

	CASIObj CASIDocument::getStreamByName(CString streamName) //get a stream pointer by the stream 
	{
		CString streamNodePath = nodePath;

		int slength = streamNodePath.GetLength();
		//streamNodePath.Insert(slength,".Streams.");
		//slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(*ihRoot, streamNodePath, STREAM);
		return result;
	}

	CASIObj CASIDocument::getStreamByIndex(int index) //get a stream pointer by index
	{
		CString streamnodepath;

		int slength = nodePath.GetLength();
		streamnodepath=nodePath;

		streamnodepath.Insert(slength,".Streams");
		Variable streams(*ihRoot, streamnodepath);

		Variable streami = streams.getChild(index);

		CASIObj result(*ihRoot, streami.getNodePath(), STREAM);
		return result;
	}

	Variable CASIDocument::getVarByNodePath(CString path) //get a viable pointer by the node path in the variable expoloer
	{
		Variable result(*ihRoot, path);

		return result;
	}

	Variable CASIDocument::getVarByAlias(CString alias) //get a pointer to a variable by alias name
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
		
	void CASIDocument::initializeSolver() //Initializes the current solution to its initial state. May purge the current results form the problem.
	{
		hAPsim->Reinit();
	}

	void CASIDocument::runSolver(bool async)
	{
		VARIANTARG vasync;
		::VariantInit(&vasync);
		
		vasync.vt= VT_BOOL;
		if(async)
			vasync.boolVal = VARIANT_TRUE;
		else
			vasync.boolVal = VARIANT_FALSE;

		hAPsim->Run2(vasync);

	}
	
	
	void CASIDocument::solved()  //callback when a problem is solved
	{
		return;	
	}

	void CASIDocument::failed()  //callback when a problem solving is failed
	{
		return;
	}	

	void CASIDocument::step()
	{
		IHAPEngine engine;
		engine = hAPsim->GetEngine();
		engine.Step();
	}

	void CASIDocument::CreateDummyDesignSpec()
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
		
		//readBranch(node);
#ifdef YANGDEBUG		
		fclose(test);
#endif
	}
} //namespace CASI