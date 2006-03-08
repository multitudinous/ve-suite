#pragma warning (disable: 4786)
#include "CASI.h"

namespace CASI
{

	CASIDocument::CASIDocument()
	{
		BOOL bSuccess = hAPsim.CreateDispatch(_T("apwn.document.IP"));
		//BOOL bSuccess = hAPsim.CreateDispatch(_T("apwn.document"));
		if (!bSuccess) 
			AfxMessageBox("hAPsim initialization failed.");

		simOpened = false;
	}

	CASIDocument::~CASIDocument()
	{
		close();
	}

	IHNode CASIDocument::getRoot()
	{
		return ihRoot;
	}
	//File operating functions
	void CASIDocument::open(CString filename) //Open an Aspen Document
	{
		CString sim = filename;
		BSTR bstr = sim.AllocSysString();
		//OLECHAR* szName = OLESTR("");
		static bool firstTime=true;
	
		
		VARIANT args[7];
		int i;
		for (i=0; i<7; i++)
			::VariantInit(&args[i]);
		
		//if (firstTime)
		//{
			hAPsim.InitFromArchive2(&bstr, args[0], args[1], args[2], args[3], args[4], args[5]);//, args[6]);
			firstTime=false;
		//}
		//else
		//	hAPsim.Readback(LPCTSTR (filename), 0);
		
		hAPsim.SetVisible(FALSE);

		nodePath=_T("Data");

		ihRoot = hAPsim.GetTree();

		simOpened = true;
	}
	
	void CASIDocument::close() //Close the file, clear up 
	{
		VARIANTARG reserved;
		::VariantInit(&reserved);
		if (simOpened)
		{
			hAPsim.Close(reserved);
			//hAPsim.Reinit();
			simOpened = false;
		}
	}
	
	void CASIDocument::save() //Save the document back;
	{
		hAPsim.Save();
	}
	
	void CASIDocument::saveAs(CString filename) //save this as another document
	{
		CString cfname = _T(filename);
		
		VARIANTARG overwrite;
		::VariantInit(&overwrite);	

		BSTR bsfname = cfname.AllocSysString();
		hAPsim.SaveAs(&bsfname, overwrite);
	}

	void CASIDocument::showAspen(bool status)
	{
		if (simOpened)
				hAPsim.SetVisible(status);
	}
	//Block, Stream and variable access functions
	int CASIDocument::getNumOfBlocks() //get number of blocks in this document
	{
		CString blocknodepath;
		
		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		blocknodepath.Insert(slength,".Blocks");
			
		IHNode node;

		node=nodeNav(ihRoot,  blocknodepath);

		return getChildNum(node);
	}
	
	int CASIDocument::getNumOfStreams() //get number of streams in the this document
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		blocknodepath.Insert(slength,".Streams");
			
		IHNode node;
		
		node=nodeNav(ihRoot,  blocknodepath);

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

		CASIObj result = CASIObj(ihRoot, blockNodePath, BLOCK);

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

		CASIObj result(ihRoot, blockNodePath, BLOCK);
		
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


		Variable portVar(ihRoot, blockPortPath);

		Variable streamVar = portVar.getChild(streamIndex);

		if (!streamVar.valid())
			return CASIObj(ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(ihRoot, streamNodePath, STREAM);

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

		Variable portVar(ihRoot, blockPortPath);

		Variable streamVar = portVar.getChild(streamIndex);

		if (!streamVar.valid())
			return CASIObj(ihRoot, "", STREAM);
		
		CString streamName = streamVar.getName(); //It cut the last part out, which is the stream name;
		CString streamNodePath = nodePath;

		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(ihRoot, streamNodePath, STREAM);

		return result;
	}
	
	CASIObj CASIDocument::getBlockByName(CString blockName) //get a block pointer by name
	{
		CString blockNodePath = nodePath;

		int slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,".Blocks.");
		slength = blockNodePath.GetLength();
		blockNodePath.Insert(slength,blockName);

		CASIObj result(ihRoot, blockNodePath, BLOCK);
		return result;
	}
	
	CASIObj CASIDocument::getBlockByIndex(int index) //get a block pointer by index
	{
		CString blocknodepath;

		int slength = nodePath.GetLength();
		blocknodepath=nodePath;

		blocknodepath.Insert(slength,".Blocks");
		Variable blocks(ihRoot, blocknodepath);

		Variable blocki=blocks.getChild(index);

		CASIObj result(ihRoot,blocki.getNodePath(), BLOCK);
		return result;
	}

	CASIObj CASIDocument::getStreamByName(CString streamName) //get a stream pointer by the stream 
	{
		CString streamNodePath = nodePath;

		int slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,".Streams.");
		slength = streamNodePath.GetLength();
		streamNodePath.Insert(slength,streamName);

		CASIObj result(ihRoot, streamNodePath, STREAM);
		return result;
	}

	CASIObj CASIDocument::getStreamByIndex(int index) //get a stream pointer by index
	{
		CString streamnodepath;

		int slength = nodePath.GetLength();
		streamnodepath=nodePath;

		streamnodepath.Insert(slength,".Streams");
		Variable streams(ihRoot, streamnodepath);

		Variable streami = streams.getChild(index);

		CASIObj result(ihRoot, streami.getNodePath(), STREAM);
		return result;
	}

	Variable CASIDocument::getVarByNodePath(CString path) //get a viable pointer by the node path in the variable expoloer
	{
		Variable result(ihRoot, path);

		return result;
	}

	Variable CASIDocument::getVarByAlias(CString alias) //get a pointer to a variable by alias name
	{
		std::map<CString, CString>::iterator iter;
		CString varnodepath;

		if ((iter=aliasMapping.find(alias))!=aliasMapping.end())
			varnodepath= iter->second;
		else 
			return Variable(ihRoot, "");

		Variable result(ihRoot, varnodepath);		
		return result;
	}
		
		//simulation control fucntions
		
	void CASIDocument::initializeSolver() //Initializes the current solution to its initial state. May purge the current results form the problem.
	{
		hAPsim.Reinit();
	}

	void CASIDocument::runSolver(bool async)
	{
		VARIANTARG vasync;
		::VariantInit(&vasync);

		vasync.vt=VT_BOOL;

		CString val="TRUE";

		if (!async)
			val="FALSE";

		stringToVariant(val, vasync);

		hAPsim.Run2(vasync);

	}
	
	
	void CASIDocument::solved()  //callback when a problem is solved
	{
		return;	
	}

	void CASIDocument::failed()  //callback when a problem solving is failed
	{
		return;
	}	


} //namespace CASI