#pragma warning (disable: 4786)
//#include "stdafx.h"
#include "CASI.h"

#ifdef YANGDEBUG
FILE* test;
#endif
namespace CASI
{
	////////////////////////////////public functions///////////////////////
	CASIBroker::CASIBroker() //default constructor 
	{
	
	}

	void CASIBroker::init() //initialization should go here. 
	{

	//Put this call here instead of make it in constructor is to avoid some weire DLL potential problem
	
	
	}
	
	
	void CASIBroker::fini() //finalize the broker;
	{
	
	}


	void readTree(IHapp hAPsim)
	{
		IHNodeCol ihcol;
		IHNode ihRoot;

		ihRoot = hAPsim.GetTree();
	    ihcol = ihRoot.GetElements();
		
		readBranch(ihRoot);

	}

	void readBranch(IHNode& root)
	{
		static int level=0;
		IHNode cnode;
		IHNodeCol ihcol;
		VARIANTARG arg[5];
	
		int i, j, d, total, ind;
		long* rc;
		CString nodepath;

		VARIANT val, force;
		CString the_sec;
		int vt;
	
		level++;
		TRY
		{
			// do stuff that may throw exceptions

			::VariantInit(&val);
			::VariantInit(&force);

			for (i=0; i<5; i++) 
				::VariantInit(&arg[i]);
			
			d = root.GetDimension();
			vt =root.GetValueType();

			if (d>0&&vt<=0) //has offsprings
			{
				ihcol = root.GetElements();
		
				d = ihcol.GetDimension(); //d is not suppose to > 5 a
				rc = new long[d];
				total=0;
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
			
					nodepath = cnode.GetName(force);
#ifdef YANGDEBUG
					for (int li=0; li<level; li++)
						fprintf(test, "\t");
					fprintf(test,"%s\n", nodepath);
#endif
					readBranch(cnode);
				}

				delete rc;
			}
		}
		CATCH_ALL( e )
		{
			level--;
			return;
		}
		END_CATCH_ALL
		level--;
	} //end of readBranch

	IHNode nodeNav(IHNode& root, CString NodePath)
	{
	//	int i, j;
		int len;
		IHNode cnode;
		int dotPos;
	//	char* sbuf;
		
		CString curPath;	

		CString NodeName=NodePath;
		cnode = root;
		
		do 
		{
			len = NodeName.GetLength();
			dotPos = NodeName.Find(_T("."));
			if (dotPos==-1)
			{
				curPath=NodeName;
				cnode = cnode.FindNode(curPath);
				if (cnode.m_lpDispatch==NULL)
					return cnode;
				break;
			}
			else
				curPath=NodeName.Left(dotPos);
			NodeName=NodeName.Right(len-dotPos-1);
			cnode = cnode.FindNode(curPath);
			if (cnode.m_lpDispatch==NULL)
					return cnode;
		} while (len>0);

		return cnode;
	}

	void readStreamsAndBlocks(IHNode& ihRoot, std::vector<CASIObj> &blocks, std::vector<CASIObj> &streams)
	{
		IHNode streamRoot;
	
		std::vector<CString> blockName;
		std::vector<CString> streamName;

		streamRoot=nodeNav(ihRoot, "Data.Streams");

		IHNode blockRoot;

		blockRoot=nodeNav(ihRoot, "Data.Blocks");
		
		int numOfBlocks=getChildNum(blockRoot);

		getChildNames(blockRoot, blockName);

		blocks.resize(numOfBlocks);

		int i;
		CString cur_blockpath;
		for (i=0; i<numOfBlocks; i++)
		{
			cur_blockpath=blockName[i];
			cur_blockpath.Insert(0,_T("."));
			cur_blockpath.Insert(0,_T("Data.Blocks"));
			CASIObj tempblock(ihRoot, cur_blockpath, BLOCK);
			blocks[i]=tempblock;
		}

		int numOfStreams=getChildNum(streamRoot);

		getChildNames(streamRoot, streamName);

		streams.resize(numOfStreams);
		CString cur_streampath;

		for (i=0; i<numOfStreams; i++)
		{
			cur_streampath=blockName[i];
			cur_streampath.Insert(0,_T("."));
			cur_streampath.Insert(0,_T("Data.Streams"));
			CASIObj tempstream(ihRoot, cur_streampath, STREAM);
			streams[i]=tempstream;
		}
		
	}

	int getChildNum(IHNode &root)
	{
		IHNodeCol ihcol;
		int d, i, total;
		int rc;

		total = 0;
		if (root.m_lpDispatch==NULL)
			return 0;
		d = root.GetDimension();
		if (d>0)
		{
			ihcol=root.GetElements();
			//total = ihcol.GetCount();

			for (i=0; i<d; i++)
			{
				rc = ihcol.GetRowCount(i);
				total+=rc;
			}
		}
		return total;
	}
	
	void getChildNames(IHNode &root, std::vector<CString>& results)
	{
		IHNodeCol ihcol;
		int d, i, total;
		int ind, j;
		long* rc;
		VARIANTARG arg[5];
		CString cnodename;
		IHNode cnode;
		VARIANTARG force;

		results.clear();
		if (root.m_lpDispatch==NULL)
			return;
		
		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=root.GetElements();
		d = ihcol.GetDimension(); 

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{	rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}
		
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
			cnodename = cnode.GetName(force);
			results.push_back(cnodename);
		}
		
		delete [] rc;
	}
	
	CString variantToString(VARIANTARG& va)
	{
		/*
		union {
			Byte                    bVal;                 // VT_UI1.
		    Short                   iVal;                 // VT_I2.
			long                    lVal;                 // VT_I4.
			float                   fltVal;               // VT_R4.
			double                  dblVal;               // VT_R8.
			VARIANT_BOOL            boolVal;              // VT_BOOL.
			SCODE                   scode;                // VT_ERROR.
			CY                      cyVal;                // VT_CY.
			DATE                    date;                 // VT_DATE.
			BSTR                    bstrVal;              // VT_BSTR.
			DECIMAL                 FAR* pdecVal          // VT_BYREF|VT_DECIMAL.
			IUnknown                FAR* punkVal;         // VT_UNKNOWN.
			IDispatch               FAR* pdispVal;        // VT_DISPATCH.
			SAFEARRAY               FAR* parray;          // VT_ARRAY|*.
			Byte                    FAR* pbVal;           // VT_BYREF|VT_UI1.
			short                   FAR* piVal;           // VT_BYREF|VT_I2.
			long                    FAR* plVal;           // VT_BYREF|VT_I4.
			float                   FAR* pfltVal;         // VT_BYREF|VT_R4.
			double                  FAR* pdblVal;         // VT_BYREF|VT_R8.
			VARIANT_BOOL            FAR* pboolVal;        // VT_BYREF|VT_BOOL.
			SCODE                   FAR* pscode;          // VT_BYREF|VT_ERROR.
			CY                      FAR* pcyVal;          // VT_BYREF|VT_CY.
			DATE                    FAR* pdate;           // VT_BYREF|VT_DATE.
			BSTR                    FAR* pbstrVal;        // VT_BYREF|VT_BSTR.
			IUnknown                FAR* FAR* ppunkVal;   // VT_BYREF|VT_UNKNOWN.
			IDispatch               FAR* FAR* ppdispVal;  // VT_BYREF|VT_DISPATCH.
			SAFEARRAY               FAR* FAR* pparray;    // VT_ARRAY|*.
			VARIANT                 FAR* pvarVal;         // VT_BYREF|VT_VARIANT.
			void                    FAR* byref;           // Generic ByRef.
			char                    cVal;                 // VT_I1.
			unsigned short          uiVal;                // VT_UI2.
			unsigned long           ulVal;                // VT_UI4.
			int                     intVal;               // VT_INT.
			unsigned int            uintVal;              // VT_UINT.
			char FAR *              pcVal;                // VT_BYREF|VT_I1.
			unsigned short FAR *    puiVal;               // VT_BYREF|VT_UI2.
			unsigned long FAR *     pulVal;               // VT_BYREF|VT_UI4.
			int FAR *               pintVal;              // VT_BYREF|VT_INT.
			unsigned int FAR *      puintVal;             //VT_BYREF|VT_UINT.
		};
	*/

	    CString s;
		char str[80];
		switch(va.vt)
		{ /* vt */
		case VT_UI1:
			s.Format(_T("%d"), va.bVal);
			return s;
		case VT_I2:
			s.Format(_T("%d"), va.iVal);
			return s;
		case VT_I4:
			s.Format(_T("%d"), va.lVal);
			return s;
		case VT_R4:
			s.Format(_T("%f"), va.fltVal);
			return s;
		case VT_R8:
			s.Format(_T("%lf"), va.dblVal);
			return s;
		case VT_BOOL:
			if (va.boolVal)
				s=_T("TRUE");
			else
				s=_T("FALSE");
			return s;
		case VT_ERROR:
			s.Format(_T("%ld"), va.scode);
			return s;
		case VT_BSTR:
			return CString(va.bstrVal);
		case VT_UI1|VT_BYREF:
			s.Format(_T("%d"), *(va.pbVal));
			return s;
		case VT_I2|VT_BYREF:
			s.Format(_T("%d"), *(va.piVal));
			return s;
		case VT_I4|VT_BYREF:
			s.Format(_T("%d"), *(va.plVal));
			return s;
		case VT_R4|VT_BYREF:
			s.Format(_T("%f"), *(va.pfltVal));
			return s;
		case VT_R8|VT_BYREF:
			s.Format(_T("%lf"), *(va.pdblVal));
			return s;
		case VT_BOOL|VT_BYREF:
			if (*(va.pboolVal))
				s="TRUE";
			else
				s="FALSE";
			return s;
		case VT_ERROR|VT_BYREF:
			s.Format(_T("%ld"), *(va.pscode));
			return s;
		case VT_BSTR|VT_BYREF:
			return CString(*(va.pbstrVal));
		default:
          //ASSERT(FALSE); // unknown VARIANT type (this ASSERT is optional)
			sprintf_s(str, "%d", va.vt);
          return CString(str);
		} /* vt */

		return CString("N/A");
	}

	void stringToVariant(CString val, VARIANT& var)
	{
		COleVariant cvar;

		//cvar.SetString(LPCTSTR(val),var.vt);

		::VariantCopy(&var, COleVariant(val));

		return;
	}
}

