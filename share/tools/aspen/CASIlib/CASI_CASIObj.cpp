#pragma warning (disable: 4786)
//#include "stdafx.h"
#include "CASI.h"

#include <vector>

namespace CASI
{
	CASIObj::CASIObj(IHNode root, CString nodepath, VARTYPE nodetype)
		:Variable(root,nodepath, nodetype)
	{
		//if (nodeType==STREAM)
		//	prepStream();
		//else if (nodeType==BLOCK)
			prepBlock();
	}
	
	CString CASIObj::getOutputVarName(int index) //get a varaible name in the output category by index
	{
		return  blockOutputs[index];//(varnodePath.Right(slength-lastDot-1));
	}
	
	CString CASIObj::getInputVarName(int index) //get a varaible name in the input category by index
	{
		return  blockInputs[index];//(varnodePath.Right(slength-lastDot-1));
	}
	
	CString CASIObj::getSIPortName(int index) //get the source or input port name by index
	{
		CString siPortPath;
		CString varnodename;

		int slength = nodePath.GetLength();
		siPortPath=nodePath;

		if (nodeType==STREAM)
			siPortPath.Insert(slength, ".Ports.SOURCE");
		else if (nodeType==BLOCK)
			siPortPath.Insert(slength, ".Ports");
		else
			return CString("");
			
		IHNode node, cnode;

		node=nodeNav(ihRoot,  siPortPath);

		IHNodeCol ihcol;
		int d, i, j, total;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;

		::VariantInit(&force);
		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);

		ihcol=node.GetElements();
		d = ihcol.GetDimension();

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}
		
		//int lastDot;
		slength;
		std::vector< ::CString > results;
		int cur_ind;
		for (i=0; i<total; i++)
		{
			cur_ind=i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=cur_ind%rc[j];
				cur_ind = cur_ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
			varnodename = cnode.GetName(force);
			

			if (nodeType==BLOCK)
			{
				if (varnodename.Find("(IN)")!=-1)
					results.push_back(varnodename);
			}
			else
				results.push_back(varnodename);
		}
		
		delete [] rc;

		if (index<0 || index>=results.size())
			return CString("");

		CString vname=results[index];
		//lastDot=vpath.ReverseFind('.');

		//slength=vpath.GetLength();

		return  vname;//vpath.Right(slength-lastDot-1);
			
	}
	
	CString CASIObj::getDOPortName(int index) //get the destination or output port name by index
	{
		CString doPortPath;
		CString varnodename;

		int slength = nodePath.GetLength();
		doPortPath=nodePath;

		if (nodeType==STREAM)
			doPortPath.Insert(slength,".Ports.DEST");
		else if (nodeType==BLOCK)
			doPortPath.Insert(slength,".Ports");
		else
			return CString("");
			
		IHNode node, cnode;

		node=nodeNav(ihRoot,  doPortPath);

		IHNodeCol ihcol;
		int d, i, j, total;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;
		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=node.GetElements();
		d = ihcol.GetDimension();

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}
		
//		int lastDot;
		slength;
		std::vector<CString> results;
		int cur_ind;

		for (i=0; i<total; i++)
		{
			cur_ind=i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=cur_ind%rc[j];
				cur_ind = cur_ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
			varnodename = cnode.GetName(force);

			if (nodeType==BLOCK)
			{
				if (varnodename.Find("(OUT)")!=-1)
					results.push_back(varnodename);
			}
			else
				results.push_back(varnodename);
		}
		
		delete [] rc;

		if (index<0 || index>=results.size())
			return CString("");

		//CString vpath=results[index];
		//lastDot=vpath.ReverseFind('.');

		//slength=vpath.GetLength();

		return  results[index];//vpath.Right(slength-lastDot-1);

					
	}

	Variable CASIObj::getOutputVarByName(CString vname) //get a pointer to a output variable by name
	{
		CString varnodepath;
		int slength = nodePath.GetLength();
		varnodepath= nodePath;
		varnodepath.Insert(slength,".Output.");
		slength = varnodepath.GetLength();
		varnodepath.Insert(slength,vname);

		Variable result(ihRoot, varnodepath);

		return result;
	}
	
	Variable CASIObj::getInputVarByName(CString vname)	//get a pointer to a input variable by name
	{
		CString varnodepath;
		int slength = nodePath.GetLength();
		varnodepath= nodePath;
		varnodepath.Insert(slength,".Input.");
		slength = varnodepath.GetLength();
		varnodepath.Insert(slength,vname);

		Variable result(ihRoot, varnodepath);
		return result;
	}

	Variable CASIObj::getInputVarByIndex(int index)  //get a pointer to a input variable by index
	{
		CString varname = getInputVarName(index);
		
		return getInputVarByName(varname);		
	}
	
	Variable CASIObj::getOutputVarByIndex(int index) //get a pointer to a output variable by index
	{
		CString varname = getOutputVarName(index);

		return getOutputVarByName(varname);		
	}
	

	int CASIObj::getNumCompOfStream() //get number of the components in Table seciton for a certain stream
	{
		return streamComps.size();
	}

	CString* CASIObj::GetStreamComponentNameList() //get the string list of stream component names
	{
		
		CString *results;
		std::set<CString>::iterator iter;
		int i=0;

		results = new CString[streamComps.size()];
		for (iter=streamComps.begin();iter!=streamComps.end();iter++)
			results[i++]=*iter;

		return results;
	}
	
	Variable CASIObj::GetStreamComponentVarByName(CString streamName, CString componentName) //get the varialble pointer of a stream component
	{
		CString streamnodepath;
		int slength = nodePath.GetLength();

		streamnodepath= nodePath;
		
		//streamnodepath.Insert(slength,".Stream Results.Table.(");
		streamnodepath.Insert(slength,streamName);
		slength=streamnodepath.GetLength();
		streamnodepath.Insert(slength,".Input.FLOW.MIXED.");
		slength=streamnodepath.GetLength();
		streamnodepath.Insert(slength,componentName);
		//slength=streamnodepath.GetLength();
		//streamnodepath.Insert(slength,",");
		//slength=streamnodepath.GetLength();
		//streamnodepath.Insert(slength,streamName);
		//slength=streamnodepath.GetLength();
		//streamnodepath.Insert(slength,")");

		Variable resultvar(ihRoot, streamnodepath);

		return resultvar;
	}
	
	CString CASIObj::GetStreamComponentVarValue(CString streamName, CString componentName) //get the value of a stream component
	{
		Variable var;
		CString result;

		var=GetStreamComponentVarByName(streamName, componentName);
		result=var.getValue();

		return result;
	}

	bool CASIObj::SetStreamComponentVarValue(CString streamName, CString componentName, CString value) //set the value of a stream component
	{
		Variable var;
		bool result;

		var=GetStreamComponentVarByName(streamName, componentName);
		result=var.setValue(value);

		return result;
	}

	int CASIObj::getNumInputVar() //get number of input variable
	{
		CString varnodepath;

		int slength = nodePath.GetLength();
		varnodepath=nodePath;

		varnodepath.Insert(slength,".Input");
			
		IHNode node;

		node=nodeNav(ihRoot,  varnodepath);

		return getChildNum(node);
	}
	
	int CASIObj::getNumOutputVar() //get number of output variable
	{
		CString varnodepath;

		int slength = nodePath.GetLength();
		varnodepath=nodePath;

		varnodepath.Insert(slength,".Output");
			
		IHNode node;

		node=nodeNav(ihRoot,  varnodepath);

		return getChildNum(node);
	}
	
	int CASIObj::getNumSIPort() //get number of source/input port
	{
		CString siPortPath;
		CString varnodename;

		int slength = nodePath.GetLength();
		siPortPath=nodePath;

		if (nodeType==STREAM)
			siPortPath.Insert(slength,".Ports.SOURCE");
		else if (nodeType==BLOCK)
			siPortPath.Insert(slength,".Ports");
		else
			return NULL;
			
		IHNode node, cnode;

		node=nodeNav(ihRoot,  siPortPath);

		IHNodeCol ihcol;
		int d, i, j, total;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;

		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=node.GetElements();

		d = ihcol.GetDimension(); 

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}
		
		std::vector<CString> results;
		int cur_ind;

		for (i=0; i<total; i++)
		{
			cur_ind=i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=cur_ind%rc[j];
				cur_ind = cur_ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
			varnodename = cnode.GetName(force);

			if (nodeType==BLOCK)
			{
				if (varnodename.Find("(IN)")!=-1)
					results.push_back(varnodename);
			}
			else
				results.push_back(varnodename);
		}

		delete [] rc;

		return results.size();
		
	}

	int CASIObj::getNumDOPort() //get number of destination/output port
	{
		CString siPortPath;
		CString varnodename;

		int slength = nodePath.GetLength();
		siPortPath=nodePath;

		if (nodeType==STREAM)
			siPortPath.Insert(slength,".Ports.DEST");
		else if (nodeType==BLOCK)
			siPortPath.Insert(slength,".Ports");
		else
			return NULL;
			
		IHNode node, cnode;

		node=nodeNav(ihRoot,  siPortPath);

		IHNodeCol ihcol;
		int d, i, j, total;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;

		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=node.GetElements();

		d = ihcol.GetDimension(); 

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}
		
		std::vector<CString> results;
		int cur_ind;

		for (i=0; i<total; i++)
		{
			cur_ind=i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=cur_ind%rc[j];
				cur_ind = cur_ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
			varnodename = cnode.GetName(force);

			if (nodeType==BLOCK)
			{
				if (varnodename.Find("(OUT)")!=-1)
					results.push_back(varnodename);
			}
			else
				results.push_back(varnodename);
		}

		delete [] rc;

		return results.size();
	
	}

//private functions
void CASIObj::prepStream() //get the string list of stream component names
	{
		CString streamnodepath;
		int slength = nodePath.GetLength();
		streamnodepath= nodePath;
		streamnodepath.Insert(slength,".Input.FLOW.MIXED");
		
		IHNode node, cnode;

		node=nodeNav(ihRoot,  streamnodepath);
		
		if (node.m_lpDispatch==NULL)
			return;
		IHNodeCol ihcol;
		int d, i, j, total;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;

		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=node.GetElements();

		d = ihcol.GetDimension(); 

		rc = new long[d];
		total=0;
		for (i=0; i<d; i++) //for each dimention
		{
			rc[i] = ihcol.GetRowCount(i);
			total+=rc[i];
		}

//		int lastComa, lastDot;
		
		CString varnodename;

		//CString* results;

		//results= new CString[total];
		int cur_ind;
		for (i=0; i<total; i++)
		{
			cur_ind = i;
			for (j=0; j<d; j++)
			{
				arg[j].vt = VT_INT;
				V_INT(&arg[j])=cur_ind%rc[j];
				cur_ind = cur_ind/rc[j];
			}
			cnode = ihcol.GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
			varnodename = cnode.GetName(force);
			
			//lastComa=varnodename.ReverseFind(',');
			//varnodename=varnodename.Left(lastComa);
			//lastDot=varnodename.ReverseFind('(');
			//slength=varnodename.GetLength();
			//varnodename=varnodename.Right(slength-lastDot-1);

			streamComps.insert(varnodename);
		}

		return ;
	
	}

void CASIObj::prepBlock()
	{
		//////get inputs var names
		CString inputVarPath;
		int slength = nodePath.GetLength();

		inputVarPath=nodePath;
		inputVarPath.Insert(slength,".Input");

		IHNode node;

		node=nodeNav(ihRoot, inputVarPath);

		blockInputs.clear();
		getChildNames(node, blockInputs);

		///////get outputs var names

		CString outputVarPath;
		slength = nodePath.GetLength();

		outputVarPath=nodePath;
		outputVarPath.Insert(slength,".Output");

		node=nodeNav(ihRoot,  outputVarPath);

		blockOutputs.clear();
		getChildNames(node, blockOutputs);

		return;
	}

	int CASIObj::getFVNVariables()
	{
		CASI::Variable fvnvar =getInputVarByName("FVN_VARIABLE");
		int a;
		CString tt;
		for (a=0; a<82; a++)
		{
			tt=fvnvar.getVVVV(a);
		}
		
		return 0;
	}
} //namespace CASI