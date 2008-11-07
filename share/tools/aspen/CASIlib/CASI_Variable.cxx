#pragma warning (disable: 4786)
//#include "stdafx.h"
#include "CASI.h"

namespace CASI
{
    Variable::Variable(Happ::IHNodePtr root, CString nodepath, VARTYPE nodeType)
		:ihRoot(root), nodePath(nodepath), nodeType(nodeType), aliasName("")
	{
		
	}

	VARTYPE Variable::getNodeType() //Block Node and Stream Node get special treatment
	{
		return nodeType;
	}

	CString Variable::getNodePath()//the Full Path to the node
	{
		return nodePath;
	}

	CString Variable::getAliasName() //the variable name user associated with
	{
		return aliasName;
	}

	bool Variable::valid()
	{
		if (nodePath!="")
			return true;
		else
			return false;
	}

	CString Variable::getName() //the variable explorer name of the variable, which is the lastpart of the full path
	{
		int lastDot=nodePath.ReverseFind('.');

		int slength=nodePath.GetLength();

		CString result = nodePath;

		return result.Right(slength-lastDot-1);
		
	}

	int Variable::getNumChild() //how many child this node have
	{
		Happ::IHNodePtr root;

		root = nodeNav(ihRoot, nodePath);
		return getChildNum(root);
	}

	Variable Variable::getChild(int ind) //return a pointer to the indexed child
	{
		
		Happ::IHNodeColPtr ihcol;
		Happ::IHNodePtr root, cnode;
		int d, i, j;
		long* rc;
		VARIANTARG arg[5];
		VARIANTARG force;
		int index = ind;
		CString cnodepath;

		root = nodeNav(ihRoot, nodePath);
		//if (root.m_lpDispatch==NULL)
		if (root==NULL)
			return Variable(ihRoot, _T(""));
		

		for (i=0; i<5; i++) 
			::VariantInit(&arg[i]);
		::VariantInit(&force);

		ihcol=root->GetElements();

		d = ihcol->GetDimension(); 
		
		rc = new long[d];
		
		for (i=0; i<d; i++) //for each dimention
			rc[i] = ihcol->GetRowCount(i);
		
		for (j=0; j<d; j++)
		{
			arg[j].vt = VT_INT;
			V_INT(&arg[j])=index%rc[j];
			index = index/rc[j];
		}

		cnode = ihcol->GetItem(arg[0], arg[1], arg[2], arg[3], arg[4]);
		
        cnodepath = cnode->GetName(force).GetBSTR();
		cnodepath.Insert(0,_T("."));
		cnodepath.Insert(0,LPCTSTR(nodePath));
		
		Variable result(ihRoot, cnodepath);

		delete rc;

		return result;			
	}
		
	CString Variable::getValue() //The string display in the value entry box;
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");

		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		//val=node->GetValue(force);
		
		//val=node->GetAttributeValue(HAP_VALUE, force);
		if (node->GetHasAttribute(Happ::HAP_VALUE))
		{
			val=node->GetAttributeValue(Happ::HAP_VALUE, force);
			return variantToString(val);	
		}

		return CString("NOATTR");

		//return variantToString(val);
	}
	
	CString Variable::getPhysicalQuantity() //The string display in the PhysicalQuantity box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");

		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);
		
        if (node->GetHasAttribute(Happ::HAP_UNITROW))
		{
			val=node->GetAttributeValue(Happ::HAP_UNITROW, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::getUnitOfMeasure() //The string displayed in the Unit of Measure box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");

		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);
		
		if (node->GetHasAttribute(Happ::HAP_UNITCOL))
		{
			val=node->GetAttributeValue(Happ::HAP_UNITCOL, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::getBasis() //The string displayed in the Basis box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);
		
		if (node->GetHasAttribute(Happ::HAP_BASIS))
		{
			val=node->GetAttributeValue(Happ::HAP_BASIS, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::getOptionList() //The string displayed in the Option List Box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_OPTIONLIST))
		{
			val=node->GetAttributeValue(Happ::HAP_OPTIONLIST, force);
			return variantToString(val);
		}

		return CString("NOATTR");
	}
	
	CString Variable::getOptions() //The string displayed in the Option Box, deliminate by /n for each option
	{
		return CString("NOT AVAIL");
	}
	
	CString Variable::getRecordType() //The string displayed in the Record Type Box;
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_RECORDTYPE))
		{
			val=node->GetAttributeValue(Happ::HAP_RECORDTYPE, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
		
	}

    long Variable::getDimension()
    {
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T(0);
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		//if (node->GetHasAttribute(HAP_RECORDTYPE))
		//{
		//	val=node->GetAttributeValue(HAP_RECORDTYPE, force);
		//	return variantToString(val);	
		//}
		//return CString("NOATTR");
        return node->GetDimension();
    }

	CString Variable::isOutput() //The string in the Output Box, 1 means true, 0 means false
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_OUTVAR))
		{
			val=node->GetAttributeValue(Happ::HAP_OUTVAR, force);
			return variantToString(val);
			
		}

		return CString("NOATTR");
		
		
	}

	CString Variable::isEnterable() //The string in the Enterable Box, 1 means true, 0 means false
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);
		
		if (node->GetHasAttribute(Happ::HAP_ENTERABLE))
		{
			val=node->GetAttributeValue(Happ::HAP_ENTERABLE, force);
			return variantToString(val);
		}

		return CString("NOATTR");
		
	}

	CString Variable::hasChild() //The string in the HasChild Box, 1 means true, 0 means false
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_HASCHILDREN))
		{
			val=node->GetAttributeValue(Happ::HAP_HASCHILDREN, force);
			return variantToString(val);
		}

		return CString("NOATTR");
		
	}

	CString Variable::upLimit() //the up Limit of the value
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_UPPERLIMIT))
		{
			val=node->GetAttributeValue(Happ::HAP_UPPERLIMIT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::lowerLimit() //the lower limit of the value
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_LOWERLIMIT))
		{
			val=node->GetAttributeValue(Happ::HAP_LOWERLIMIT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::getDefaultValue() //the default value
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_VALUEDEFAULT))
		{
			val=node->GetAttributeValue(Happ::HAP_VALUEDEFAULT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");

	}
	
	CString Variable::getPrompt() //the prompt box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_PROMPT))
		{
			val=node->GetAttributeValue(Happ::HAP_PROMPT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
		
	}
	
	CString Variable::getCompletionStatus() //the completion status box, deliminated by \n, the string leading with& is the first one
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_COMPSTATUS))
		{
			val=node->GetAttributeValue(Happ::HAP_COMPSTATUS, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}
	
	CString Variable::getInorOut() //the In or Out box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_INOUT))
		{
			val=node->GetAttributeValue(Happ::HAP_INOUT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}

	CString Variable::getGender() //the Gender box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_PORTSEX))
		{
			val=node->GetAttributeValue(Happ::HAP_PORTSEX, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
		
	}
	
	CString Variable::getMultiport() //the multiport box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		if (node==NULL)
		//if (node->m_lpDispatch==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_MULTIPORT))
		{
			val=node->GetAttributeValue(Happ::HAP_MULTIPORT, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	}

	CString Variable::getPortType() //the port type box
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(Happ::HAP_PORTTYPE))
		{
			val=node->GetAttributeValue(Happ::HAP_PORTTYPE, force);
			return variantToString(val);	
		}

		return CString("NOATTR");

	}


	void Variable::setAliasName(CString aname) //the variable name user associated with
	{
		aliasName=CString(aname);
	}
	
	bool Variable::setValue(CString value) //The value field is writable. And at least in GUI, set value could fail;
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return false;
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);
		
		val= node->GetAttributeValue(Happ::HAP_VALUE, force); //so the val will have the correct type for this variant;
		stringToVariant(value, val);

		//node->SetAttributeValue(Happ::HAP_VALUE, force, val);
		node->PutAttributeValue(Happ::HAP_VALUE, force, val);
				
		return true;
	}

	int Variable::getValues(CString* values)
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return false;
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		return 0;
	}

	CString Variable::getVVVV(int index)
	{
		Happ::IHNodePtr node;

		node=nodeNav(ihRoot,  nodePath);
		//if (node->m_lpDispatch==NULL)
		if (node==NULL)
			return _T("NOATTR");
		VARIANT val;
		VARIANTARG force;

		::VariantInit(&val);
		::VariantInit(&force);

		if (node->GetHasAttribute(index))
		{
			val=node->GetAttributeValue(index, force);
			return variantToString(val);	
		}

		return CString("NOATTR");
	
	}
} //namespace CASI
