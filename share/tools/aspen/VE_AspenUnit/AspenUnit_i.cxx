// -*- C++ -*-
//
// $Id$

// ****  Code generated by the The ACE ORB (TAO) IDL Compiler ****
// TAO and the TAO IDL Compiler have been developed by:
//       Center for Distributed Object Computing
//       Washington University
//       St. Louis, MO
//       USA
//       http://www.cs.wustl.edu/~schmidt/doc-center.html
// and
//       Distributed Object Computing Laboratory
//       University of California at Irvine
//       Irvine, CA
//       USA
//       http://doc.ece.uci.edu/
// and
//       Institute for Software Integrated Systems
//       Vanderbilt University
//       Nashville, TN
//       USA
//       http://www.isis.vanderbilt.edu/
//
// Information about TAO is available at:
//     http://www.cs.wustl.edu/~schmidt/TAO.html

// TAO_IDL - Generated from 
// .\be\be_codegen.cpp:1063

#include "stdafx.h"
#include "AspenUnit_i.h"
#include "VE_AspenUnit.h"
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <fstream>
#include <iostream>

// Implementation skeleton constructor
Body_Unit_i::Body_Unit_i( /*Body::Executive_ptr exec,*/ std::string name, 
    /*BKPParser* parser,*/ CVE_AspenUnitDlg * dialog, 
    CorbaUnitManager* parent, std::string dir )
    :
    bkp( new BKPParser() ),
    AspenLog( 0 ),
    theParent( parent ),
    theDialog( dialog ),
    return_state( 0 ),
    cur_id_( 0 ),
    UnitName_( name ),
    workingDir( dir )
{
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "XML",new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "Shader",new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "Model",new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "CAD",new ves::open::xml::cad::CADCreator() );

	bkp->SetWorkingDir( workingDir );

    AspenLog = reinterpret_cast<CEdit *>(theDialog->GetDlgItem(IDC_EDIT1));

    mQueryCommandNames.insert( "getNetwork");
    mQueryCommandNames.insert( "openSimulation");
    mQueryCommandNames.insert( "runNetwork");
	mQueryCommandNames.insert( "reinitNetwork");
    mQueryCommandNames.insert( "stepNetwork");
    mQueryCommandNames.insert( "showSimulation");
    mQueryCommandNames.insert( "hideSimulation");
    mQueryCommandNames.insert( "closeSimulation");
    mQueryCommandNames.insert( "saveSimulation");
    mQueryCommandNames.insert( "saveAsSimulation");
    mQueryCommandNames.insert( "getInputModuleParamList");
    mQueryCommandNames.insert( "getInputModuleProperties");
    mQueryCommandNames.insert( "getOutputModuleParamList");
    mQueryCommandNames.insert( "getOutputModuleProperties");
    mQueryCommandNames.insert( "getStreamInputModuleParamList");
    mQueryCommandNames.insert( "getStreamInputModuleProperties");
    mQueryCommandNames.insert( "getStreamOutputModuleParamList");
    mQueryCommandNames.insert( "getStreamOutputModuleProperties");
    mQueryCommandNames.insert( "setParam");
}
////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton destructor
Body_Unit_i::~Body_Unit_i( void )
{
    delete bkp;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::ShowAspen()
{
	bkp->showAspen(true);
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::HideAspen()
{
	bkp->showAspen(false);
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::CloseAspen()
{
	bkp->closeFile();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SaveAspen()
{
	bkp->saveFile();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StepSim()
{
	bkp->step();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::ReinitializeAspen()
{
	bkp->ReinitAspen();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StartCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	//executive_->SetModuleMessage(cur_id_,"Simulation running...\n");
	bkp->aspendoc->runSolver(false);
	AspenLog->SetSel(-1, -1);
	AspenLog->ReplaceSel("Simulation Complete\r\n");
	//executive_->SetModuleMessage(cur_id_,"Simulation completed.\n");
	return_state=0;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StopCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    //executive_->SetModuleMessage(cur_id_,msg.c_str());

}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::PauseCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    //executive_->SetModuleMessage(cur_id_,msg.c_str());
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::Resume (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    //executive_->SetModuleMessage(cur_id_,msg.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::GetStatusMessage (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );

	returnState->SetCommandName("statusmessage");
	ves::open::xml::DataValuePairPtr data = returnState->GetDataValuePair(-1);
	data->SetDataName("RETURN_STATE");
	data->SetDataType("UNSIGNED INT");
	data->SetDataValue(return_state);
	
	std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;

	nodes.push_back( 
                  std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "Command" ) 
                     );
	ves::open::xml::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "vecommand" );
    return CORBA::string_dup(status.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::GetUserData (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	char * result = 0;
	return result;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetID (
    ::CORBA::Long id
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
  // no need to implement this
  std::cout << "This not implemented for the Aspen Unit" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetCurID (
    ::CORBA::Long id
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
    cur_id_ = id;
}
////////////////////////////////////////////////////////////////////////////////
::Types::ArrayLong* Body_Unit_i::GetID (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
	return &ids_;
}
////////////////////////////////////////////////////////////////////////////////
CORBA::Long Body_Unit_i::GetCurID (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
    return cur_id_;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::DeleteModuleInstance(CORBA::Long id) 
ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
	return; //do nothing;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetName (
    const char * name
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	UnitName_ = std::string(name);
}
////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::GetName (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
    return CORBA::string_dup(UnitName_.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * Body_Unit_i::Query ( const char * query_str
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
	ves::open::xml::XMLReaderWriter networkWriter;
	networkWriter.UseStandaloneDOMDocumentManager();
	networkWriter.ReadFromString();
	networkWriter.ReadXMLData( query_str, "Command", "vecommand" );
	//std::ofstream packet("packet.txt");
	//packet << query_str;
	//packet.close();
	std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

	ves::open::xml::CommandPtr cmd;	
	std::string cmdname;
	
	cmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
	cmdname = cmd->GetCommandName();
    
    std::set< std::string >::const_iterator commandItr
        = mQueryCommandNames.find( cmdname );
    
    //If the command is not processed here then do not bother doing anything more
    if( commandItr == mQueryCommandNames.end() )
    {
        return CORBA::string_dup("NULL");
    }
    
	AspenLog->SetSel(-1, -1);
	AspenLog->ReplaceSel(("Command: "+cmdname+"\r\n").c_str());
	char* returnValue = "empty";

	if (cmdname=="getNetwork")
	{
		//bkp = theParent->CreateParser();
		//theParent->CreateParser();
		//bkp = new BKPParser();
		returnValue = handleGetNetwork( cmd );
		return returnValue;
	}
	else if(cmdname=="openSimulation")
	{
		returnValue = handleOpenSimulation(cmd);
		return returnValue;
	}
	else if (cmdname=="runNetwork")
	{
		StartCalc();
		return CORBA::string_dup("networkRun");
	}
	else if (cmdname=="stepNetwork")
	{
		StepSim();
		return CORBA::string_dup("networkRun");
	}
	else if (cmdname=="showSimulation")
	{
		ShowAspen();
		return CORBA::string_dup("Simulation Shown.");
	}
	else if (cmdname=="hideSimulation")
	{
		HideAspen();
		return CORBA::string_dup("Simulation hidden.");
	}
	else if (cmdname=="closeSimulation")
	{
		AspenLog->SetSel(-1, -1);
		AspenLog->ReplaceSel("closing...\r\n");
		CloseAspen();
		AspenLog->SetSel(-1, -1);
		AspenLog->ReplaceSel("closed.\r\n");
		return CORBA::string_dup("Simulation closed.");
	}
	else if (cmdname=="saveSimulation")
	{
		AspenLog->SetSel(-1, -1);
		AspenLog->ReplaceSel("saving...\r\n");
		try
		{
			SaveAspen();
		    AspenLog->SetSel(-1, -1);
		    AspenLog->ReplaceSel("saved.\r\n");
		}
		catch(...)
		{
			AspenLog->SetSel(-1, -1);
			AspenLog->ReplaceSel("messed up save.\r\n");
		}
		return CORBA::string_dup("Simulation Saved.");
	}
	else if (cmdname=="saveAsSimulation")
	{
		returnValue = handleSaveAs(cmd);
		return returnValue;
	}
	else if( cmdname == "reinitNetwork" )
	{
		ReinitializeAspen();
		return CORBA::string_dup( "Simulation reinitialized." );
	}

	//Blocks
	else if (cmdname=="getInputModuleParamList")
	{
		//executive_->SetModuleMessage(cur_id_,"Querying inputs...\n");
		returnValue = handleGetInputModuleParamList(cmd);
		//executive_->SetModuleMessage(cur_id_,"Querying completed.\n");
		return returnValue;
	}
	else if (cmdname=="getInputModuleProperties")
	{
		returnValue = handleGetInputModuleProperties(cmd);
		return returnValue;
	}
	else if (cmdname=="getOutputModuleParamList")
	{
		//executive_->SetModuleMessage(cur_id_,"Querying outputs...\n");
		returnValue = handleGetOutputModuleParamList(cmd);
		//executive_->SetModuleMessage(cur_id_,"Querying completed.\n");
		return returnValue;
	}
	else if (cmdname=="getOutputModuleProperties")
	{
		returnValue = handleGetOutputModuleProperties(cmd);
		return returnValue;
	}

	//Streams
	else if (cmdname=="getStreamInputModuleParamList")
	{
		//executive_->SetModuleMessage(cur_id_,"Querying link inputs...\n");
		returnValue = handleGetStreamInputModuleParamList(cmd);
		//executive_->SetModuleMessage(cur_id_,"Querying link completed.\n");
		return returnValue;
	}
	else if (cmdname=="getStreamInputModuleProperties")
	{
		returnValue = handleGetStreamInputModuleProperties(cmd);
		return returnValue;
	}
	else if (cmdname=="getStreamOutputModuleParamList")
	{
		//executive_->SetModuleMessage(cur_id_,"Querying link outputs...\n");
		returnValue = handleGetStreamOutputModuleParamList(cmd);
		//executive_->SetModuleMessage(cur_id_,"Querying link completed.\n");
		return returnValue;
	}
	else if (cmdname=="getStreamOutputModuleProperties")
	{
		returnValue = handleGetStreamOutputModuleProperties(cmd);
		return returnValue;
	}

	//Params
	else if (cmdname=="setParam")
	{
		SetParam(cmd);
		return CORBA::string_dup("Param Set");
	}
	else
		return CORBA::string_dup("NULL");
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetNetwork(ves::open::xml::CommandPtr cmd)
{
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(theDialog->GetDlgItem(IDC_EDIT2));

	//this command has no params
	bool firsttime=true;

	std::string filename = cmd->GetDataValuePair(1)->GetDataString();
	if (firsttime)
	{
        //make sure bkp file exists
	    std::ifstream bkpFile( ( workingDir + filename + ".bkp" ).c_str() , std::ios::binary);
        if( !bkpFile.is_open() )
        {
            //no bkp file
		    AspenLog->SetSel(-1, -1);
		    AspenLog->ReplaceSel("BKP File Does NOT exist.\r\n");
	        return CORBA::string_dup( "BKPDNE" );
        }
        bkpFile.close();

        //make sure apw file exists
        std::ifstream apwFile( ( workingDir + filename + ".apw" ).c_str() , std::ios::binary);
        if( !apwFile.is_open() )
        {
            //no apw file
		    AspenLog->SetSel(-1, -1);
		    AspenLog->ReplaceSel("APW File Does NOT exist.\r\n");
	        return CORBA::string_dup( "APWDNE" );
        }
        apwFile.close();

		//Display->SetWindowText( ( workingDir + filename ).c_str());
		Display->SetWindowText( ( filename ).c_str());
        //go through bkp parsing procedure
		bkp->openFile(filename.c_str());
		firsttime=false;
	}
	   
    //Display->SetWindowText(filename.c_str());

	std::string network;
	try
    {
        network = bkp->CreateNetwork();
	}
	catch(...)
	{
		std::cout << "GetNetwork Exception Aspen Unit" << std::endl;
        return NULL;
	}
	//std::ofstream output("returnString.txt");
	//output<<"Return String"<<std::endl;
	//output<<network<<std::endl;
	//output.close();   
	
	//Display = reinterpret_cast<CEdit *>(theDialog->GetDlgItem(IDC_EDIT1));
    //Display->set->SetWindowText(network.c_str());

		//AspenLog->SetSel(-1, -1);
		//AspenLog->ReplaceSel(network.c_str());
	return CORBA::string_dup(network.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleOpenSimulation(ves::open::xml::CommandPtr cmd)
{
    CEdit *Display;
    Display = reinterpret_cast<CEdit *>(theDialog->GetDlgItem(IDC_EDIT2));

	//this command has no params
	std::string filename = cmd->GetDataValuePair(1)->GetDataString();
	Display->SetWindowText(filename.c_str());
	bkp->openFile(filename.c_str());
	return CORBA::string_dup("Simulation Opened.");
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleSaveAs(ves::open::xml::CommandPtr cmd)
{
	AspenLog->SetSel(-1, -1);
	AspenLog->ReplaceSel("saving...\r\n");
	std::string filename = cmd->GetDataValuePair(1)->GetDataString();
	bkp->saveAs(filename.c_str());
	AspenLog->SetSel(-1, -1);
	AspenLog->ReplaceSel("saved.\r\n");
	return CORBA::string_dup("Simulation Saved.");
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetInputModuleParamList(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname;
	unsigned int modId;

	for( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="ModuleId")
			curPair->GetData(modId);
	}

	//There shouldn't be two intances of an Aspen framework. so discard the moduleId
	//the returned string will be a well formated XML within "vecommand" element
	std::string netPak = bkp->GetInputModuleParams(modname);

    //std::ofstream output("inputList.txt");
    //output<<netPak;
    //output.close();  

	return CORBA::string_dup(netPak.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetInputModuleProperties(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname,paramName;
	unsigned int modId;

	for ( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="moduleId")
			curPair->GetData(modId); //modId is discarded because of the same reason as before
		else if (curPair->GetDataName()=="ParamName")
			paramName=curPair->GetDataString();
	}
	std::string netPak = bkp->GetInputModuleParamProperties(modname, paramName);
	return CORBA::string_dup(netPak.c_str());

}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetOutputModuleParamList(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname;
	unsigned int modId;

	for( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="ModuleId")
			curPair->GetData(modId);
	}

	//There shouldn't be two intances of an Aspen framework. so discard the moduleId
	//the returned string will be a well formated XML within "vecommand" element
	std::string netPak = bkp->GetOutputModuleParams(modname);
	return CORBA::string_dup(netPak.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetOutputModuleProperties(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname,paramName;
	unsigned int modId;

	for ( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="moduleId")
			curPair->GetData(modId); //modId is discarded because of the same reason as before
		else if (curPair->GetDataName()=="ParamName")
			paramName=curPair->GetDataString();
	}
	std::string netPak = bkp->GetOutputModuleParamProperties(modname, paramName);
	return CORBA::string_dup(netPak.c_str());

}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetStreamInputModuleParamList(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname;
	unsigned int modId;

	for ( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="ModuleId")
			curPair->GetData(modId);
	}

	//There shouldn't be two intances of an Aspen framework. so discard the moduleId
	//the returned string will be a well formated XML within "vecommand" element
	std::string netPak = bkp->GetStreamInputModuleParams(modname);
	return CORBA::string_dup(netPak.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetStreamInputModuleProperties(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname,paramName;
	unsigned int modId;

	for ( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="moduleId")
			curPair->GetData(modId); //modId is discarded because of the same reason as before
		else if (curPair->GetDataName()=="ParamName")
			paramName=curPair->GetDataString();
	}
	std::string netPak = bkp->GetStreamInputModuleParamProperties(modname, paramName);
	return CORBA::string_dup(netPak.c_str());

}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetStreamOutputModuleParamList(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname;
	unsigned int modId;

	for( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="ModuleId")
			curPair->GetData(modId);
	}

	//There shouldn't be two intances of an Aspen framework. so discard the moduleId
	//the returned string will be a well formated XML within "vecommand" element
	std::string netPak = bkp->GetStreamOutputModuleParams(modname);
	return CORBA::string_dup(netPak.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::handleGetStreamOutputModuleProperties(ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname,paramName;
	unsigned int modId;

	for( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="moduleId")
			curPair->GetData(modId); //modId is discarded because of the same reason as before
		else if (curPair->GetDataName()=="ParamName")
			paramName=curPair->GetDataString();
	}
	std::string netPak = bkp->GetStreamOutputModuleParamProperties(modname, paramName);
	return CORBA::string_dup(netPak.c_str());

}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetParams (CORBA::Long id,
    const char * param)
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   //discard the id, it is not used;
   ves::open::xml::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( param, "Command", "vecommand" );
   std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();
  
   //this part would need rewrite later
   for( size_t i=0; i<objectVector.size(); i++)
   {
		ves::open::xml::CommandPtr param = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( i ) );
		std::string paramName = param->GetCommandName();
		
		size_t num = param->GetNumberOfDataValuePairs();
		for (size_t j=0; j<num; j++)
		{
			ves::open::xml::DataValuePairPtr curPair= param->GetDataValuePair("NodePath");
			CString nodepath = curPair->GetDataString().c_str();
			curPair = param->GetDataValuePair("Value");
			CString nodevalue = curPair->GetDataString().c_str();

			CASI::Variable cur_var=bkp->aspendoc->getVarByNodePath(nodepath);
			cur_var.setValue(nodevalue);
			
		}
   }
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetParam (ves::open::xml::CommandPtr cmd)
{
	size_t num = cmd->GetNumberOfDataValuePairs();
	std::string modname,paramName, paramValue;

	for( size_t i=0; i < num; i++)
	{
		ves::open::xml::DataValuePairPtr curPair= cmd->GetDataValuePair(i);
		if (curPair->GetDataName()=="ModuleName")
			modname=curPair->GetDataString();
		else if (curPair->GetDataName()=="ParamName")
			paramName=curPair->GetDataString();
		else if (curPair->GetDataName()=="ParamValue")
			paramValue=curPair->GetDataString();
	}
	
	CASI::CASIObj cur_block = bkp->aspendoc->getBlockByName(modname.c_str());
	CASI::Variable tempvar = cur_block.getInputVarByName(paramName.c_str());
	CASI::Variable cur_var = bkp->aspendoc->getVarByNodePath(tempvar.getNodePath());
	CString newValue;
	newValue = paramValue.c_str();
	bool success = cur_var.setValue(newValue);
}
////////////////////////////////////////////////////////////////////////////////
