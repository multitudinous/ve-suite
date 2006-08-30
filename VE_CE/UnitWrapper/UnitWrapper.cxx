#include "UnitWrapper.h"

#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Command.h"

#include <sstream>

// Implementation skeleton constructor
UnitWrapper::UnitWrapper (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
{
  UnitName_=name;
  return_state = 0;
}
////////////////////////////////////////////////////////////////////////////////
// Implementation skeleton destructor
UnitWrapper::~UnitWrapper (void)
{
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StartCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StopCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );

}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::PauseCalc (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::Resume (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	std::string msg;
    msg = UnitName_+" : Instant calculation, already finished\n";
    executive_->SetModuleMessage( activeId, msg.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetStatusMessage (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	VE_XML::Command returnState;

	returnState.SetCommandName("statusmessage");
	VE_XML::DataValuePair* data=returnState.GetDataValuePair(-1);
	data->SetDataName("RETURN_STATE");
	data->SetDataType("UNSIGNED INT");
	data->SetDataValue(return_state);
	
	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;

	nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ) 
                     );
	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return CORBA::string_dup(status.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetUserData (
    
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
void UnitWrapper::SetParams ( ::CORBA::Long id, const char * param )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( param, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   for (size_t i=0; i<objectVector.size(); i++)
   {
		VE_XML::Command* params = dynamic_cast< VE_XML::Command* >( objectVector.at( i ) );
		unsigned int num = params->GetNumberOfDataValuePairs();
		//for (j=0; j<num; j++)
		//{
			VE_XML::DataValuePair* curPair= params->GetDataValuePair("NodePath");
			//CString nodepath = curPair->GetDataString().c_str();
			curPair = params->GetDataValuePair("Value");
			//CString nodevalue = curPair->GetDataString().c_str();

			//CASI::Variable cur_var=bkp.aspendoc.getVarByNodePath(nodepath);
			//cur_var.setValue(nodevalue);
			
		//}
   }
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetID (
    ::CORBA::Long id
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	 //id_.length( id_.length() + 1 );
    //id_[ id_.length() - 1 ] = id;
    
	 std::cout<<UnitName_<<" :SetID called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetCurID (
                            ::CORBA::Long id
                            )
ACE_THROW_SPEC ((
                 ::CORBA::SystemException,
                 ::Error::EUnknown
                 ))
{
   activeId = id;
}
////////////////////////////////////////////////////////////////////////////////
::Types::ArrayLong* UnitWrapper::GetID (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
	std::cout<<UnitName_<<" :GetID called"<<std::endl;
    return id_.out() ;
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long UnitWrapper::GetCurID (
                                     
                                     )
ACE_THROW_SPEC ((
                 ::CORBA::SystemException,
                 ::Error::EUnknown
                 ))
{
   // Add your implementation here
   return activeId;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetName (
    const char * name
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
  // Add your implementation here
	UnitName_ = std::string(name);
    std::cout<<UnitName_<<" :SetName called"<<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetName (
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
    std::cout<<UnitName_<<" :GetName called"<<std::endl;
    return CORBA::string_dup(UnitName_.c_str());
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::Query ( const char* command
    
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{

   //if request is get inputs
   //if request is set inputs
   //if request is get results
   //if request is get port data
   //if request is set port data
   //if request is 
	bool firsttime=true;

	std::string filename="Hyper.bkp";
	if (firsttime)
	{
		//bkp.openFile(filename.c_str());
		firsttime=false;
	}
	//std::string network = bkp.CreateNetwork();
   std::string network;
	return CORBA::string_dup(network.c_str());
	
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::DeleteModuleInstance( ::CORBA::Long module_id )
   ACE_THROW_SPEC(( ::CORBA::SystemException, ::Error::EUnknown ))
{
   std::ostringstream strm;
   strm << module_id;
   
   std::map< std::string, VE_Model::Model* >::iterator iter;
   iter = xmlModelMap.find( strm.str() );
   if ( iter != xmlModelMap.end() )
   {
      delete iter->second;
      xmlModelMap.erase( iter );
   }
}
