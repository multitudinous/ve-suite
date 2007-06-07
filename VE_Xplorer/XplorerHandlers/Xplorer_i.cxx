/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/Xplorer_i.h"

#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/XplorerHandlers/cfdDisplaySettings.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#ifdef _OSG
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"
#include "VE_Xplorer/TextureBased/cfdVolumeVisualization.h"
using namespace VE_TextureBased;
#endif

#include <vpr/System.h>
#include <vpr/Util/Debug.h>

#include <jccl/RTRC/ConfigManager.h>

using namespace VE_Xplorer;
using namespace VE_XML;
using namespace VE_SceneGraph;
using namespace VE_TextureBased;

////////////////////////////////////////////////////////////////////////////////
Body_VEXplorer_i::Body_VEXplorer_i (void)
:
m_xplorerAMIHandler()
{ 
   bufferCommand = new Command();
   bufferCommand->AddDataValuePair( new DataValuePair() );
   bufferCommand->SetCommandName( "wait" );
   isCluster = false;
}
////////////////////////////////////////////////////////////////////////////////
Body_VEXplorer_i::~Body_VEXplorer_i (void)
{
   if(bufferCommand)
   {
      delete bufferCommand;
      bufferCommand = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
char* Body_VEXplorer_i::GetStatusMessage( )
  ACE_THROW_SPEC (( ::CORBA::SystemException,::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetParams ( const char* param )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetID ( ::CORBA::Long id )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long Body_VEXplorer_i::GetID ( )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetName ( const char* name )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
char* Body_VEXplorer_i::GetName ( )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetNetwork ( const char* network )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetCommand ( const char* command )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  //When starting xplorer it is possible to connect and send a command before 
   // xplorer is ready to receive it
   while ( !jccl::ConfigManager::instance()->isPendingStale() )
   {
      vpr::System::msleep( 50 );  // 50 milli-second delay
   }
   
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);

   std::string commandString( command );
   if ( isCluster )
   {
      if ( mStates.isLocal()   )
      {
         commandStringQueue.push_back( commandString );
      }
   }

   vprDEBUG(vprDBG_ALL,2) <<"VjObs::SetCommandString(): "<< std::endl << commandString << std::endl << vprDEBUG_FLUSH;
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();
   networkWriter.ReadXMLData( commandString, "Command", "vecommand" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   for ( size_t i = 0; i < objectVector.size(); ++i )
   {
      commandVectorQueue.push_back( static_cast< VE_XML::Command* >( objectVector.at( i ) ) );
   }
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::RegisterUI ( const char* UIName, ::Body::UI_ptr ui )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
	std::cout << "Body_VEXplorer_i::RegisterUI Registering " << UIName << std::endl;
	uiCom[std::string(UIName)] = Body::UI::_duplicate( ui );
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::UnRegisterUI ( const char* UIName )
  ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ))
{
  // Add your implementation here
	throw CORBA::NO_IMPLEMENT();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::PreFrameUpdate( void )
{
   vpr::Guard<vpr::Mutex> val_guard(mValueLock);
 }
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::CreateCommandQueue( void )
{
   double newId = _bufferArray->GetCommandValue( cfdCommandArray::CFD_SC );
   double newPreState = _bufferArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   double newIsoValue = _bufferArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   
   //if we are doing transient vis then we already have an active model and dataset
   int activeVector = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetActiveVector();
   int activeScalar = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetActiveScalar();

   double activeMinMax[ 2 ];
   cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetRange( activeMinMax );

   std::map< int, cfdDataSet* >::iterator iter;
   
   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 0 );

   commandQueue.push_back( new cfdCommandArray() );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_ID, TRANSIENT_ACTIVE );
   commandQueue.back()->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::InitCluster( void )
{
   if ( !isCluster )
   {
      return;
   }

   // Cluster Stuff
   vpr::GUID new_guid("15c09c99-ed6d-4994-bbac-83587d4400d1");
   this->mStates.init(new_guid);

   //Initialize cluster variables
   this->mStates->clusterIso_value = 0.0f;
   this->mStates->clusterSc = 0.0f;
   this->mStates->clusterMin = 0.0f;
   this->mStates->clusterMax = 0.0f;
   this->mStates->clusterId = 0.0f;
   this->mStates->clusterGeo_state = 0.0f;
   this->mStates->clusterPostdata_state = 0.0f;
   this->mStates->clusterPre_state = false;
   this->mStates->clusterTimesteps = 0.0f;
   this->mStates->clusterTeacher_state = 0.0f; 
   this->mStates->clusterClientInfoFlag = 0; 
   this->mStates->currentFrame = 0;; // the index of the current frame
   this->mStates->clusterTime_since_start = 0;
   this->mStates->clusterFrameNumber = 0.0f;
   this->mStates->clusterQuatCamIncrement = 0.0f;
   
   for(int i=0;i<16;i++)
   {
      this->mStates->clusterMatrix[i] = 0.0f;
   }
   this->mStates->clusterMatrix[0] = 1.0f;
   this->mStates->clusterMatrix[5] = 1.0f;
   this->mStates->clusterMatrix[10] = 1.0f;
   this->mStates->clusterMatrix[15] = 1.0f;
   
   this->mStates->clusterXMLCommands.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::GetUpdateClusterStateVariables( void )
{
   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetUpdateClusterStateVariables Cluster Mode " 
                           << isCluster << std::endl << vprDEBUG_FLUSH;
   if ( !isCluster )
   {
      return;
   }
   
   {
      vpr::Guard<vpr::Mutex> val_guard(mValueLock);
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_ISO_VALUE, this->mStates->clusterIso_value );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_SC, this->mStates->clusterSc );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_MIN, this->mStates->clusterMin );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_MAX, this->mStates->clusterMax );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_ID, this->mStates->clusterId );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_GEO_STATE, this->mStates->clusterGeo_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE, this->mStates->clusterPostdata_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_PRE_STATE, this->mStates->clusterPre_state );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_TIMESTEPS, this->mStates->clusterTimesteps );
      _cfdArray->SetCommandValue( cfdCommandArray::CFD_TEACHER_STATE, this->mStates->clusterTeacher_state );
   }

   vprDEBUG(vprDBG_ALL,3) << "|\tVjObs_i::GetUpdateClusterStateVariables Node Local " 
                           << mStates.isLocal() << " " 
                           << vpr::System::getHostname() 
                           << std::endl << vprDEBUG_FLUSH;
   
   if ( mStates.isLocal() )
   {
      return;
   }

   //Do for all the slaves
   if ( this->mStates->clusterXMLCommands.size() > 0 )
   {  
      SetCommand( this->mStates->clusterXMLCommands.c_str() );  
   }

   //sync up the frames on all nodes in the
   //cluster
   {
      vpr::Guard<vpr::Mutex> val_guard(mValueLock);
      gmtl::Matrix44d matrix;

      for(int i=0;i<16;i++){
         matrix.mData[i]=this->mStates->clusterMatrix[i];
      }
      //std::cout << "slave: " << std::endl << matrix << std::endl;
      VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->SetMat( matrix );

      time_since_start = this->mStates->clusterTime_since_start;
#ifdef _OSG
      if ( cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode() )
      {
         cfdTextureBasedVizHandler::instance()->GetActiveVolumeVizNode()->SetCurrentTransientTexture( this->mStates->clusterFrameNumber);
      }
#endif
   }
}
////////////////////////////////////////////////////////////////////////////////
float Body_VEXplorer_i::GetSetAppTime( float x )
{
   if ( x == -1 )
   {
      return time_since_start;
   }
   else
   {
      time_since_start = x;
      return -1;
   }
}
////////////////////////////////////////////////////////////////////////////////
long Body_VEXplorer_i::GetSetFrameNumber( long x )
{
   if ( x == -1 )
   {
      return frameNumber;
   }
   else
   {
      frameNumber = x;
      return -1;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetClusterMode( bool clusterFlag )
{
   isCluster = clusterFlag;
}
////////////////////////////////////////////////////////////////////////////////
bool Body_VEXplorer_i::GetClusterMode( void )
{
   return isCluster;
}
////////////////////////////////////////////////////////////////////////////////
void Body_VEXplorer_i::SetXplorerData( std::string input )
{
    ///AMI call
    Body::AMI_UIHandler_var xplorerComAMIHandler = m_xplorerAMIHandler._this();
    std::map<std::string,Body::UI_var>::iterator currentConductor;
    for(currentConductor = uiCom.begin();
        currentConductor != uiCom.end();
        ++currentConductor)
    {
        
       currentConductor->second->sendc_SetXplorerData(xplorerComAMIHandler.in(), input.c_str());
    }
}
