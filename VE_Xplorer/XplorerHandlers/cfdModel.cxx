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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdSound.h"
#include "VE_Xplorer/XplorerHandlers/ModelCADHandler.h"

#include "VE_Xplorer/SceneGraph/Utilities/Attribute.h"
#include "VE_Xplorer/SceneGraph/Clone.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"

#include "VE_Xplorer/SceneGraph/Utilities/OpacityVisitor.h"

#include "VE_Xplorer/Utilities/cfdGrid2Surface.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLCreator.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADCreator.h"

#include "VE_Open/XML/Shader/ShaderCreator.h"

#ifdef _OSG
#include <osg/StateSet>
#include "VE_Xplorer/TextureBased/cfdTextureDataSet.h"
using namespace VE_TextureBased;
#endif

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>
//#include <vpr/vprTypes.h>

#include <vtkUnstructuredGrid.h>
#include <vtkDataWriter.h>
#include <vtkZLibDataCompressor.h>
#include <vtkUnsignedCharArray.h>
#include <vtkDataSet.h>
#include <vtkTriangleFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkSTLWriter.h>
#include <vtkPolyData.h>
#include <vtkUnstructuredGridReader.h>

#include <osg/BlendFunc>

#include <fstream>
#include <sstream>

#include <boost/bind.hpp>

#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

////////////////////////////////////////////////////////////////////////////////
cfdModel::cfdModel( VE_SceneGraph::DCS* worldDCS )
{
   vprDEBUG(vesDBG,1) << "|\tNew cfdModel ! " 
                          << std::endl << vprDEBUG_FLUSH;
   this->mModelNode = 0;
   //this->actor = NULL;
   //ModelIndex = static_cast<ModelTypeIndex>(value);
   // Will fix this later so that each model has a dcs
   //mModelDCS = new VE_SceneGraph::DCS();
   _worldDCS = worldDCS;
   m_cadHandler = new VE_Xplorer::ModelCADHandler( _worldDCS.get() );
   //mirrorNode = 0;
   //mirrorGroupNode = 0;

   //this->animation = 0;
   this->activeDataSet = 0;
   mirrorDataFlag = false;
#ifdef _OSG
   _activeTextureDataSet = 0;
#endif
   modelID = 10000000;
   
}
////////////////////////////////////////////////////////////////////////////////
cfdModel::~cfdModel()
{
   vprDEBUG(vesDBG,2) << "cfdModel destructor"
                          << std::endl << vprDEBUG_FLUSH;

 
  /* for ( std::map<std::string ,VE_EVENTS::EventHandler*>::iterator itr = _eventHandlers.begin();
                                       itr != _eventHandlers.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _eventHandlers.clear();*/
   // the following block allows the program to get to pfExit
   /*for ( VTKDataSetList::iterator itr = mVTKDataSets.begin();
                                  itr != mVTKDataSets.end(); itr++ )
   {
      vprDEBUG(vesDBG,2) << "deleting a cfdModel"
                             << std::endl << vprDEBUG_FLUSH;
      delete *itr;
   }*/
   size_t dataSetSize = mVTKDataSets.size();
   for ( size_t i = 0; i < dataSetSize; i++)
   {
      delete mVTKDataSets.at(i);
   }
   mVTKDataSets.clear();
   vprDEBUG(vesDBG,2) << "deleting mVTKDataSets"
      << std::endl << vprDEBUG_FLUSH;

   //texture data cleanup
#ifdef _OSG
   /*TextureDataSetList::iterator tDataSet;
   for ( tDataSet=mTextureDataSets.begin(); tDataSet!=mTextureDataSets.end();tDataSet++ )
   {
         delete *tDataSet;
   }*/
   for(unsigned int i = 0; i < mTextureDataSets.size(); i++)
   {
      delete mTextureDataSets.at(i);
   }
   mTextureDataSets.clear();
   vprDEBUG(vesDBG,2) << "deleting mTextureDataSets"
      << std::endl << vprDEBUG_FLUSH;
#endif
 
   //std::map<int,cfdDataSet*>::iterator foundPlugin;
   // Remove any plugins that aren't present in the current network
   /*for ( foundPlugin=transientDataSets.begin(); foundPlugin!=transientDataSets.end(); )
   {
         // When we clear the _plugin map will
         // loop over all plugins
         transientDataSets.erase( foundPlugin++ );
   }
   transientDataSets.clear();
   */
/*
   // The following block is broken
   // It loops more than it should (ie, twice for a single dataset)
   // and seg faults trying to erase something that is not there
   for ( VTKDataSetList::iterator itr = mVTKDataSets.begin();
                                  itr != mVTKDataSets.end(); )
   {
      vprDEBUG(vesDBG,2) << "erasing a cfdModel"
                             << std::endl << vprDEBUG_FLUSH;
      mVTKDataSets.erase( itr++ );
   }
*/

   vprDEBUG(vesDBG,2) << "cfdModel destructor finished"
                          << std::endl << vprDEBUG_FLUSH;
   _availableSounds.clear();

   if( m_cadHandler)
   {
       delete m_cadHandler;
       m_cadHandler = 0;
   }
}
/////////////////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::ModelCADHandler* cfdModel::GetModelCADHandler()
{
    return m_cadHandler;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::PreFrameUpdate()
{
   vprDEBUG(vesDBG,1) << "cfdModel::PreFrameUpdate " <<std::endl<< vprDEBUG_FLUSH;;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::CreateCfdDataSet( void )
{
   mVTKDataSets.push_back( new cfdDataSet() );
}

////////////////////////////////////////////////////////////////////////////////
void cfdModel::SetMirrorNode( VE_SceneGraph::Group* dataNode )
{
   if ( !mirrorNode )
   {
      mirrorNode = new VE_SceneGraph::Clone();
		mirrorNode->CloneNode( GetActiveDataSet()->GetDCS() ); 
      double rot[ 3 ];
      rot[ 0 ] = 180.0f;
      rot[ 1 ] = 0.0f;
      rot[ 2 ] = 0.0f;
      mirrorNode->SetRotationArray( rot );
      this->_worldDCS->AddChild( mirrorNode->GetClonedGraph() );
   }
   else
   {
      this->_worldDCS->RemoveChild( mirrorNode->GetClonedGraph() );
      delete mirrorNode;     
      mirrorNode = new VE_SceneGraph::Clone( GetActiveDataSet()->GetDCS() );
      double rot[ 3 ];
      rot[ 0 ] = 180.0f;
      rot[ 1 ] = 0.0f;
      rot[ 2 ] = 0.0f;
      mirrorNode->SetRotationArray( rot );
      this->_worldDCS->AddChild( mirrorNode->GetClonedGraph() );
   }
}
////////////////////////////////////////////////////////////////////////////////
cfdDataSet* cfdModel::GetActiveDataSet( void )
{
   return activeDataSet;
}

////////////////////////////////////////////////////////////////////////////////
void cfdModel::SetActiveDataSet( cfdDataSet* input )
{
   activeDataSet = input;
}
#ifdef _OSG
////////////////////////////////////////////////////////////////////////////////
void cfdModel::CreateTextureDataSet()
{
   mTextureDataSets.push_back(new cfdTextureDataSet());
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::AddDataSetToTextureDataSet(unsigned int index,
                                     std::string textureDescriptionFile)
{
   mTextureDataSets.at(index)->CreateTextureManager(textureDescriptionFile);
}
#endif
////////////////////////////////////////////////////////////////////////////////
void cfdModel::CreateGeomDataSet( std::string filename )
{
   mGeomDataSets.push_back( new CADEntity( filename, _worldDCS.get(), false, true ) );
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::setModelType( ModelTypeIndex type )
{
   this->mModelType = type;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* cfdModel::GetCfdNode( )
{
   return this->mModelNode;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdModel::GetDCS( )
{
   return this->_worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
bool cfdModel::GetMirrorDataFlag( void )
{
   return mirrorDataFlag;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::SetMirrorDataFlag( bool input )
{
   mirrorDataFlag = input;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdModel::GetID( void )
{
   return modelID;
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::SetID( unsigned int id )
{
   modelID = id;
}
////////////////////////////////////////////////////////////////////////////////
cfdDataSet* cfdModel::GetCfdDataSet( int dataset )
{
   // Check and see if we have any datasets
   // if not return null
   // to get the last added dataset pass in -1
   if ( mVTKDataSets.empty() )
      return NULL;
   else if ( dataset == -1 )
      return mVTKDataSets.back();
   else
      return mVTKDataSets.at( dataset );
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdModel::GetIndexOfDataSet( std::string dataSetName )
{
   unsigned int dataSetIndex = 0;
   for ( size_t i = 0; i < mVTKDataSets.size(); ++i )
   {
      if ( mVTKDataSets.at( i )->GetFileName() == dataSetName )
      {
         dataSetIndex = i;
         break;
      }
   }
   return dataSetIndex;
}
#ifdef _OSG
////////////////////////////////////////////////////////////////////////////////
VE_TextureBased::cfdTextureDataSet* cfdModel::GetTextureDataSet(unsigned int index)
{
   if(mTextureDataSets.empty())
   {
      return 0;
   }else{
      return mTextureDataSets.at(index);
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::SetActiveTextureDataSet(VE_TextureBased::cfdTextureDataSet* tDS)
{
   _activeTextureDataSet = tDS;
}
////////////////////////////////////////////////////////////////////////////////
VE_TextureBased::cfdTextureDataSet* cfdModel::GetActiveTextureDataSet()
{
   return _activeTextureDataSet;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfTextureDataSets()
{
   return mTextureDataSets.size();
}
#endif
////////////////////////////////////////////////////////////////////////////////
int cfdModel::GetKeyForCfdDataSet( cfdDataSet* input )
{
   int key = -1;
   for ( unsigned int i = 0; i < mVTKDataSets.size(); ++i )
   {
      if ( mVTKDataSets.at( i ) == input )
      {
         key = i;
         break;
      }
   }
   
   return key;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfCfdDataSets( void )
{
   return mVTKDataSets.size();
}
////////////////////////////////////////////////////////////////////////////////
CADEntity* cfdModel::GetGeomDataSet( int dataset )
{
   // Check and see if we have any datasets
   // if not return null
   // to get the last added dataset pass in -1
   if ( mGeomDataSets.empty() )
      return NULL;
   else if ( dataset == -1 )
      return mGeomDataSets.back();
   else
      return mGeomDataSets.at( dataset );
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfGeomDataSets( void )
{
   return mGeomDataSets.size();
}

////////////////////////////////////////////////////////////////////////////////
// Dynamic Loading Data Start From Here
void cfdModel::DynamicLoadingData(vtkUnstructuredGrid* dataset, int datasetindex, double* scale, double* trans, double* rotate)
{
   this->CreateCfdDataSet();
   

   vprDEBUG(vesDBG,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   //double scale[3], trans[3], rotate[3];   // pfDCS stuff
   //this->_readParam->read_pf_DCS_parameters( input, scale, trans, rotate);

   //hard code here and will change it later
   //scale[0]=2.0;scale[1]=2.0;scale[2]=2.0;
   //trans[0]=0.0; trans[1]=1.0; trans[2]=0.0;
   //rotate[0]=0.0; rotate[1]=0.0; rotate[2]=0.0;
   // Pass in -1 to GetCfdDataSet to get the last dataset added
   this->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( scale );
   this->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( trans );
   this->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( rotate );
   this->GetCfdDataSet( -1 )->LoadData(dataset, datasetindex);
  
  // this->MakingSurface( _model->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
  //
   std::cout<<"[DBG]...Before add data into waitinglist"<<std::endl;
  this->waitingdatalist.push_back(dataset);
  std::cout<<"[DBG]...After add data into waitinglist"<<std::endl;
   
}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::DynamicLoadingGeom(std::string surfacefilename, double* scale, 
               double* trans, double* rotate, double* stlColor, int color, int transFlag)
{  
   std::cout<<"[DBG]...the geom file is "<<surfacefilename<<std::endl;
   this->CreateGeomDataSet(surfacefilename);
   std::cout<<"[DBG]...after cfdFile constructor"<<std::endl;
   this->GetGeomDataSet(-1)->GetDCS()->SetScaleArray( scale );
   this->GetGeomDataSet(-1)->GetDCS()->SetTranslationArray( trans );
   this->GetGeomDataSet(-1)->GetDCS()->SetRotationArray(rotate);
}
////////////////////////////////////////////////////////////////////////////////
std::vector<vtkDataSet*> cfdModel::GetWaitingDataList()
{
   return this->waitingdatalist;
}
////////////////////////////////////////////////////////////////////////////////
#if __VJ_version > 2000003
void cfdModel::GetDataFromUnit(void)
#elif __VJ_version == 2000003
void cfdModel::GetDataFromUnit(void* unused)
#endif
{

 //std::vector<VTKSmartPtr<vtkUnstructuredGrid> > _gridList;
   //char answer;
   
   vpr::Uint32 data_length =0;
   vpr::Uint32 compressed_data_length(0);
   unsigned int bytes_read = 0;
   int i=0;
   
   while(1){
   std::cout<<"[DBG]...Before ACCEPT New Data, *****************************************"<<std::endl;
   vpr::SocketStream connection;
   vpr::InetAddr addr;
   std::string localhostname;
   localhostname = vpr::System::getHostname();
   addr.setAddress(localhostname, 50031);
   vpr::SocketAcceptor server(addr);

   server.accept(connection);
   i++;

   // Get the length of the uncompressed data.
   mValueLock.acquire();
   {
      try
      {
         //connection.recvn( (void*)(&data_length), sizeof(data_length), bytes_read);
      }
      catch (...)
      {
         std::cerr << "[ERR] Unable to receive data length "
         << __FILE__ << ":" << __LINE__ << std::endl;
      }
   }
   mValueLock.release();
   // Get the length of the compressed data
   mValueLock.acquire();
   {
      try
      {
         //connection.recvn( (void*)(&compressed_data_length), 
         //                         sizeof(compressed_data_length), bytes_read);
      }
      catch (...)
      {
         std::cerr << "[ERR] Unable to receive compressed data length "
         << __FILE__ << ":" << __LINE__ << std::endl;
         //return 1;
      }
   }
   mValueLock.release();
   
   ///Set the byte-order
   data_length = vpr::System::Ntohl(data_length);
   compressed_data_length = vpr::System::Ntohl(compressed_data_length);
   vpr::Uint8* compressed_data = new vpr::Uint8[compressed_data_length];
   mValueLock.acquire();
   {
      //connection.recvn( (void*)compressed_data, 
      //                           compressed_data_length, 
      //                           bytes_read );
   }
   mValueLock.release();
   
   /*if(status != vpr::ReturnStatus::Succeed)
   {
      std::cout << "[ERR] Error receiving data; read " << bytes_read << " of "
                << data_length << " bytes." << std::endl;
      if (status == vpr::ReturnStatus::Fail)
      {
         std::cout << "[ERR] Read failed." << std::endl;
      }
      else if (status == vpr::ReturnStatus::WouldBlock)
      {
         std::cout << "[ERR] This read would block the caller." << std::endl;
      }
      else if (status == vpr::ReturnStatus::Timeout)
      {
         std::cout << "[ERR] This read timed out." << std::endl;
      }
      else if (status == vpr::ReturnStatus::InProgress)
      {
         std::cout << "[ERR] This read is still in progress." << std::endl;
      }
      else if (status == vpr::ReturnStatus::NotConnected)
      {
         std::cout << "[ERR] The device is not connected." << std::endl;
      }
      else
      {
         std::cout << "[ERR] Unknown Result." << std::endl;
      }
      //return 1;
    }*/
    std::cout << "[DBG] Read " << data_length << " of " 
              << compressed_data_length << " bytes."
              << std::endl;
    // Uncompress the data
    vtkZLibDataCompressor* compressor = vtkZLibDataCompressor::New();
    vtkUnsignedCharArray* vtk_data = vtkUnsignedCharArray::New();
    vtk_data = compressor->Uncompress(compressed_data, compressed_data_length, data_length);
    /*VTKSmartPtr<vtkUnsignedCharArray> vtk_data = 
                compressor->Uncompress( compressed_data, 
                                        compressed_data_length,
                                        data_length );*/
    //VTKSmartPtr<vtkUnstructuredGridReader> reader;
    vpr::Uint8* data = new vpr::Uint8[vtk_data->GetSize()];
    data = vtk_data->WritePointer(0,data_length);
    vtkUnstructuredGridReader* reader = vtkUnstructuredGridReader::New();
    reader->ReadFromInputStringOn();
    reader->SetBinaryInputString( reinterpret_cast<char*>(data), 
                                    data_length );


    //VTKSmartPtr<vtkUnstructuredGrid> ugrid(reader->GetOutput());
    
    vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
    ugrid = reader->GetOutput();
    mValueLock.acquire();
    {
      std::cout << "[DBG] Now drawing" << std::endl;
    }
    mValueLock.release();
      
    mValueLock.acquire();
    {
      std::cout<<"[DBG]...READY TO READ DATA FROM UNIT **********************************"<<std::endl;
      double scale[3],trans[3],rotate[3];
      scale[0] = scale[1] = scale[2] = 1.0f;
      trans[0] = trans[1] = trans[2] = 0.0f;
      rotate[0] = rotate[1] = rotate[2] = 0.0f;
      //since the user doesn't have access to the scale a and translate edata in this thread
      // this data for this cfddataset can be set after rloading that dataset 
      // in the activate custom viz function when necessary 
      this->DynamicLoadingData(ugrid,i,scale,trans,rotate);
      std::cout<<"[DBG]...AFTER LOAD DATA ****************************************"<<std::endl;
      this->currentsurfacefilename =(std::string)(this->MakeSurfaceFile(ugrid, i));

      std::cout<<"[DBG]...current surface file is "<<currentsurfacefilename<<std::endl;
      //currentsurfacefilename ="NewlyLoadedDataSet_1.stl";
      //this->DynamicLoadingGeom(currentsurfacefilename);
      std::cout<<"[DBG]...After load geom data************************************"<<std::endl;
            //reader->Delete();
      //ugrid->Delete();
      connection.close();
   
      std::cout<<"[DBG]...After Server Close"<<std::endl;
   }
   mValueLock.release();

  }

}
////////////////////////////////////////////////////////////////////////////////
void cfdModel::ActiveLoadingThread()
{
#if __VJ_version > 2000003
   this->loadDataTh = new vpr::Thread( boost::bind( &cfdModel::GetDataFromUnit, this ) );
#elif __VJ_version == 2000003
   this->loadDataTh = new vpr::Thread( 
                        new vpr::ThreadMemberFunctor< cfdModel >( 
                                 this, &cfdModel::GetDataFromUnit) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string cfdModel::MakeSurfaceFile(vtkDataSet* ugrid,int datasetindex)
{
   std::ostringstream file_name;
   std::string currentStlFileName = "NewlyLoadedDataSet_000.stl";
   file_name<<"NewlyLoadedDataSet_"<<datasetindex<<".stl";
   currentStlFileName = file_name.str();

   std::string newStlName;

   newStlName = currentStlFileName;

   vtkPolyData * surface = NULL;
   std::cout<<"[DBG]... after readVtkThing"<<std::endl;
    // Create a polydata surface file that completely envelopes the solution space
   float deciVal=0.7;
   surface = cfdGrid2Surface( ugrid , deciVal );
   
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   vtkGeometryFilter *gFilter = NULL;

   // convert dataset to vtkPolyData
   tFilter->SetInput( (vtkPolyData*)surface );
  
   std::cout << "Writing \"" << newStlName << "\"... ";
   std::cout.flush();
   vtkSTLWriter *writer = vtkSTLWriter::New();
   writer->SetInput( tFilter->GetOutput() );
   writer->SetFileName( newStlName.c_str() );
   writer->SetFileTypeToBinary();
   writer->Write();
   writer->Delete();
   std::cout << "... done" << std::endl;

   tFilter->Delete();

   if ( gFilter ) 
      gFilter->Delete();

//clean up
   surface->Delete();
   std::cout << "\ndone\n";
   return newStlName;
}
//Dynamic Loading Data End Here
/////////////////////////////////////////////////
void cfdModel::AddNewSound(std::string soundName,
                           std::string filename)
{
   cfdSound newSound;
   newSound.fileName = filename;
   newSound.soundName = soundName;

   if(newSound.initSound())
   {
      _availableSounds[soundName] = newSound;
   }

}
///////////////////////////////////////////////////
void cfdModel::ActivateSound(std::string soundName)
{
   try
   {
      _availableSounds[soundName].playSound();
   }
   catch(...)
   {
      std::cout<<"Invalid sound: "<<soundName<<std::endl;
      std::cout<<"cfdModel::ActivateSound"<<std::endl;
   }
}
/////////////////////////////////////////////////////
void cfdModel::DeactivateSound(std::string soundName)
{
   try
   {
      _availableSounds[soundName].stopSound();
   }
   catch(...)
   {
      std::cout<<"Invalid sound: "<<soundName<<std::endl;
      std::cout<<"cfdModel::ActivateSound"<<std::endl;
   }
}