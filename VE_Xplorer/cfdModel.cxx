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
 * File:          $RCSfile: cfdModel.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_SceneGraph/cfdTempAnimation.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdNode.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdClone.h"
#include "VE_SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Builder/Translator/cfdGrid2Surface.h"
#include "VE_SceneGraph/cfdClone.h"
#include "VE_SceneGraph/Utilities/Attribute.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/XMLObjectFactory.h"
#include "VE_Open/XML/XMLCreator.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADCreator.h"

#include "VE_Open/XML/Shader/ShaderCreator.h"


#ifdef _OSG
#include <osg/StateSet>
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdTextureDataSet.h"
using namespace VE_TextureBased;
#endif
#endif

#include "VE_Xplorer/cfdDebug.h"
#include <vpr/IO/Socket/SocketStream.h>
#include <vpr/IO/Socket/SocketAcceptor.h>
#include <vpr/System.h>
//#include <vpr/vprTypes.h>

#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkDataWriter.h>
#include <vtkZLibDataCompressor.h>
#include <vtkUnsignedCharArray.h>
#include <vtkDataSet.h>
#include <vtkTriangleFilter.h>
#include <vtkGeometryFilter.h>
#include <vtkSTLWriter.h>
#include <vtkPolyData.h>

#include <fstream>
#include <sstream>

#include "VE_Xplorer/cfdModel.h"
using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdModel::cfdModel( VE_SceneGraph::cfdDCS *worldDCS )
{
   vprDEBUG(vesDBG,1) << "|\tNew cfdModel ! " 
                          << std::endl << vprDEBUG_FLUSH;
   this->mModelNode = 0;
   //this->actor = NULL;
   //ModelIndex = static_cast<ModelTypeIndex>(value);
   // Will fix this later so that each model has a dcs
   //mModelDCS = new cfdDCS();
   _worldDCS = worldDCS;
   mirrorNode = 0;
   mirrorGroupNode = 0;

   this->animation = 0;
   this->activeDataSet = 0;
   mirrorDataFlag = false;
#ifdef _OSG
   _activeTextureDataSet = 0;
#endif
   _rootCADNode = 0;
   modelID = 10000000;
}

cfdModel::~cfdModel()
{
   vprDEBUG(vesDBG,2) << "cfdModel destructor"
                          << std::endl << vprDEBUG_FLUSH;

   for ( std::map<unsigned int,VE_Xplorer::cfdFILE*>::iterator itr = _partList.begin();
                                       itr != _partList.end(); itr++ )
   {
      delete itr->second;
   }
   _partList.clear();
   for ( std::map<unsigned int,VE_SceneGraph::cfdDCS*>::iterator itr = _assemblyList.begin();
                                       itr != _assemblyList.end(); itr++ )
   {
      delete itr->second;
   }
   _assemblyList.clear();

   for ( std::map<unsigned int,VE_SceneGraph::cfdClone*>::iterator itr = _cloneList.begin();
                                       itr != _cloneList.end(); itr++ )
   {
      delete itr->second;
   }
   _cloneList.clear();
  /* for ( std::map<std::string ,VE_EVENTS::EventHandler*>::iterator itr = _eventHandlers.begin();
                                       itr != _eventHandlers.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _eventHandlers.clear();*/
   // the following block allows the program to get to pfExit
   for ( VTKDataSetList::iterator itr = mVTKDataSets.begin();
                                  itr != mVTKDataSets.end(); itr++ )
   {
      vprDEBUG(vesDBG,2) << "deleting a cfdModel"
                             << std::endl << vprDEBUG_FLUSH;
      delete *itr;
   }
   mVTKDataSets.clear();

   //texture data cleanup
#ifdef _OSG
#ifdef VE_PATENTED
   TextureDataSetList::iterator tDataSet;
   for ( tDataSet=mTextureDataSets.begin(); tDataSet!=mTextureDataSets.end();tDataSet++ )
   {
         delete *tDataSet;
   }
   mTextureDataSets.clear();
#endif
#endif
 
   std::map<int,cfdDataSet*>::iterator foundPlugin;
   // Remove any plugins that aren't present in the current network
   for ( foundPlugin=transientDataSets.begin(); foundPlugin!=transientDataSets.end(); )
   {
         // When we clear the _plugin map will
         // loop over all plugins
         transientDataSets.erase( foundPlugin++ );
   }
   transientDataSets.clear();

   if( animation )
   {
      delete animation;
      animation = 0;
   }

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
   // Do we need to delete these
   // it should be tested  
 /*this->switchNode = new cfdSwitch();
   this->classic = new cfdGroup();
   this->textureBased = new cfdGroup();
*/

   vprDEBUG(vesDBG,2) << "cfdModel destructor finished"
                          << std::endl << vprDEBUG_FLUSH;
   if(_rootCADNode)
   {
      delete _rootCADNode;
      _rootCADNode = 0;
   }
}

VE_SceneGraph::cfdTempAnimation* cfdModel::GetAnimation()
{
   if ( !animation )
   {
      animation = new VE_SceneGraph::cfdTempAnimation();
   }
   return animation;
}
///////////////////////////////
void cfdModel::PreFrameUpdate()
{
   vprDEBUG(vesDBG,1) << "cfdModel::PreFrameUpdate " <<std::endl<< vprDEBUG_FLUSH;;
}
///////////////////////////////////////
void cfdModel::CreateCfdDataSet( void )
{
   mVTKDataSets.push_back( new cfdDataSet() );
}

/////////////////////////////////////////////////////////////
void cfdModel::SetMirrorNode( VE_SceneGraph::cfdGroup* dataNode )
{
   if ( !mirrorNode )
   {
      mirrorNode = new VE_SceneGraph::cfdClone();
      mirrorNode->CloneNode( GetActiveDataSet()->GetDCS() ); 
      float rot[ 3 ];
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
      mirrorNode = new VE_SceneGraph::cfdClone( GetActiveDataSet()->GetDCS() );
      float rot[ 3 ];
      rot[ 0 ] = 180.0f;
      rot[ 1 ] = 0.0f;
      rot[ 2 ] = 0.0f;
      mirrorNode->SetRotationArray( rot );
      this->_worldDCS->AddChild( mirrorNode->GetClonedGraph() );
   }
}
///////////////////////////////////////
cfdDataSet* cfdModel::GetActiveDataSet( void )
{
   return activeDataSet;
}

////////////////////////////////////////////////////
void cfdModel::SetActiveDataSet( cfdDataSet* input )
{
   activeDataSet = input;
}
#ifdef _OSG
#ifdef VE_PATENTED
/////////////////////////////////////
void cfdModel::CreateTextureDataSet()
{
   mTextureDataSets.push_back(new cfdTextureDataSet());
}
/////////////////////////////////////////////////////////////////
void cfdModel::AddDataSetToTextureDataSet(unsigned int index,
                                     std::string textureDescriptionFile)
{
   mTextureDataSets.at(index)->CreateTextureManager(textureDescriptionFile);
}
#endif
#endif
//////////////////////////////////////////////////
void cfdModel::CreateGeomDataSet( std::string filename )
{
   mGeomDataSets.push_back( new cfdFILE( filename, _worldDCS ) );
}

void cfdModel::setModelNode( VE_SceneGraph::cfdNode *temp )
{
   std::cout<<" !!!!! cfdModel::setModelNode is doing nothing"<<std::endl;    
}

void cfdModel::setModelType( ModelTypeIndex type )
{
   this->mModelType = type;
}

void cfdModel::setTrans( float t[3] )
{
   this->mModelDCS->SetTranslationArray( t );
   this->setTrans3( t[0], t[1], t[2] );
   vprDEBUG(vesDBG,1) << "Trans x: " << t[0] << " y: "
      << t[1] << " z: " << t[2] << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////////////
void cfdModel::setTrans3( float x, float y, float z )
{
   std::cout<<" !!!!! cfdModel::setTrans3 is doing nothing"<<std::endl;    
   //this->mModelDCS->SetTrans( x, y, z );
   vprDEBUG(vesDBG,1) << "Trans x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}
//////////////////////////////////////////////////////
void cfdModel::setScale( float x, float y, float z )
{
   float temp[ 3 ];
   temp [ 0 ] = x;
   temp [ 1 ] = y;
   temp [ 2 ] = z;

   this->mModelDCS->SetScaleArray( temp );
   vprDEBUG(vesDBG,1) << "Scale x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}
/////////////////////////////////////////////////
void cfdModel::setRot(float h, float p, float r)
{
   float temp[ 3 ];
   temp [ 0 ] = h;
   temp [ 1 ] = p;
   temp [ 2 ] = r;
   this->mModelDCS->SetRotationArray(temp);
   vprDEBUG(vesDBG,1) << "Rot h: " << h << " p: " 
      << p << " r: " << r << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////
void cfdModel::setRotMat(double *rotate)
{
   std::cout<<" !!!!! cfdModel::setRotMat is doing nothing"<<std::endl;    
}
////////////////////////////////
VE_SceneGraph::cfdNode* cfdModel::GetCfdNode( )
{
   return this->mModelNode;
}
//////////////////////////////
VE_SceneGraph::cfdDCS* cfdModel::GetCfdDCS( )
{
   return this->_worldDCS;
}
///////////////////////////////
bool cfdModel::GetMirrorDataFlag( void )
{
   return mirrorDataFlag;
}
///////////////////////////////
void cfdModel::SetMirrorDataFlag( bool input )
{
   mirrorDataFlag = input;
}
///////////////////////////////
unsigned int cfdModel::GetID( void )
{
   return modelID;
}
///////////////////////////////
void cfdModel::SetID( unsigned int id )
{
   modelID = id;
}
///////////////////////////////
void cfdModel::updateCurModel()
{
   vprDEBUG(vesDBG, 1) << "cfdModel::UpdateCurModel..."
                           << std::endl << vprDEBUG_FLUSH;
   // Need to fix this 
   std::string tempstring;
   int temp;
   temp = 1;
   std::cout<<"Setting temp to 1 to avoid irix compile warning \'The variable \"temp\" is used before its value is set\'"<<std::endl;    
  if( this->mUpdateModelFlag )
  {
      switch(this->mActiveOperation2Model)
      {
         case AddVTKdataset:
            addVTKdataset(tempstring); 
            break;
               
         case DeleteVTKdataset:
            delVTKdataset();
            break;
            
         case AddGeodataset:
            addGeomdataset( tempstring );
            break;
            
         case DeleteGeomdataset:
            delGeomdataset( temp ); 
            break;
            
         default:
            std::cout<<"[DBG]...This is no legal operation..."<<std::endl;    
            break;
      }
   }
}
////////////////////////////////////////////////////////////
void cfdModel::addVTKdataset(const std::string& vtkfilename)
{
   std::cout<<" !!!!! cfdModel::addVTKdataset is doing nothing"<<std::endl;    
}
//////////////////////////////
void cfdModel::delVTKdataset()
{
   std::cout<<" !!!!! cfdModel::delVTKdataset is doing nothing"<<std::endl;    
}
//////////////////////////////////////////////////////////////
void cfdModel::addGeomdataset(const std::string& geomfilename)
{
   std::cout << "WARNING: doing nothing in cfdModel::addGeomdataset with \"" << geomfilename << "\""<< std::endl;
      // Need to fix this
      // this->mGeomFileInfo->fileName= (char*)geomfilename.c_str();
      //this->mMoveOldGeomDataSets = true;
      //this->mMoveOldVTKDataSets = true;
      //char* mGeomFileName;
      //std::cout << "[DBG]....Adding Geometry files " << mGeomFileName<<std::endl;
      // Need to fix the iterator stuff
      //for(GeometryDataSetList::iterator itr = mGeomDataSets.begin(); itr != mGeomDataSets.end(); ++itr)

      {/*
         //The following need to be changed later, maybe we can get trans information from Java GUI
         this->mGeomDataSets[itr]->DCS->setTrans(, , ,); 
         
        */ 
      }
      //assume that if we move the geometry, the relative VTK dataset should be moved too.
      // Need to fix the iterator stuff
      //for(VTKDataSetList::iterator itr = mVTKDataSets.begin(); itr != mVTKDataSets.end(); ++itr)
      {
         //this->mVTKDataSets[i]->DCS->setTrans(, , ,);
      }
      
      // Fix this don't have a class cfdGeomSets
      //this->mGeomDataSets.push_back( new cfdGeomSets(fileInfo *mGeomFileInfo,this->mModelDCS ) );
     /* 
      Need to find an efficient way to  
      this->mGeomeDataSets->DCS->setScale( cfdscale[0], cfdscale[1], cfdscale[2] );
      this->mGeomDataSets->DCS->setRot( cfdvrxprRot[0], cfdvrxprRot[1],cfdvrxprRot[2] );
      this->mGeometrySets->DCS->setTrans( cfdtranslate );
     */
      //this->mMoveOldGeomDataSets = false;
      //this->mMoveOldVTKDataSets = false;
}
///////////////////////////////////////////
void cfdModel::delGeomdataset(int DelIndex)
{
   //delete (mGeomDataSets[DelIndex]);
   this->mGeomDataSets.erase(this->mGeomDataSets.begin() + DelIndex);
}
///////////////////////////////////////////////////
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
#ifdef _OSG
/////////////////////////////////////////////////////////////////////
VE_TextureBased::cfdTextureDataSet* cfdModel::GetTextureDataSet(unsigned int index)
{
   if(mTextureDataSets.empty())
   {
      return 0;
   }else{
      return mTextureDataSets.at(index);
   }
}
///////////////////////////////////////////////////////////////
void cfdModel::SetActiveTextureDataSet(VE_TextureBased::cfdTextureDataSet* tDS)
{
   _activeTextureDataSet = tDS;
}
//////////////////////////////////////////////////////
VE_TextureBased::cfdTextureDataSet* cfdModel::GetActiveTextureDataSet()
{
   return _activeTextureDataSet;
}
///////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfTextureDataSets()
{
   return mTextureDataSets.size();
}
#endif
///////////////////////////////////////////////////////
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
/////////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfCfdDataSets( void )
{
   return mVTKDataSets.size();
}
////////////////////////////////////////////////
cfdFILE* cfdModel::GetGeomDataSet( int dataset )
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
//////////////////////////////////////////////////////
unsigned int cfdModel::GetNumberOfGeomDataSets( void )
{
   return mGeomDataSets.size();
}

/////////////////////////////////////////////////////
// Dynamic Loading Data Start From Here
void cfdModel::DynamicLoadingData(vtkUnstructuredGrid* dataset, int datasetindex, float* scale, float* trans, float* rotate)
{
   this->CreateCfdDataSet();
   

   vprDEBUG(vesDBG,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   //float scale[3], trans[3], rotate[3];   // pfDCS stuff
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

void cfdModel::DynamicLoadingGeom(std::string surfacefilename, float* scale, 
               float* trans, float* rotate, float* stlColor, int color, int transFlag)
{  
   //float scale[3], trans[3], rotate[3];   // pfDCS stuff
   //float stlColor[3];
   //int color;
   //int transFlag;

   //hard code here and will change it later
   //scale[0]=2.0;scale[1]=2.0;scale[2]=2.0;
   //trans[0]=0.0; trans[1]=1.0; trans[2]=0.0;
   //rotate[0]=0.0; rotate[1]=0.0; rotate[2]=0.0;
   //color =1;
   //stlColor[0]=1; stlColor[1]=0; stlColor[2]=0;
   //transFlag =1;
   std::cout<<"[DBG]...the geom file is "<<surfacefilename<<std::endl;
   this->CreateGeomDataSet(surfacefilename);
   std::cout<<"[DBG]...after cfdFile constructor"<<std::endl;
   this->GetGeomDataSet(-1)->GetDCS()->SetScaleArray( scale );
   this->GetGeomDataSet(-1)->GetDCS()->SetTranslationArray( trans );
   this->GetGeomDataSet(-1)->GetDCS()->SetRotationArray(rotate);
   this->GetGeomDataSet(-1)->SetFILEProperties(color, transFlag, stlColor);
   this->GetGeomDataSet(-1)->setOpac(1.0f);

}

std::vector<vtkDataSet*> cfdModel::GetWaitingDataList()
{
   return this->waitingdatalist;
}

void cfdModel::GetDataFromUnit(void* unused)
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
   vpr::ReturnStatus status;

   // Get the length of the uncompressed data.
   mValueLock.acquire();
   {
      status = connection.recvn( (void*)(&data_length), sizeof(data_length), bytes_read);
   }
   mValueLock.release();
   if(status != vpr::ReturnStatus::Succeed)
   {
     std::cerr << "[ERR] Unable to receive data length "
               << __FILE__ << ":" << __LINE__ << std::endl;
     //return 1;
   }
   // Get the length of the compressed data
   mValueLock.acquire();
   {
      status = connection.recvn( (void*)(&compressed_data_length), 
                                  sizeof(compressed_data_length), bytes_read);
   }
   mValueLock.release();
   if(status != vpr::ReturnStatus::Succeed)
   {
     std::cerr << "[ERR] Unable to receive compressed data length "
               << __FILE__ << ":" << __LINE__ << std::endl;
     //return 1;
   }
   
   ///Set the byte-order
   data_length = vpr::System::Ntohl(data_length);
   compressed_data_length = vpr::System::Ntohl(compressed_data_length);
   vpr::Uint8* compressed_data = new vpr::Uint8[compressed_data_length];
   mValueLock.acquire();
   {
      status = connection.recvn( (void*)compressed_data, 
                                 compressed_data_length, 
                                 bytes_read );
   }
   mValueLock.release();
   
   if(status != vpr::ReturnStatus::Succeed)
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
    }
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
      float scale[3],trans[3],rotate[3];
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

void cfdModel::ActiveLoadingThread()
{
   vpr::ThreadMemberFunctor<cfdModel> *loadDataFunc;
   loadDataFunc = new vpr::ThreadMemberFunctor<cfdModel> (this, &cfdModel::GetDataFromUnit);
   this->loadDataTh = new vpr::Thread( loadDataFunc );
}

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

///////////////////////////////////////////
VE_CAD::CADNode* cfdModel::GetRootCADNode()
{
   return _rootCADNode;
}
///////////////////////////////////////////////////////
void cfdModel::CreateClone(unsigned int cloneID,
                        unsigned int originalID,
                        std::string originalType)
{
   if(originalType == std::string("Assembly"))
   {
      _cloneList[cloneID] = new VE_SceneGraph::cfdClone(GetAssembly(originalID));
   }
   else if(originalType == std::string("Part"))
   {
      _cloneList[cloneID] = new VE_SceneGraph::cfdClone(GetPart(originalID)->GetNode());
   }
   
}
///////////////////////////////////////////////////////
void cfdModel::CreateAssembly(unsigned int assemblyID)
{
   _assemblyList[assemblyID] = new VE_SceneGraph::cfdDCS();
}
//////////////////////////////////////////////
void cfdModel::CreatePart(std::string fileName,
                       unsigned int partID,
                       unsigned int parentID)
{
   _partList[partID] = new VE_Xplorer::cfdFILE(fileName,_assemblyList[parentID]);
}
////////////////////////////////////////////////////////////////
void cfdModel::SetActiveAttributeOnNode(unsigned int nodeID,
                                       std::string nodeType,
                                       std::string attributeName)
{
#ifdef _OSG
   std::map< unsigned int, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
   attributeList = _nodeAttributes.find(nodeID);
   
   if(attributeList != _nodeAttributes.end())
   {
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
      namesAndAttributes = attributeList->second;
      for(foundAttribute = namesAndAttributes.begin();
          foundAttribute != namesAndAttributes.end();
          foundAttribute++)
      {
         vprDEBUG(vesDBG,1) <<"|\tFound attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
         if(foundAttribute->first == attributeName)
         {
            if(nodeType == "Assembly")
            {
               vprDEBUG(vesDBG,1) <<"|\tSetting Assembly attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
               GetAssembly(nodeID)->GetRawNode()->setStateSet(foundAttribute->second.get());
            }
            else if(nodeType == "Part")
            {
               vprDEBUG(vesDBG,1) <<"|\tSetting Part attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
               GetPart(nodeID)->GetDCS()->GetRawNode()->setStateSet(foundAttribute->second.get());
               vprDEBUG(vesDBG,1) <<"|\tvalid: "<<foundAttribute->first<<std::endl;
            }
            else if(nodeType == "Clone")
            {
               vprDEBUG(vesDBG,1) <<"|\tSetting Clone attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
               GetClone(nodeID)->GetClonedGraph()->GetRawNode()->setStateSet(foundAttribute->second.get());
            }
         }
      }
   }
   else
   {
      vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
   }
#elif _PERFORMER               
   std::cout<<"cfdModel::SetActiveAttributeOnNode not implemented for Performer yet!!!"<<std::endl;
#endif
}

//////////////////////////////////////////////
void cfdModel::AddAttributeToNode(unsigned int nodeID,
                              VE_CAD::CADAttribute* newAttribute)
{
#ifdef _OSG
   vprDEBUG(vesDBG,1) <<"|\tcfdModel::AddAttributeToNode()---"<<std::endl<< vprDEBUG_FLUSH;
   osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = new VE_SceneGraph::Utilities::Attribute();
   attribute->CreateStateSetFromAttribute(newAttribute);

   std::pair<std::string,osg::ref_ptr< osg::StateSet > >attributeInfo;
   attributeInfo.first = newAttribute->GetAttributeName();
   attributeInfo.second = attribute.get();
   
   std::map< unsigned int, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
   attributeList = _nodeAttributes.find(nodeID);
   
   if(attributeList != _nodeAttributes.end())
   {
      vprDEBUG(vesDBG,1) <<"|\tAdding attribute: "<<attributeList->first<<std::endl<< vprDEBUG_FLUSH;
      attributeList->second.push_back(attributeInfo);
   }
   else
   { 
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > temp;
      vprDEBUG(vesDBG,1) <<"|\tAdding attribute: "<<attributeList->first<<std::endl<< vprDEBUG_FLUSH;
      temp.push_back(attributeInfo);
      _nodeAttributes[nodeID] = temp;
   }
#elif _PERFORMER
   vprDEBUG(vesDBG,1)<<"Not implemented for Performer yet!!!"<<std::endl<< vprDEBUG_FLUSH;
#endif
   vprDEBUG(vesDBG,1) <<"|\tend cfdModel::AddAttributeToNode()---"<<std::endl<< vprDEBUG_FLUSH;

}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void cfdModel::UpdateMaterialMode(unsigned int nodeID,std::string attributeName,std::string type,std::string mode)
{
#ifdef _OSG
   std::map< unsigned int, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
   attributeList = _nodeAttributes.find(nodeID);
   
   if(attributeList != _nodeAttributes.end())
   {
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
      namesAndAttributes = attributeList->second;
      for(foundAttribute = namesAndAttributes.begin();
          foundAttribute != namesAndAttributes.end();
          foundAttribute++)
      {
         vprDEBUG(vesDBG,1) <<"|\tFound attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
         if(foundAttribute->first == attributeName)
         {
            ///update the material component
            osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = 
                              dynamic_cast<VE_SceneGraph::Utilities::Attribute*>(foundAttribute->second.get());
            if(attribute.valid())
            {
               attribute->UpdateMaterialMode(type,mode);
            }
            else
            {
               vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
            }
         }
      }
   }
#endif
}
//////////////////////////////////////////////////////////////////////////////////////
void cfdModel::UpdateMaterialComponent(unsigned int nodeID, std::string attributeName,
                                       std::string component,std::string face,std::vector<double> values)
{
#ifdef _OSG
   std::map< unsigned int, std::vector< std::pair< std::string, osg::ref_ptr< osg::StateSet > > > >::iterator attributeList;
   attributeList = _nodeAttributes.find(nodeID);
   
   if(attributeList != _nodeAttributes.end())
   {
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > > namesAndAttributes;
      std::vector< std::pair<std::string,osg::ref_ptr< osg::StateSet > > >::iterator foundAttribute;
      namesAndAttributes = attributeList->second;
      for(foundAttribute = namesAndAttributes.begin();
          foundAttribute != namesAndAttributes.end();
          foundAttribute++)
      {
         vprDEBUG(vesDBG,1) <<"|\tFound attribute: "<<foundAttribute->first<<std::endl<< vprDEBUG_FLUSH;
         if(foundAttribute->first == attributeName)
         {
            ///update the material component
            osg::ref_ptr<VE_SceneGraph::Utilities::Attribute> attribute = 
                              dynamic_cast<VE_SceneGraph::Utilities::Attribute*>(foundAttribute->second.get());
            if(attribute.valid())
            {
               attribute->UpdateMaterial(component,face,values);
            }
            else
            {
               vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
            }
         }
      }
   }
   else
   {
      vprDEBUG(vesDBG,1) <<"|\tAttribute not found: "<<attributeName<<std::endl<< vprDEBUG_FLUSH;
   }
#endif
}
///////////////////////////////////////////////////////////
VE_Xplorer::cfdFILE* cfdModel::GetPart(unsigned int partID)
{
   return _partList[partID];
}
/////////////////////////////////////////////////////////////////////
VE_SceneGraph::cfdDCS* cfdModel::GetAssembly(unsigned int assemblyID)
{
   return _assemblyList[assemblyID];
}
/////////////////////////////////////////////////////////////////////
VE_SceneGraph::cfdClone* cfdModel::GetClone(unsigned int cloneID)
{
   return _cloneList[cloneID];
}
//////////////////////////////////////////////
bool cfdModel::PartExists(unsigned int partID)
{
   std::map<unsigned int,VE_Xplorer::cfdFILE*>::iterator foundPart;
   foundPart = _partList.find(partID);

   if(foundPart != _partList.end())
   {
      return true;
   }
   return false;
}
/////////////////////////////////////////////////////
bool cfdModel::AssemblyExists(unsigned int assemblyID)
{
   std::map<unsigned int,VE_SceneGraph::cfdDCS*>::iterator foundAssembly;
   foundAssembly = _assemblyList.find(assemblyID);

   if(foundAssembly != _assemblyList.end())
   {
      return true;
   }
   return false;
}
/////////////////////////////////////////////////////
bool cfdModel::CloneExists(unsigned int cloneID)
{
   std::map<unsigned int,VE_SceneGraph::cfdClone*>::iterator foundClone;
   foundClone = _cloneList.find(cloneID);

   if(foundClone!= _cloneList.end())
   {
      return true;
   }
   return false;
}
