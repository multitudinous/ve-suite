/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
#include "cfdModel.h"
#include "cfdObjects.h"
#include "cfdReadParam.h"
#include "cfdDataSet.h"
#include "cfdDCS.h"
#include "cfdNode.h"
#include "cfdFILE.h"

#include <vpr/Util/Debug.h>

cfdModel::cfdModel( cfdDCS *worldDCS)
{
   vprDEBUG(vprDBG_ALL,1) << " New cfdModel ! " 
                          << std::endl << vprDEBUG_FLUSH;
   this->mModelNode = NULL;
   //this->actor = NULL;
   //ModelIndex = static_cast<ModelTypeIndex>(value);
   // Will fix this later so that each model has a dcs
   //mModelDCS = new cfdDCS();
   _worldDCS = worldDCS;
}

cfdModel::~cfdModel()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdModel destructor"
                          << std::endl << vprDEBUG_FLUSH;
/*
   for(GeometryDataSetList::iterator itr = mGeomDataSets.begin();
                                     itr != mGeomDataSets.end(); itr++)
   {
      delete *itr;
   }
   mGeomDataSets.clear();
*/
   for(VTKDataSetList::iterator itr = mVTKDataSets.begin();
                                itr != mVTKDataSets.end(); itr++)
   {
      delete *itr;
      itr++;
   }
   mVTKDataSets.clear();
}

void cfdModel::CreateCfdDataSet( void )
{
   mVTKDataSets.push_back( new cfdDataSet() );
}

void cfdModel::CreateGeomDataSet( char* filename )
{
   mGeomDataSets.push_back( new cfdFILE( filename, _worldDCS ) );
}

void cfdModel::setModelNode( cfdNode *temp )
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
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << t[0] << " y: "
      << t[1] << " z: " << t[2] << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setTrans3( float x, float y, float z )
{
   std::cout<<" !!!!! cfdModel::setTrans3 is doing nothing"<<std::endl;    
   //this->mModelDCS->SetTrans( x, y, z );
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setScale( float x, float y, float z )
{
   float temp[ 3 ];
   temp [ 0 ] = x;
   temp [ 1 ] = y;
   temp [ 2 ] = z;

   this->mModelDCS->SetScaleArray( temp );
   vprDEBUG(vprDBG_ALL,1) << "Scale x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setRot(float h, float p, float r)
{
   float temp[ 3 ];
   temp [ 0 ] = h;
   temp [ 1 ] = p;
   temp [ 2 ] = r;
   this->mModelDCS->SetRotationArray(temp);
   vprDEBUG(vprDBG_ALL,1) << "Rot h: " << h << " p: " 
      << p << " r: " << r << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setRotMat(double *rotate)
{
   std::cout<<" !!!!! cfdModel::setRotMat is doing nothing"<<std::endl;    
}

cfdNode* cfdModel::GetCfdNode( )
{
   return this->mModelNode;
}

cfdDCS* cfdModel::GetCfdDCS( )
{
   return this->_worldDCS;
}

void cfdModel::updateCurModel()
{
   vprDEBUG(vprDBG_ALL, 1) << "cfdModel::UpdateCurModel..."
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

void cfdModel::addVTKdataset(const std::string& vtkfilename)
{
   std::cout<<" !!!!! cfdModel::addVTKdataset is doing nothing"<<std::endl;    
}

void cfdModel::delVTKdataset()
{
   std::cout<<" !!!!! cfdModel::delVTKdataset is doing nothing"<<std::endl;    
}

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

void cfdModel::delGeomdataset(int DelIndex)
{
   delete (mGeomDataSets[DelIndex]);
   this->mGeomDataSets.erase(this->mGeomDataSets.begin() + DelIndex);
}

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

unsigned int cfdModel::GetNumberOfCfdDataSets( void )
{
   return mVTKDataSets.size();
}

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

unsigned int cfdModel::GetNumberOfGeomDataSets( void )
{
   return mGeomDataSets.size();
}

