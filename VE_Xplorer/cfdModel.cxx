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
 * Date modified: $Date: 2004/03/23 16:29:16 $
 * Version:       $Revision: 1.5 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdModel_hgx.h"
#include "cfdObjects.h"
#include "cfdReadParam.h"
#include "cfdDataSet.h"
#include <vpr/Util/Debug.h>

// Performer Includes
#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfNode.h>

// C++ Libs
#include <vector>


cfdModel::cfdModel( pfDCS *worldDCS)
{
   vprDEBUG(vprDBG_ALL, 1) << " New cfdModel ! " 
                           << std::endl << vprDEBUG_FLUSH;
   this->mModelNode = NULL;
   this->actor = NULL;
   ModelIndex = static_cast<ModelTypeIndex>(value);
   mModelDCS=new pfDCS;
   worldDCS->addChild(mModelDCS);
   
}


cfdModel::~cfdModel()
{
   for(GeometryDataSetList::iterator itr = mGeomDataSets.begin(); itr != mGeomDataSets.end(); ++itr)
   {
      delete *itr;
   }
   mGeomDataSets.clear();
   for(VTKDataSetList::iterator itr = mVTKDataSets.begin(); itr != mVTKDataSets.end(); ++itr)
   {
      delete *itr;
   }
   mVTKDataSets.clear();
}

void cfdModel::setModelNode( *pfNode *temp )
{

}

void cfdModel::setModelType( ModelTypeIndex type )
{
   this->mModelType = type;
}

void cfdModel::setTrans( float t[3] )

{
   this->setTrans3( t[0], t[1], t[2] );
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << t[0] << " y: "
      << t[1] << " z: " << t[2] << std::endl << vprDEBUG_FLUSH;
}


void cfdModel::setTrans3( float x, float y, float z )
{
   this->mModelDCS->setTrans( x, y, z );
   vprDEBUG(vprDBG_ALL,1) << "Trans x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setScale( float x, float y, float z )
{
   this->mModelDCS->setScale( x, y, z );
   vprDEBUG(vprDBG_ALL,1) << "Scale x: " << x << " y: " 
      << y << " z: " << z << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setRot(float h, float p, float r)
{
   this->mModelDCS->setRot(h,p,r);
   vprDEBUG(vprDBG_ALL,1) << "Rot h: " << h << " p: " 
      << p << " r: " << r << std::endl << vprDEBUG_FLUSH;
}

void cfdModel::setRotMat(double *rotate)
{
}

pfNode *cfdModel::getpfNode( )
{
   return this->node;
}

pfDCS *cfdModel::getpfDCS( )
{
   return this->mModeldcs;
}

void updateCurModel()
{
  
   vprDEBUG(vprDBG_ALL, 1) << "cfdModel::UpdateCurModel..."
                           << std::endl << vprDEBUG_FLUSH;
  if(this->mUpdateModelFlag)
  {
      switch(this->mActiveOperation2Model)
      {
         case AddVTKdataset:
            addVTKdataset(); 
            break;
               
         case DeleteVTKdataset:
            delVTKdataset();
            break;
            
         case AddGeodataset:
            addGeomdataset();
            break;
            
         case DeleteGeomdataset:
            delGeodataset(); 
            break;
            
         default:
            std::cout<<"[DBG]...This is no legal operation..."<<std::endl;    
            break;
      }
   }
}

void addVTKdataset(const std::string& vtkfilename)
{

}

void delVTKdataset()
{

}

void addGeometrydataset(const std::string& geomfilename)
{
      this->mGeomFileInfo->fileName=<const_cast<char*>(geomfilename.c_str());
      this->mMoveOldGeomDataSets = true;
      this->mMoveOldVTKDataSets = true;
      std::cout << "[DBG]....Adding Geometry files " << mGeomFileName<<std::endl;
      for(GeometryDataSetList::iterator itr = mGeomDataSets.begin(); itr != mGeomDataSets.end(); ++itr)

      {/*
         //The following need to be changed later, maybe we can get trans information from Java GUI
         this->mGeomDataSets[itr]->DCS->setTrans(, , ,); 
         
        */ 
      }
      //assume that if we move the geometry, the relative VTK dataset should be moved too.
      for(VTKDataSetList::iterator itr = mVTKDataSets.begin(); itr != mVTKDataSets.end(); ++itr)
      {
         //this->mVTKDataSets[i]->DCS->setTrans(, , ,);
      }
      
      this->mGeomDataSets.push_back( new cfdGeomSets(fileInfo *mGeomFileInfo,this->mModelDCS ) );
     /* 
      Need to find an efficient way to  
      this->mGeomeDataSets->DCS->setScale( cfdscale[0], cfdscale[1], cfdscale[2] );
      this->mGeomDataSets->DCS->setRot( cfdvrxprRot[0], cfdvrxprRot[1],cfdvrxprRot[2] );
      this->mGeometrySets->DCS->setTrans( cfdtranslate );
     */
      this->mMoveOldGeomDataSets = false;
      this->mMoveOldVTKDataSets = false;
   
}

void delGeodataset(int DelIndex)
{
   delete (mGeomDataSets[DelIndex]);
   this->mGeomDataSets.erase(this->mGeomDataSets.begin() + DelIndex);
}

