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
 * File:          $RCSfile: cfdModel.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef CFD_MODEL_H
#define CFD_MODEL_H

/*

1.The difference between the concept of multiple models and multiple datasets is that:

A model can contain several vtkDataSets, geometricalDataSets (surface dataset). These datasets have the same operations. For example, for the same shape design, if we want to see the effects of different boundary condition, we can put two different cases into the same model, so the comparision can be made very easily.But if we want to see the two different shape design or need to see the difference between the experiment results and the simulation results, it is better to treat these two dataset as two different models.
 
*/

#include "cfdFileInfo.h"
#include "cfdDataSets.h"
#include "cfdGeomDataSets.h"

enum ModelTypeIndex
{
   SimulationModel=1,
   ExperimentalModel,
   DesignModel,
   GeometricalModel
};

enum Operation2Model
{
   AddVTKdataset=1,
   DelVTKdataset,
   AddGeodataset,
   DelGeodataset
};

class cfdModel
{
   public:
      cfdModel(pfDCS *);
      ~cfdModel();
      
      void setModelNode( pfNode * );
      void setModelType( int );//four type models right now (experiment, simulation, design, and geometry)
            
      void setTrans3( float x, float y, float z );
      void setTrans( float trans[3] );
      void setScale( float x,float y, float z );
      void setRot(float,float,float);
      void setRotMat(double *);

      void updateCurModel();//handling the add or delete the vtkdateset or geomdataset
      void addVTKdataset();
      void delVTKdataset();
      void addGeomdataset(const std::string &filename);
      void delGeomdataset(int);
      
   private:
      typedef std::vector<cfdGeomDataSets *> GeometoryDataSetList;
      GeometoryDataSetList mGeomDataSets;
      typedef std::vector<cfdDataSet *> VTKdataSetList;
      VTKDataSetList mVTKDataSets;
      
      pfDCS *mModelDCS;
      class fileInfo mGeomFileInfo,mVTKFileInfo;
   
      
      //the information for following three variables should be transfered from cfdApp
      ModelTypeIndex mModelType;
      Operation2Model mActiveOperation2Model;
      
      bool mUpdateModelFlag;
      bool mMoveOldGeomDataSets;
      bool mMoveOldVTKDataSets;
   	
   
};

#endif
