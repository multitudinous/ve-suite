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
 * File:          $RCSfile: cfdGeomDataSet.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GEOMDATASET_H
#define CFD_GEOMDATASET_H

#include <vector>
#include "cfdFileInfo.h"
#ifdef _PERFORMER
#include "Performer/pf/pfNode.h"
#include "Performer/pf/pfDCS.h"
//class pfNode;
class pfMaterial;
//class pfNode;
class pfLightModel;
//class pfDCS;
#elif _OSG
#include <osg/Node>
#include <osg/MatrixTransform>
#include <osg/Material>
#endif
#include "cfdNode.h"
#include "cfdDCS.h"

//! CFD shell model loader
/*!
A reader that reads any data files in vtk format by changing
the inherit dataset type.  Examples data sets are:
vtkPolyDataReader, vtkUnstructuredGridReader, etc. 
The output of this reader is a single vtkPolyData data object
that has been translated into pfGeode and add into the 
pfDCS. This class is good for other types of data that
have been supported by VTK.
*/
class cfdGeomDataSet 
{
 public:
   cfdGeomDataSet(fileInfo *geomfile, cfdDCS *);

  cfdGeomDataSet( float, float [ 3 ], char * );
  ~cfdGeomDataSet( );

  /*!
    Initialize the data sets to be loaded. And render using
    VTK functions and translate into pfGeode object and 
    loaded into pfDCS.
  */
  void Initialize(float);
  void setTrans3( float x, float y, float z );
  void setTrans( float trans[3] );
  void setScl( float x,float y, float z );
  void setRot(float,float,float);
  void setRotMat(double *);
#ifdef _PERFORMER
  void pfTravNodeMaterial(pfNode*, pfMaterial*, int );
#elif _OSG
  //void TravNodeMaterial(cfdNode*,osg::Material*,int);
#endif
  /*!
    Get the pfDCS.
  */
  /*
  pfDCS * getpfDCS( );
  pfNode * getpfNode( );
  pfNode *node;//[3];
  pfDCS *DCS;//[3];
  
  

  */
  //these need to renamed!!
   cfdDCS* getpfDCS();
   cfdNode* getpfNode();
   cfdNode* node;
   cfdDCS* DCS;

#ifdef _PERFORMER
  pfLightModel *matLight;
  pfMaterial *mat1, *mat0;
  pfMaterial *fmaterial;
  pfMaterial *bmaterial;
#elif _OSG
   osg::Material* mat1, *mat0;
   osg::Material* fmaterial;
   osg::Material* bmaterial;
#endif

  //typedef std::vector< pfMaterial *> matlist;
  //matlist matList;
  void setOpac(float op_val);
  float getOpacity();
  int mat_count;
  int color;
  int transparent;
  float *stlColor;
 private:
  float trans[3];
  float scale[3];
  float op;
  
  };

#endif
