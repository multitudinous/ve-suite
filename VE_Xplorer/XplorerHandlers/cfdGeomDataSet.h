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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GEOMDATASET_H
#define CFD_GEOMDATASET_H
/*!\file cfdGeomDataSet.h
cfdGeomDataSet API
*/
/*!\class VE_Xplorer::cfdGeomDataSet
* 
*/
#include "VE_Xplorer/SceneGraph/DCS.h"

#include "VE_Xplorer/SceneGraph/cfdFileInfo.h"

#ifdef _PERFORMER
#include "Performer/pf/pfNode.h"
#include "Performer/pf/pfDCS.h"
//class pfNode;
class pfMaterial;
//class pfNode;
class pfLightModel;
//class pfDCS;
#elif _OSG
#include <osg/ref_ptr>

#include <osg/Node>
#include <osg/MatrixTransform>
#include <osg/Material>
#endif

#include <vector>

namespace VE_SceneGraph
{
   class DCS;
	class CADEntityHelper;
}

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
namespace VE_Xplorer
{
   class cfdGeomDataSet 
   {
      public:
			cfdGeomDataSet(VE_SceneGraph::fileInfo *geomfile, VE_SceneGraph::DCS* );

         cfdGeomDataSet( float, float [ 3 ], std::string );
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
         //void TravNodeMaterial(CADEntityHelper*,osg::Material*,int);
      #endif
         //these need to renamed!!
         VE_SceneGraph::DCS* getpfDCS();
         VE_SceneGraph::CADEntityHelper* getpfNode();
         VE_SceneGraph::CADEntityHelper* node;
         osg::ref_ptr< VE_SceneGraph::DCS > dcs;

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
}
#endif
