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
 * Date modified: $Date: 2006-07-08 22:04:36 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4905 $
 * Author:        $Author: mccdo $
 * Id:            $Id: cfdScalarBarActor.h 4905 2006-07-09 03:04:36Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef DATASET_AXIS_H
#define DATASET_AXIS_H

#include <string>
#include <vector>
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGeode;
   class cfdGroup;
}
class vtkLookupTable;
class vtkVectorText;

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdDataSet;
   class cfdReadParam;
}

//! osg dataset axis
/*!
  Renders an axis for a given dataset
*/
namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS DataSetAxis : public cfdGlobalBase
{
public:
   DataSetAxis( void );
   ~DataSetAxis();

   /// compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray ){ return true; }
   /// in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand(){ ; }
   /// Create the scalar bar
   void SetBoundingBox( double* inBBox );
   /// Set the active dataset for scalar bar computations
   void SetAxisLabels( std::string xAxis, std::string yAxis, std::string zAxis );
   /// Read parameter file
   void CreateAxis( void );
   /// Get the axis that was created
   VE_SceneGraph::cfdGroup* GetAxis( void );
   
private:
   double bbox[ 6 ];
   std::string xAxisLabel;
   std::string yAxisLabel;
   std::string zAxisLabel;
   VE_SceneGraph::cfdGroup* axisGroup;
};
}
#endif
