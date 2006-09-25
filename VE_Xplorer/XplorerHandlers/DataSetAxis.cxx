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
 * Date modified: $Date: 2006-07-08 22:57:46 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4907 $
 * Author:        $Author: mccdo $
 * Id:            $Id: cfdScalarBarActor.cxx 4907 2006-07-09 03:57:46Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/DataSetAxis.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include <fstream>
#include <sstream>
#include <iomanip>
#include <string>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
////////////////////////////////////////////////////////////////////////////////
DataSetAxis::DataSetAxis( void )
{
   vprDEBUG(vesDBG,2) << "constructing cfdScalarBarActor" 
                          << std::endl << vprDEBUG_FLUSH;
   bbox[ 0 ] = bbox[ 2 ] = bbox[ 4 ] = 0.0f;
   bbox[ 1 ] = bbox[ 3 ] = bbox[ 5 ] = 1.0f;
   
   xAxisLabel = "X Axis";
   xAxisLabel = "Y Axis";
   xAxisLabel = "Z Axis";
   axisGroup = 0;
}
////////////////////////////////////////////////////////////////////////////////
DataSetAxis::~DataSetAxis()
{
   vprDEBUG(vesDBG,2) << "deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;

   // may note need to delete anything
   vprDEBUG(vesDBG,2) << "   finished deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::SetBoundingBox( double* inBBox )
{
   for ( size_t i = 0; i < 6; ++i )
   {
      bbox[ i ] = inBBox[ i ];
   }
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::SetAxisLabels( std::string xAxis, 
                                 std::string yAxis, 
                                 std::string zAxis )
{
   xAxisLabel = xAxis;
   yAxisLabel = yAxis;
   zAxisLabel = zAxis;
}
////////////////////////////////////////////////////////////////////////////////
void DataSetAxis::CreateAxis( void )
{
   //do all the osg stuff here
}
////////////////////////////////////////////////////////////////////////////////
cfdGroup* DataSetAxis::GetAxis( void )
{
   return axisGroup;
}
