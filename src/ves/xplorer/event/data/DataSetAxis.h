/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef DATASET_AXIS_H
#define DATASET_AXIS_H

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/Group.h>

#include <osg/Geode>

#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
   class Group;
}
}
}

namespace ves
{
namespace xplorer
{
/*!\file DataSetAxis.h
DataSetAxis API
*/
/*!\class ves::xplorer::DataSetAxis
*   Renders an axis for a given dataset
*/
class VE_XPLORER_EXPORTS DataSetAxis : public GlobalBase
{
public:
   ///Constructor
   DataSetAxis( void );
   ///Destructor
   virtual ~DataSetAxis( void );
   /// in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand(){ ; }
   /// Create the scalar bar
   void SetBoundingBox( double* inBBox );
   /// Set the active dataset for scalar bar computations
   void SetAxisLabels( std::string xAxis, std::string yAxis, std::string zAxis );
   /// Read parameter file
   void CreateAxis( void );
   /// Get the axis that was created
   ves::xplorer::scenegraph::Group* GetAxis( void );
   ///Create the labels for the axes
   ///\param terxtIn The text for the axis
   ///\param x the x location for the label
   ///\param y the y location for the label
   ///\param z the z location for the label
   osg::ref_ptr< osg::Geode > CreateAxisLabels( std::string textIn, double x, double y, double z );
   ///Create the lines to represent the axes
   osg::ref_ptr< osg::Geode > CreateAxisLines( void );

private:
   double bbox[ 6 ]; ///<Bounding box
   std::string xAxisLabel; ///<X axis label
   std::string yAxisLabel; ///<Y axis label
   std::string zAxisLabel; ///<Z axis label
   osg::ref_ptr< ves::xplorer::scenegraph::Group > axisGroup; ///<Do not know what this is
};
}
}
#endif
