/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/xplorer/data/DatasetPropertySet.h>
#include <ves/xplorer/data/Property.h>

using namespace ves::xplorer::data;

DatasetPropertySet::DatasetPropertySet( )
{
    mTableName = "Dataset";
    _createSkeleton( );
}

DatasetPropertySet::DatasetPropertySet( const DatasetPropertySet& orig )
{
}

DatasetPropertySet::~DatasetPropertySet( )
{
}

void DatasetPropertySet::_createSkeleton( )
{
    AddProperty( "SurfaceWrap", false, "Surface Wrap" );

    AddProperty( "BoundingBox", false, "Bounding Box" );

    AddProperty( "ScalarBar", false, "Scalar Bar" );

    AddProperty( "Axes", false, "Axes" );
    SetPropertyAttribute( "Axes", "setExpanded", false );

    AddProperty( "Axes_XLabel", std::string( "X Axis" ), "X axis label" );

    AddProperty( "Axes_YLabel", std::string( "Y Axis" ), "Y axis label" );

    AddProperty( "Axes_ZLabel", std::string( "Z Axis" ), "Z axis label" );



    //***** The following properties should all eventually make use of
    // of userVisibile = false to hide them from the user. They are intended
    // to provide persistence and access mechanisms to dataset info without
    // having to go through xplorer directly.

    AddProperty( "Filename", std::string(""), "File name");
    SetPropertyAttribute( "Filename", "userVisible", false );

    std::vector< std::string > stringVector;
    AddProperty( "ScalarNames", stringVector, "Scalar Names" );
    SetPropertyAttribute( "ScalarNames", "userVisible", false );

    AddProperty( "VectorNames", stringVector, "Vector Names" );
    SetPropertyAttribute( "VectorNames", "userVisible", false );

    std::vector< double > doubleVector;
    AddProperty( "ScalarMins", doubleVector, "Scalar Mins" );
    SetPropertyAttribute( "ScalarMins", "userVisible", false );
    AddProperty( "ScalarMaxes", doubleVector, "Scalar Maxes" );
    SetPropertyAttribute( "ScalarMaxes", "userVisible", false );

    AddProperty( "StepLength", 0.0f, "Integration Step Length");
    SetPropertyAttribute( "StepLength", "userVisible", false );

    AddProperty( "TimeStep", 0.0f, "Integration Time Step");
    SetPropertyAttribute( "TimeStep", "userVisible", false );

    AddProperty( "MaxTime", 0.0f, "Max Integration Time" );
    SetPropertyAttribute( "MaxTime", "userVisible", false );

    AddProperty( "Type", 0, "Type" );
    SetPropertyAttribute( "Type", "userVisible", false );

    AddProperty( "PrecomputedDataSliceDir", std::string(""), "Precomputed Data Slice Dir" );
    SetPropertyAttribute( "PrecomputedDataSliceDir", "userVisible", false );

    AddProperty( "PrecomputedSurfaceDir", std::string(""), "Precomputed Surface Dir" );
    SetPropertyAttribute( "PrecomputedSurfaceDir", "userVisible", false );
}

