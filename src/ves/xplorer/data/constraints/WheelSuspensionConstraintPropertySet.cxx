/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/data/constraints/WheelSuspensionConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
WheelSuspensionConstraintPropertySet::WheelSuspensionConstraintPropertySet()
{
    mTableName = "WheelSuspensionConstraint";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr WheelSuspensionConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new WheelSuspensionConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void WheelSuspensionConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "SteeringAngle", 0, "Steering Angle" );

    AddProperty( "RestLength", 100, "Suspension Rest Position" );
    SetPropertyAttribute( "RestLength", "minimumValue", 0 );

    AddProperty( "Stiffness", 1, "Suspension Stiffness" );
    SetPropertyAttribute( "Stiffness", "minimumValue", 0 );

    AddProperty( "Damping", 20, "Suspension Damping (%)" );
    SetPropertyAttribute( "Damping", "minimumValue", 0 );
    SetPropertyAttribute( "Damping", "maximumValue", 100 );

    AddProperty( "LowerLimitY", false, "Lower Limit Y" );
    AddProperty( "LowerLimitY_Limit", -200, "From" );
    AddProperty( "UpperLimitY", false, "Upper Limit Y" );
    AddProperty( "UpperLimitY_Limit", 200, "To" );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
