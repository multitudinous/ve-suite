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
#include <ves/xplorer/data/constraints/HingeConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
HingeConstraintPropertySet::HingeConstraintPropertySet()
{
    SetTypeName( "HingeConstraint" );

    RegisterPropertySet( GetTypeName() );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr HingeConstraintPropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new HingeConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void HingeConstraintPropertySet::CreateSkeleton()
{
    AddProperty( "IgnoreCollisions", true, "Ignore Collisions" );

    AddProperty( "Bounce", 0, "Bounce (%)" );
    SetPropertyAttribute( "Bounce", "minimumValue", 0 );
    SetPropertyAttribute( "Bounce", "maximumValue", 100 );

    AddProperty( "AngularLimit", false, "Angular Limit" );
    AddProperty( "AngularLimit_From", 0, "From" );
    AddProperty( "AngularLimit_To", 180, "To" );

    AddProperty( "ObjectA_ReferenceAxisA", std::string(""), "Reference Axis A" );
    std::vector<std::string> enumValues;
    enumValues.push_back( "X of Object" );
    enumValues.push_back( "Y of Object" );
    enumValues.push_back( "Z of Object" );
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "enumValues", enumValues );
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "userVisible", false );

    AddProperty( "ObjectB_ReferenceAxisB", std::string(""), "Reference Axis B" );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "enumValues", enumValues );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "userVisible", false );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
