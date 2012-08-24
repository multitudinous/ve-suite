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
#include <ves/xplorer/data/constraints/AngularSpringConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
AngularSpringConstraintPropertySet::AngularSpringConstraintPropertySet()
{
    SetTypeName( "AngularSpring" );

    RegisterPropertySet( GetTypeName() );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr AngularSpringConstraintPropertySet::CreateNew()
{
    return propertystore::PropertySetPtr( new AngularSpringConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void AngularSpringConstraintPropertySet::CreateSkeleton()
{
    SetPropertyAttribute( "ObjectA_ReferenceAxisA", "userVisible", true );
    SetPropertyAttribute( "ObjectB_ReferenceAxisB", "userVisible", true );
    SetPropertyAttribute( "Angular", "userVisible", true );
    SetPropertyAttribute( "Angular_RestAngle", "userVisible", true );
    SetPropertyAttribute( "Angular_Stiffness", "userVisible", true );
    SetPropertyAttribute( "Angular_Damping", "userVisible", true );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}
