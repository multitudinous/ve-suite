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
#include <ves/xplorer/data/constraints/LinearSpringConstraintPropertySet.h>

namespace ves
{
namespace xplorer
{
namespace data
{
namespace constraints
{
////////////////////////////////////////////////////////////////////////////////
LinearSpringConstraintPropertySet::LinearSpringConstraintPropertySet()
{
    mTableName = "LinearSpring";

    RegisterPropertySet( mTableName );

    CreateSkeleton();
}
////////////////////////////////////////////////////////////////////////////////
PropertySetPtr LinearSpringConstraintPropertySet::CreateNew()
{
    return PropertySetPtr( new LinearSpringConstraintPropertySet );
}
////////////////////////////////////////////////////////////////////////////////
void LinearSpringConstraintPropertySet::CreateSkeleton()
{
    SetPropertyAttribute( "Linear", "userVisible", true );
    SetPropertyAttribute( "Linear_RestLength", "userVisible", true );
    SetPropertyAttribute( "Linear_Stiffness", "userVisible", true );
    SetPropertyAttribute( "Linear_Damping", "userVisible", true );
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}