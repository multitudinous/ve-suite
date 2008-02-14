/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/OneDDoubleArray.h>
#include <ves/open/xml/OneDIntArray.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/open/xml/ThreeDDoubleArray.h>
#include <ves/open/xml/ThreeDIntArray.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/TwoDDoubleArray.h>
#include <ves/open/xml/TwoDIntArray.h>
#include <ves/open/xml/User.h>

using namespace ves::open::xml;
/////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLCreator::CreateNewXMLObject( const std::string& objectType )
{
    if( objectType == "FloatArray" )
    {
        return new FloatArray();
    }
    else if( objectType == "Transform" )
    {
        return new Transform();
    }
    else if( objectType == "Command" )
    {
        return new Command();
    }
    else if( objectType == "vecommand" )
    {
        return new Command();
    }
    else if( objectType == "DataValuePair" )
    {
        return new DataValuePair();
    }
    else if( objectType == "OneDDoubleArray" )
    {
        return new OneDDoubleArray();
    }
    else if( objectType == "OneDIntArray" )
    {
        return new OneDIntArray();
    }
    else if( objectType == "OneDStringArray" )
    {
        return new OneDStringArray();
    }
    else if( objectType == "ParameterBlock" )
    {
        return new ParameterBlock();
    }
    else if( objectType == "StateInfo" )
    {
        return new StateInfo();
    }
    else if( objectType == "ThreeDDoubleArray" )
    {
        return new ThreeDDoubleArray();
    }
    else if( objectType == "Transform" )
    {
        return new Transform();
    }
    else if( objectType == "TwoDDoubleArray" )
    {
        return new TwoDDoubleArray();
    }
    else if( objectType == "TwoDIntArray" )
    {
        return new TwoDIntArray();
    }
    else if( objectType == "ThreeDIntArray" )
    {
        return new ThreeDIntArray();
    }
    else if( objectType == "User" )
    {
        return new User();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLCreator::CreateNewXMLObjectCopy( const std::string& objectType,
                                               const XMLObjectPtr& objectToCopy )
{
    if( objectType == "FloatArray" )
    {
        return FloatArrayPtr( objectToCopy );
    }
    else if( objectType == "Command" )
    {
        return CommandPtr( objectToCopy );
    }
    else if( objectType == "vecommand" )
    {
        return CommandPtr( objectToCopy );
    }
    else if( objectType == "DataValuePair" )
    {
        return DataValuePairPtr( objectToCopy );
    }
    else if( objectType == "OneDDoubleArray" )
    {
        return OneDDoubleArrayPtr( objectToCopy );
    }
    else if( objectType == "OneDIntArray" )
    {
        return OneDIntArrayPtr( objectToCopy );
    }
    else if( objectType == "OneDStringArray" )
    {
        return OneDStringArrayPtr( objectToCopy );
    }
    else if( objectType == "ParameterBlock" )
    {
        return ParameterBlockPtr( objectToCopy );
    }
    else if( objectType == "StateInfo" )
    {
        return StateInfoPtr( objectToCopy );
    }
    else if( objectType == "ThreeDDoubleArray" )
    {
        return ThreeDDoubleArrayPtr( objectToCopy );
    }
    else if( objectType == "ThreeDIntArray" )
    {
        return ThreeDIntArrayPtr( objectToCopy );
    }
    else if( objectType == "Transform" )
    {
        return TransformPtr( objectToCopy );
    }
    else if( objectType == "TwoDDoubleArray" )
    {
        return TwoDDoubleArrayPtr( objectToCopy );
    }
    else if( objectType == "TwoDIntArray" )
    {
        return TwoDIntArrayPtr( objectToCopy );
    }
    else if( objectType == "User" )
    {
        return UserPtr( objectToCopy );
    }
    return ves::open::xml::XMLObjectPtr();
}


