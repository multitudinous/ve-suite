/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/open/xml/TwoDStringArray.h>
#include <ves/open/xml/User.h>

using namespace ves::open::xml;
/////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLCreator::CreateNewXMLObject( const std::string& objectType )
{
    XMLObjectPtr tmp = XMLObjectPtr();

    if( objectType == "FloatArray" )
    {
        tmp = FloatArrayPtr( new FloatArray() );
    }
    else if( objectType == "Transform" )
    {
        tmp = TransformPtr( new Transform() );
    }
    else if( objectType == "Command" )
    {
        tmp = CommandPtr( new Command() );
    }
    else if( objectType == "vecommand" )
    {
        tmp = CommandPtr( new Command() );
    }
    else if( objectType == "DataValuePair" )
    {
        tmp = DataValuePairPtr( new DataValuePair() );
    }
    else if( objectType == "OneDDoubleArray" )
    {
        tmp = OneDDoubleArrayPtr( new OneDDoubleArray() );
    }
    else if( objectType == "OneDIntArray" )
    {
        tmp = OneDIntArrayPtr( new OneDIntArray() );
    }
    else if( objectType == "OneDStringArray" )
    {
        tmp = OneDStringArrayPtr( new OneDStringArray() );
    }
    else if( objectType == "ParameterBlock" )
    {
        tmp = ParameterBlockPtr( new ParameterBlock() );
    }
    else if( objectType == "StateInfo" )
    {
        tmp = StateInfoPtr( new StateInfo() );
    }
    else if( objectType == "ThreeDDoubleArray" )
    {
        tmp = ThreeDDoubleArrayPtr( new ThreeDDoubleArray() );
    }
    else if( objectType == "Transform" )
    {
        tmp = TransformPtr( new Transform() );
    }
    else if( objectType == "TwoDDoubleArray" )
    {
        tmp = TwoDDoubleArrayPtr( new TwoDDoubleArray() );
    }
    else if( objectType == "TwoDIntArray" )
    {
        tmp = TwoDIntArrayPtr( new TwoDIntArray() );
    }
	else if( objectType == "TwoDStringArray" )
    {
        tmp = TwoDStringArrayPtr( new TwoDStringArray() );
    }
    else if( objectType == "ThreeDIntArray" )
    {
        tmp = ThreeDIntArrayPtr( new ThreeDIntArray() );
    }
    else if( objectType == "User" )
    {
        tmp = UserPtr( new User() );
    }
    return tmp;
}
////////////////////////////////////////////////////////////////////////////
XMLObjectPtr XMLCreator::CreateNewXMLObjectCopy( const std::string& objectType,
                                               const XMLObjectPtr& objectToCopy )
{
    XMLObjectPtr tmp = XMLObjectPtr();
    if( objectType == "FloatArray" )
    {
        tmp = FloatArrayPtr( new FloatArray(
            *( boost::dynamic_pointer_cast<FloatArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "Command" )
    {
        tmp = CommandPtr( new Command(
            *( boost::dynamic_pointer_cast<ves::open::xml::Command>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "vecommand" )
    {
        tmp = CommandPtr( new Command(
            *( boost::dynamic_pointer_cast<ves::open::xml::Command>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "DataValuePair" )
    {
        tmp = DataValuePairPtr( new DataValuePair(
            *( boost::dynamic_pointer_cast<DataValuePair>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "OneDDoubleArray" )
    {
        tmp = OneDDoubleArrayPtr( new OneDDoubleArray(
            *( boost::dynamic_pointer_cast<OneDDoubleArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "OneDIntArray" )
    {
        tmp = OneDIntArrayPtr( new OneDIntArray(
            *( boost::dynamic_pointer_cast<OneDIntArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "OneDStringArray" )
    {
        tmp = OneDStringArrayPtr( new OneDStringArray(
            *( boost::dynamic_pointer_cast<OneDStringArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "ParameterBlock" )
    {
        tmp = ParameterBlockPtr( new ParameterBlock(
            *( boost::dynamic_pointer_cast<ParameterBlock>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "StateInfo" )
    {
        tmp = StateInfoPtr( new StateInfo(
            *( boost::dynamic_pointer_cast<StateInfo>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "ThreeDDoubleArray" )
    {
        tmp = ThreeDDoubleArrayPtr( new ThreeDDoubleArray(
            *( boost::dynamic_pointer_cast<ThreeDDoubleArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "ThreeDIntArray" )
    {
        tmp = ThreeDIntArrayPtr( new ThreeDIntArray(
            *( boost::dynamic_pointer_cast<ThreeDIntArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "Transform" )
    {
        tmp = TransformPtr( new Transform(
            *( boost::dynamic_pointer_cast<Transform>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "TwoDDoubleArray" )
    {
        tmp = TwoDDoubleArrayPtr( new TwoDDoubleArray(
            *( boost::dynamic_pointer_cast<TwoDDoubleArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "TwoDIntArray" )
    {
        tmp = TwoDIntArrayPtr( new TwoDIntArray(
            *( boost::dynamic_pointer_cast<TwoDIntArray>( 
			objectToCopy ) ) ) );
    }
	else if( objectType == "TwoDStringArray" )
    {
        tmp = TwoDStringArrayPtr( new TwoDStringArray(
            *( boost::dynamic_pointer_cast<TwoDStringArray>( 
			objectToCopy ) ) ) );
    }
    else if( objectType == "User" )
    {
        tmp = UserPtr( new User(
            *( boost::dynamic_pointer_cast<User>( 
			objectToCopy ) ) ) );
    }
    return tmp;
}


