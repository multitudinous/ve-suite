/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/data/BindableAnyWrapper.h>
#include <Poco/Data/Binding.h>

//#include <Poco/Data/Common.h>
#include <Poco/Data/Statement.h>
//#include <Poco/Data/SQLite/Connector.h>

#include <iostream>

using namespace ves::xplorer::data;

BindableAnyWrapper::BindableAnyWrapper( )
{
}

BindableAnyWrapper::BindableAnyWrapper( const BindableAnyWrapper& orig )
{
}

BindableAnyWrapper::~BindableAnyWrapper( )
{
}

bool BindableAnyWrapper::BindValue( Poco::Data::Statement* statement,
                                   boost::any value )
{
    bool returnValue = false;

    if( value.type( ) == typeid ( bool ) )
    {
        mBool = boost::any_cast< bool >( value );
        (*statement), Poco::Data::use( mBool );
        returnValue = true;
    }
    else if( value.type( ) == typeid (int ) )
    {
        mInt = boost::any_cast< int >( value );
        (*statement), Poco::Data::use( mInt );
        returnValue = true;
    }
    else if( value.type( ) == typeid (float ) )
    {
        mFloat = boost::any_cast< float >( value );
        (*statement), Poco::Data::use( mFloat );
        returnValue = true;
    }
    else if( value.type( ) == typeid (double ) )
    {
        mDouble = boost::any_cast< double >( value );
        (*statement), Poco::Data::use( mDouble );
        returnValue = true;
    }
    else if( boost::any_cast< std::string > ( &value ) )
    {
        mString = boost::any_cast< std::string > ( value );
        (*statement), Poco::Data::use( mString );
        returnValue = true;
    }

    return returnValue;
}

