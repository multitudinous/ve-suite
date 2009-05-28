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

// --- VE-Suite Includes --- //
#include <ves/xplorer/volume/cfdUpdateParameterCallback.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>

// --- C/C++ Includes --- //
#include <iostream>

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
cfdUpdateParameterCallback::cfdUpdateParameterCallback()
{
    _value[0] = 0;
    _value[1] = 0;
    _value[2] = 0;
    _value[3] = 0;
    _type = VECTOR;
    _size = ONE;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdUpdateParameterCallback
::cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                          const osg::CopyOp &copyop )
:osg::Uniform::Callback(copy, copyop)
{
   _value[0] = 0;
   _value[1] = 0;
   _value[2] = 0;
   _value[3] = 0;
   _type = copy._type;
   _size = copy._size;
}*/
////////////////////////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::operator()( osg::Uniform* uniVar, osg::NodeVisitor* nv )
{
    if( _type == VECTOR )
    {
        switch ( _size )
        {
            case ONE:
                uniVar->set( _value[0] );
                break;
            case TWO:
                uniVar->set( osg::Vec2( _value[0], _value[1] ) );
                break;
            case THREE:
                uniVar->set( osg::Vec3( _value[0], _value[1], _value[2] ) );
                break;
            case FOUR:
            default:
                uniVar->set( osg::Vec4( _value[0], _value[1], _value[2], _value[3] ) );
                break;
        };
    }
    else if( _type == TIME )
    {
        uniVar->set(( float )nv->getFrameStamp()->getReferenceTime() );
    }
    else
    {
        std::cout << "Unknown parameter type in cfdUpdateParameterCallback." << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::updateParameter( float* v )
{
    switch ( _size )
    {
        case ONE:
            _value[0] = v[0];
            break;
        case TWO:
            _value[0] = v[0];
            _value[1] = v[1];
            break;
        case THREE:
            _value[0] = v[0];
            _value[1] = v[1];
            _value[2] = v[2];
            break;
        case FOUR:
        default:
            _value[0] = v[0];
            _value[1] = v[1];
            _value[2] = v[2];
            _value[3] = v[3];
            break;
    };
}
////////////////////////////////////////////////////////////////////////////////
