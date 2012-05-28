/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/xplorer/scenegraph/util/AnimationNonInterpolatedPath.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
////////////////////////////////////////////////////////////////////////////////
AnimationNonInterpolatedPath::AnimationNonInterpolatedPath()
    :
    osg::AnimationPath()
{

}
////////////////////////////////////////////////////////////////////////////////
AnimationNonInterpolatedPath::~AnimationNonInterpolatedPath()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool AnimationNonInterpolatedPath::getInterpolatedControlPoint( double time, ControlPoint& controlPoint ) const
{
    if( _timeControlPointMap.empty() )
    {
        return false;
    }

    switch( _loopMode )
    {
    case( SWING ):
    {
        double modulated_time = ( time - getFirstTime() ) / ( getPeriod() * 2.0 );
        double fraction_part = modulated_time - floor( modulated_time );
        if( fraction_part > 0.5 )
        {
            fraction_part = 1.0 - fraction_part;
        }

        time = getFirstTime() + ( fraction_part * 2.0 ) * getPeriod();
        break;
    }
    case( LOOP ):
    {
        double modulated_time = ( time - getFirstTime() ) / getPeriod();
        double fraction_part = modulated_time - floor( modulated_time );
        time = getFirstTime() + fraction_part * getPeriod();
        break;
    }
    case( NO_LOOPING ):
        // no need to modulate the time.
        break;
    }



    TimeControlPointMap::const_iterator second = _timeControlPointMap.lower_bound( time );
    if( second == _timeControlPointMap.begin() )
    {
        controlPoint = second->second;
    }
    else if( second != _timeControlPointMap.end() )
    {
        TimeControlPointMap::const_iterator first = second;
        --first;

        controlPoint = first->second;
    }
    else // (second==_timeControlPointMap.end())
    {
        controlPoint = _timeControlPointMap.rbegin()->second;
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
}

