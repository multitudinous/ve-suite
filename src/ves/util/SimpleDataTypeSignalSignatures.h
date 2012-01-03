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
#ifndef VES_UTIL_SIMPLE_DATA_TYPE_SIGNAL_SIGNATURE_H
#define VES_UTIL_SIMPLE_DATA_TYPE_SIGNAL_SIGNATURE_H

#include <boost/signals2/signal.hpp>

//#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

#include <vector>
#include <string>

namespace ves
{
namespace util
{
    ///Signal with no arguments and no returns
    typedef boost::signals2::signal< void () > VoidSignal_type;

    ///Signal for a String
    typedef boost::signals2::signal< void ( std::string const& ) > StringSignal_type;
    typedef boost::signals2::signal< void ( std::string const&, std::string const& ) > TwoStringSignal_type;
    typedef boost::signals2::signal< void ( std::string const&, std::string const&, std::string const& ) > ThreeStringSignal_type;

    ///Signal for a bool
    typedef boost::signals2::signal< void ( bool const& ) > BoolSignal_type;

    ///Signal for double
    typedef boost::signals2::signal< void ( double const& ) > DoubleSignal_type;
    typedef boost::signals2::signal< void ( double const&, double const& ) > TwoDoubleSignal_type;
    typedef boost::signals2::signal< void ( double const&, double const&, double const& ) > ThreeDoubleSignal_type;

    ///Signal for int
    typedef boost::signals2::signal< void ( int const& ) > IntSignal_type;
    
    ///Signal for bools and vectors
    typedef boost::signals2::signal< void ( const bool, const std::vector< double >&  ) > BoolAndDoubleVectorSignal_type;

}
}
#endif


