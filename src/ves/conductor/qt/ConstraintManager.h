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
#pragma once
#include <ves/VEConfig.h>

#include <propertystore/PropertySetPtr.h>

#include <ves/xplorer/Logging.h>
#include <switchwire/ScopedConnectionList.h>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>

#include <string>
#include <vector>
#include <map>

// Forward declarations
namespace ves
{
namespace xplorer
{
namespace data
{
class PropertySet;
} // data
} // xplorer
} // ves

namespace ves
{
namespace conductor
{
/*!\file ConstraintManager.h
 * \class ves::conductor::ConstraintManager
 * \namespace ves::conductor
 *
 */

class VE_CONDUCTOR_QTUI_EXPORTS ConstraintManager : boost::noncopyable
{
public:
    /**
     * Factory function to create constraint property sets by name. If you add a new
     * constraint type, add a new branch to the big if/else statement in this method.
     *
     * @param contraintType Unique name associated with the class to create. This
     * will generally be a human-readable name rather than directly a class name; eg.
     * passing "LinearSpring" will create an instance of LinearSpringConstraintPropertySet.
     *
     * @exception none Does not throw.
     *
     * @return Pointer to the created PropertySet. If no matching feature exists,
     * the returned pointer will be a null pointer. The caller is expected to
     * manage the lifetime of the created object.
     **/
    propertystore::PropertySetPtr CreateNewConstraint( const std::string& contraintType );


    std::vector< std::pair< std::string, std::string > >
    GetNameIDPairsForConstraint( const std::string& contraintType );

private:

    ///Set this class up as a singleton
    /// Constructor
    ConstraintManager();

    ///Destructor
    virtual ~ConstraintManager();

    ///Singleton declarations
    vprSingletonHeader( ConstraintManager );

    ///Reloads all viz features from the database. This slot is connected to
    ///"DatabaseManager.ResyncFromDatabase"
    void ResyncFromDatabase();

    ///Map to hold a mapping between feature names and table names
    std::map< std::string, std::string > m_constraintNameToTableName;

    std::map< std::string, propertystore::PropertySetPtr > m_constraintTypeToSetPtrMap;

    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;

    ///Manages slot connections
    switchwire::ScopedConnectionList m_connections;

};
} // namespace conductor
} // namespace ves
