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

#include <Persistence/DataAbstractionLayerPtr.h>
#include <Persistence/Persistable.h>
#include <Persistence/SearchCriterion.h>

namespace Persistence
{

class PERSISTENCE_EXPORT DataAbstractionLayer
{
public:
    DataAbstractionLayer();
    virtual ~DataAbstractionLayer();

    /// Persistables can be used in two roles:
    enum Role{ WORKING_ROLE = 1,
               BACKING_ROLE = 2,
               DEFAULT_ROLE = 3,
               VERSIONING_ROLE = 4 };

    /// Saves persistable to data store. Unless a version branch is specifically
    /// being tagged, role should be left to the default.
    virtual void Save( const Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Loads persistable from data store.
    virtual void Load( Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Removes persistable from data store.
    virtual void Remove( Persistable& persistable, Role role = DEFAULT_ROLE );

    /// Does this DAL object have a datum with this ID?
    virtual bool HasIDForTypename( const boost::uuids::uuid& id, const std::string& typeName );

    /// Gets all available IDs for a given type.
    virtual void GetIDsForTypename( const std::string& typeName,
                                    std::vector< std::string >& resultIDs );

    /// Gets all available IDs for a given type where criteria are met.
    virtual void Search( const std::string& typeName,
                         std::vector< SearchCriterion >& criteria,
                         const std::string& returnField,
                         std::vector< std::string >& results );


    /// Gives the DAL a chance to do background tasks such as buffering data,
    /// performing heuristic analysis, building indices, etc.
    virtual void ProcessBackgroundTasks();

    /// Sets the store-side child of this DAL. Calls to all other methods in
    /// this interface should be forwarded to the store-side child after any
    /// local processing occurs.
    virtual void SetChild( DataAbstractionLayerPtr child );

    /// Delete all records with typename typeName in all stores of with role Role
    virtual void Drop( const std::string& typeName, Role role = DEFAULT_ROLE  );

protected:
    /// Holds the store-side child of this DAL
    DataAbstractionLayerPtr m_child;
};

} // namespace Persistence
