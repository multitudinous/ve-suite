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

#include <Persistence/Store.h>
#include "mongo/client/dbclient.h"

namespace Persistence
{

class PERSISTENCE_EXPORT MongoStore : public Store
{
public:
    MongoStore();
    ~MongoStore();

    void SetStorePath( const std::string& path );

    /**
     * Checks whether table with @c typeName exists in the current database.
     * @param typeName Name of table to check for
     * @return @c true if table exists, @c false otherwise.
     */
    bool HasTypename( const std::string& typeName );

    virtual void Attach();

    /**
      * Attempts clean shutdown of database connections and deletes the
      * working db file.
      */
    virtual void Detach();

    virtual void Remove( Persistable& persistable );

    virtual bool HasIDForTypename( const boost::uuids::uuid& id, const std::string& typeName );

    virtual void GetIDsForTypename( const std::string& typeName,
                                    std::vector< std::string >& resultIDs );

    virtual void Search( const std::string& typeName,
                         std::vector< SearchCriterion >& criteria,
                         const std::string& returnField,
                         std::vector< std::string >& results );

    virtual void ProcessBackgroundTasks();

    //virtual void SetChild( DataAbstractionLayerPtr child );

    virtual void Drop( const std::string& typeName, Role role = DEFAULT_ROLE  );

    void MapReduce( const std::string& typeName,
                    const std::string& jsMapFunction,
                    const std::string& jsReduceFunction,
                    mongo::BSONObj queryObj,
                    const std::string& outputUUID,
                    const std::string& outputcollection = ""  );

protected:
    virtual void SaveImpl( const Persistable& persistable,
                           Role role = DEFAULT_ROLE  );

    virtual void LoadImpl( Persistable& persistable,
                           Role role = DEFAULT_ROLE );

private:

    unsigned int GetBoostAnyVectorSize( const boost::any& value );

    /// Holds the current db path
    std::string m_path;

    mongo::DBClientConnection* m_connection;
};

} // namespace Persistence
