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

#include <Persistence/DataAbstractionLayer.h>

namespace Persistence
{

class PERSISTENCE_EXPORT Store : public DataAbstractionLayer
{
public:
    // These are binary flags: DEFAULTSTORE_ROLE is intended to be a combination
    // of WORKINGSTORE_ROLE and BACKINGSTORE_ROLE
    enum StoreRole { WORKINGSTORE_ROLE = 1, BACKINGSTORE_ROLE = 2,
                     DEFAULTSTORE_ROLE = 3, VERSIONINGSTORE_ROLE = 4 };

    enum SyncMode { SYNCHRONOUS_MODE, ASYNCHRONOUS_MODE };

    // Read/write access mode flags.
    enum AccessMode { READ_MODE = 1, WRITE_MODE = 2 };

    Store();

    virtual ~Store();

    virtual void SetStoreRole( StoreRole role );
    virtual void SetSyncMode( SyncMode mode );
    virtual void SetAccessMode( AccessMode mode );

    virtual void Attach();
    virtual void Detach();

    /// Override from DataAbstractionLayer. This method must not be overridden
    /// by derived classes, which should instead override SaveImpl.
    void Save( const Persistable& persistable, Role role = DEFAULT_ROLE  );

    /// Override from DataAbstractionLayer. This method must not be overridden
    /// by derived classes, which should instead override LoadImpl.
    void Load( Persistable& persistable, Role role = DEFAULT_ROLE );


protected:
    /// Derived classes should override this to do their actual save operation
    virtual void SaveImpl( const Persistable& persistable,
                           Role role = DEFAULT_ROLE  );

    /// Derived classes should override this to do their actual load operation
    virtual void LoadImpl( Persistable& persistable,
                           Role role = DEFAULT_ROLE );

private:
    StoreRole m_storeRole;
    SyncMode m_storeMode;
    AccessMode m_accessMode;
};

} // namespace Persistence
