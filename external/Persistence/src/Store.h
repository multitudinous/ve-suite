#pragma once

#include "DataAbstractionLayer.h"

namespace Persistence
{

class Store : public DataAbstractionLayer
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
