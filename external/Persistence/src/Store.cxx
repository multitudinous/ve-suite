#include "Store.h"

namespace Persistence
{

Store::Store()
{
}

Store::~Store()
{
}

void Store::SetStoreRole( StoreRole role )
{

}

void Store::SetSyncMode( SyncMode mode )
{

}

void Store::SetAccessMode( AccessMode mode )
{

}

void Store::Attach()
{

}

void Store::Detach()
{

}

void Store::Save( const Persistable& persistable, Role role  )
{
    SaveImpl( persistable, role );
}

void Store::Load( Persistable& persistable, Role role )
{
    LoadImpl( persistable, role );
}

void Store::SaveImpl( const Persistable& persistable,
                       Role role  )
{

}

void Store::LoadImpl( Persistable& persistable,
                       Role role )
{

}


} // namespace Persistence
