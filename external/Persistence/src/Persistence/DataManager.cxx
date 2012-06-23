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
#include <Persistence/DataManager.h>

namespace Persistence
{
////////////////////////////////////////////////////////////////////////////////
DataManager::DataManager()
{
    m_dataMultiplexer = DataAbstractionLayerPtr( new Multiplexer );
}
////////////////////////////////////////////////////////////////////////////////
DataManager::~DataManager()
{

}
////////////////////////////////////////////////////////////////////////////////
void DataManager::ProcessBackgroundTasks()
{

}
////////////////////////////////////////////////////////////////////////////////
void DataManager::AttachStore( DataAbstractionLayerPtr store,
                               Store::StoreRole role,
                               Store::SyncMode mode )
{
    static_cast< Multiplexer* >( m_dataMultiplexer.get() )->
            AttachStore( store, role, mode );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::DetachStore( DataAbstractionLayerPtr store )
{
    static_cast< Multiplexer* >( m_dataMultiplexer.get() )->
            DetachStore( store );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::SetCache( DataAbstractionLayerPtr cache )
{
    SetChild( cache );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::SetBuffer( DataAbstractionLayerPtr buffer )
{
    m_child->SetChild( buffer );
    m_buffer = buffer;
    m_buffer->SetChild( m_dataMultiplexer );
}
////////////////////////////////////////////////////////////////////////////////
void DataManager::Buffer( std::vector< std::string > ids,
                          BufferBase::BufferPriority priority )
{
    (static_cast< Cache* >(m_child.get()))->Buffer( ids, priority );
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence
