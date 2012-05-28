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

///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for removing a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

#include <ves/xplorer/minerva/RemoveRasterLayerHandler.h>
#include <ves/xplorer/minerva/MinervaManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/util/commands/Minerva.h>

using namespace ves::xplorer::minerva;


///////////////////////////////////////////////////////////////////////////////
//
//  Constructor.
//
///////////////////////////////////////////////////////////////////////////////

RemoveRasterLayerHandler::RemoveRasterLayerHandler() : BaseClass()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Destructor.
//
///////////////////////////////////////////////////////////////////////////////

RemoveRasterLayerHandler::~RemoveRasterLayerHandler()
{
}


///////////////////////////////////////////////////////////////////////////////
//
//  Remove raster layer.
//
///////////////////////////////////////////////////////////////////////////////

void RemoveRasterLayerHandler::Execute( CommandPtr command, MinervaManager& manager )
{
    ves::open::xml::DataValuePairPtr guidData( command->GetDataValuePair( ves::util::names::UNIQUE_ID ) );

    std::string guid;
    guidData->GetData( guid );

    manager.RemoveRasterLayer( guid );

}
