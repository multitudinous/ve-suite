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
#ifndef SEED_POINT_BOUNDS_EVENT_HANDLER_H
#define SEED_POINT_BOUNDS_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/ModelPtr.h>
#include <ves/open/xml/XMLObjectPtr.h>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file SPBoundEH.h
  SeedPointBoundsEventHandler API
  */
/*!\class SeedPointBoundsEventHandler
 * Update the SeedPoints Bounding box.
 */
class VE_XPLORER_EXPORTS SeedPointBoundsEventHandler : public EventHandler
{
public:
    ///Constructor
    SeedPointBoundsEventHandler();

    ///Copy Constructor
    SeedPointBoundsEventHandler( const SeedPointBoundsEventHandler& ceh );
    ///Destructor
    virtual ~SeedPointBoundsEventHandler();

    ///Equal operator
    SeedPointBoundsEventHandler& operator=( const SeedPointBoundsEventHandler& rhs );

    ///Update the bounds.
    ///\param veXMLObject The veXMLObject to execute.
    virtual void Execute( const ves::open::xml::XMLObjectPtr& veXMLObject );

    ///Set the active cfdModel
    ///\param model The active cfdModel
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model );
protected:
    ///The active model
    ves::xplorer::Model* _activeModel;

    ///Update the bounds of the SeedPoint bounding box
    ///\param bounds The bounds of the bounding box
    void UpdateAllBounds( const std::vector< double >& bounds );

};

}
}
}

#endif// TEXTURE_BASED_UPDATE_SCALAR_RANGE_EVENT_HANDLER_H
