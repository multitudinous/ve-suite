/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef CHANGE_BACKGROUND_COLOR_EVENT_HANDLER_H
#define CHANGE_BACKGROUND_COLOR_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file ChangeBackgroundColorEventHandler.h
  ChangeBackgroundColorEventHandler API
  */
/*!\class ChangeBackgroundColorEventHandler
 * Class for changing background color in xplorer.
 */
class ChangeBackgroundColorEventHandler : public EventHandler
{
public:
    ///Constructor
    ChangeBackgroundColorEventHandler();

    ///Copy Constructor
    ChangeBackgroundColorEventHandler( const ChangeBackgroundColorEventHandler& ceh );

    ///Destructor
    virtual ~ChangeBackgroundColorEventHandler();

    ///Set the cfdModel.
    ///\param model The ModelHandler to execute the Command on.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

    ///Equal operator
    ChangeBackgroundColorEventHandler& operator=( const ChangeBackgroundColorEventHandler& rhs );

    ///Update background color
    void UpdateBackgroundColor( const bool enable, const std::vector< double >& color );

protected:
};

}
}
}
#endif// CHANGE_WORKING_DIRECTORY_EVENT_HANDLER_H
