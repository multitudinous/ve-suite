/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include <ves/xplorer/data/PropertySet.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <string>

namespace ves
{
namespace conductor
{
/*!\file VisFeatureMakerBase.h
 *
 */

/*!\class ves::conductor::VisFeatureMakerBase
 *
 */

/*!\namespace ves::conductor
 *
 */
class VisFeatureMakerBase
{
public:
    ///Constructor
    VisFeatureMakerBase();
    ///Copy constructor
    VisFeatureMakerBase( const VisFeatureMakerBase& orig );
    ///Destructor
    virtual ~VisFeatureMakerBase();
    ///Update function
    virtual void Update( unsigned int recordID );

protected:
    ///Called by the update function
    virtual void UpdateAdvancedSettings( ves::xplorer::data::PropertySet& set );
    ///Called by the update function
    void UpdateBaseInformation( ves::xplorer::data::PropertySet& set );
    ///Called by the update function
    void SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand, ves::xplorer::data::PropertySet& set );

    ///The name of the command to send back
    std::string m_commandName;
    ///The advanced settings.
    std::vector<ves::open::xml::DataValuePairPtr> m_advancedSettings;
    ///The basic information from the vistab
    std::vector<ves::open::xml::DataValuePairPtr> m_vistabBaseInformation;
    
private:

};

} // namespace conductor
} // namespace ves
