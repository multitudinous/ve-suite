/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
namespace conductor{


class ContourFeatureMaker
{
public:
    ContourFeatureMaker( );
    ContourFeatureMaker( const ContourFeatureMaker& orig );
    virtual ~ContourFeatureMaker( );
    void update( unsigned int recordID );

protected:
    void _addPlane( xplorer::data::PropertySet& set );
    void _updateContourInformation( xplorer::data::PropertySet& set );
    void _updateAdvancedSettings( xplorer::data::PropertySet& set );
    void _updateBaseInformation( xplorer::data::PropertySet& set );
    void SendUpdatedSettingsToXplorer( ves::open::xml::CommandPtr subDialogCommand, xplorer::data::PropertySet& set );
    

private:
    std::vector<ves::open::xml::DataValuePairPtr> _advancedSettings;///<The advanced settings.
    std::vector<ves::open::xml::DataValuePairPtr> _contourInformation;///<The countour setting data

    std::vector<ves::open::xml::DataValuePairPtr> _vistabBaseInformation;///<The basic information from the vistab

    std::string _commandName;///<The name of the command to send back

};

} // namespace conductor
} // namespace ves
