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

#include <propertystore/PropertySet.h>

#include <ves/xplorer/event/viz/VisFeatureMakerBasePtr.h>

#include <ves/xplorer/Logging.h>

#include <string>

namespace ves
{
namespace conductor
{
/*!\class ves::conductor::VisFeatureMakerBase
 * \file VisFeatureMakerBase.h
 *
 */
class VisFeatureMakerBase
{
public:
    ///Constructor
    VisFeatureMakerBase();
    ///Copy constructor
    VisFeatureMakerBase( const VisFeatureMakerBase& orig );
    ///Update function
    virtual void Update( const std::string& recordUUID );

protected:
    ///Destructor
    virtual ~VisFeatureMakerBase();
    ///Update the propertysets for a given feature
    void Execute( propertystore::PropertySetPtr set );
    ///Set the active vector if needed
    void SetActiveVector( propertystore::PropertySetPtr set );
    ///Set the active scalar range if needed
    void SetActiveScalarAndRange( propertystore::PropertySetPtr set );
    ///Set the active dataset
    bool SetActiveDataSet( propertystore::PropertySetPtr set );

    ///The name of the command to send back
    std::string m_commandName;
    ///Logger reference
    Poco::Logger& m_logger;
    ///Actual stream for this class
    ves::xplorer::LogStreamPtr m_logStream;

private:

};

} // namespace conductor
} // namespace ves
