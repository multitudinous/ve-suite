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

#include <string>

#include <ves/open/xml/CommandPtr.h>

namespace iaf
{
namespace scenario
{
class Scenario
{
public:
    ///Constructor
    Scenario();
    ///Destructor
    ~Scenario();
    ///Copy Constructor
    Scenario( const Scenario& );
    ///equal operator
    Scenario& operator= ( const Scenario& );
    
    void SetTillage( std::string const& tillage );
    std::string& GetTillage();

    void SetRotation( std::string const& rotation );
    std::string& GetRotation();

    void SetRemovalRate( std::string const& rate );
    std::string& GetRemovalRate();
    
    void SetClimateModel( std::string const& model );
    std::string& GetClimateModel();
    
    void SetLocation( std::string const& location );
    std::string& GetLocation();
    
    ves::open::xml::CommandPtr GetXMLScenario();

private:
    ///The tillage operations
    std::string m_tillage;
    ///The crop roation scheme
    std::string m_rotation;
    ///The crop removal rate
    std::string m_removalRate;
    ///The climate model being used
    std::string m_climateModel;
    ///The farm/selection/map unit location
    std::string m_location;
};
}
}
