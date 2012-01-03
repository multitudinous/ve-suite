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
#include <ves/conductor/qt/ConstraintManager.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventManager.h>

#include <ves/xplorer/data/constraints/AngularSpringConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/LinearAndAngularSpringConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/LinearSpringConstraintPropertySet.h>

#include <ves/xplorer/data/constraints/BallAndSocketConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/BoxConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/CardanConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/FixedConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/HingeConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/PlanarConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/RagdollConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/SliderConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/TwistSliderConstraintPropertySet.h>
#include <ves/xplorer/data/constraints/WheelSuspensionConstraintPropertySet.h>

namespace ves
{
namespace conductor
{

vprSingletonImp( ConstraintManager );
////////////////////////////////////////////////////////////////////////////////
ConstraintManager::ConstraintManager()
    :
    m_logger( Poco::Logger::get("conductor.ConstraintManager") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    CONNECTSIGNALS_0( "%ResyncFromDatabase", void(),
                      &ConstraintManager::ResyncFromDatabase,
                      m_connections, any_SignalType, high_Priority );

    using namespace ves::xplorer::data;
    using namespace ves::xplorer::data::constraints;

    // ADD ANY NEW CONSTRAINT TYPES HERE OR THE UI WILL NOT BE ABLE TO DISPLAY
    // THEM!
    m_constraintTypeToSetPtrMap["Angular Spring"] = PropertySetPtr( new AngularSpringConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Linear and Angular Spring"] = PropertySetPtr( new LinearAndAngularSpringConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Linear Spring"] = PropertySetPtr( new LinearSpringConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Ball and Socket"] = PropertySetPtr( new BallAndSocketConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Box"] = PropertySetPtr( new BoxConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Cardan"] = PropertySetPtr( new CardanConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Fixed"] = PropertySetPtr( new FixedConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Hinge"] = PropertySetPtr( new HingeConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Planar"] = PropertySetPtr( new PlanarConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Ragdoll"] = PropertySetPtr( new RagdollConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Slider"] = PropertySetPtr( new SliderConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Twist Slider"] = PropertySetPtr( new TwistSliderConstraintPropertySet() );
    m_constraintTypeToSetPtrMap["Wheel Suspension"] = PropertySetPtr( new WheelSuspensionConstraintPropertySet() );
}
////////////////////////////////////////////////////////////////////////////////
ConstraintManager::~ConstraintManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::data::PropertySetPtr ConstraintManager::CreateNewConstraint( const std::string& constraintType )
{
    using namespace ves::xplorer::data;

    // Attempt to find constraintType in the map that was set up in the ctor.
    // If found, call the factory method of the associated ProertySet to get a
    // new set of that type.
    std::map< std::string, PropertySetPtr >::const_iterator iter =
            m_constraintTypeToSetPtrMap.find( constraintType );
    if( iter != m_constraintTypeToSetPtrMap.end() )
    {
        LOG_INFO( "Created new propertyset for constraint: " + constraintType );
        return iter->second->CreateNew();
    }
    else
    {
        return PropertySetPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintManager::ResyncFromDatabase()
{
    using namespace ves::xplorer::data;
    std::vector<std::string> ids;
    PropertySetPtr propSet;

    std::map< std::string, PropertySetPtr >::const_iterator iter =
            m_constraintTypeToSetPtrMap.begin();
    while( iter != m_constraintTypeToSetPtrMap.end() )
    {
        propSet = iter->second->CreateNew();
        if( propSet.get() )
        {
            ids = DatabaseManager::instance()->GetStringVector( propSet->GetTableName(), "uuid" );
            for( size_t index = 0; index < ids.size(); ++index )
            {
                propSet->SetUUID( ids.at( index ) );
                propSet->LoadFromDatabase();
                // The write operation causes the constraint to be added to the scene
                propSet->WriteToDatabase();
            }
        }
        ++iter;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > >
ConstraintManager::GetNameIDPairsForConstraint( const std::string& constraintType )
{
    std::vector< std::pair< std::string, std::string > > nameIDPairs;
    std::vector<std::string> ids;
    std::vector<std::string> names;
    using namespace ves::xplorer::data;

    std::map< std::string, PropertySetPtr >::const_iterator iter =
            m_constraintTypeToSetPtrMap.find( constraintType );
    if( iter != m_constraintTypeToSetPtrMap.end() )
    {
        ids = DatabaseManager::instance()->GetStringVector( iter->second->GetTableName(), "uuid" );
        names = DatabaseManager::instance()->GetStringVector( iter->second->GetTableName(), "NameTag" );

        // Pair up the names and IDs
        std::pair< std::string, std::string > tempPair;
        assert( names.size() == ids.size() );
        for( size_t index = 0; index < names.size(); ++index )
        {
            tempPair.first = names.at( index );
            tempPair.second = ids.at( index );
            nameIDPairs.push_back( tempPair );
        }

        LOG_DEBUG( "GetNameIDPairsForConstraint: For " + constraintType );
        return nameIDPairs;
    }

    LOG_WARNING( "GetNameIDPairsForConstraint: We do not have a " +
                constraintType + " constraint registered yet." );

    return nameIDPairs;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
