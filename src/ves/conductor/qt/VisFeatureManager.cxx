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
#include <ves/conductor/qt/VisFeatureManager.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

///Contours
#include <ves/xplorer/data/ContourPlanePropertySet.h>
///Vectors
#include <ves/xplorer/data/VectorPlanePropertySet.h>
///Streamlines
#include <ves/xplorer/data/StreamlinePropertySet.h>
///Isosurfaces
#include <ves/xplorer/data/IsosurfacePropertySet.h>
///Polydata
#include <ves/xplorer/data/PolydataPropertySet.h>
///Volume viz
#include <ves/xplorer/data/VolumeVisPropertySet.h>

namespace ves
{
namespace conductor
{

vprSingletonImp( VisFeatureManager );
////////////////////////////////////////////////////////////////////////////////
VisFeatureManager::VisFeatureManager()
    :
    m_logger( Poco::Logger::get( "conductor.VisFeatureManager" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    CONNECTSIGNALS_0( "%ResyncFromDatabase", void(),
                      &VisFeatureManager::ResyncFromDatabase,
                      m_connections, any_SignalType, high_Priority );

    using namespace ves::xplorer::data;

    // ADD ANY NEW VIZ FEATURE TYPES HERE OR THE UI WILL NOT BE ABLE TO DISPLAY
    // THEM!
    m_featureTypeToSetPtrMap["Contours"] = propertystore::PropertySetPtr( new ContourPlanePropertySet() );
    m_featureTypeToSetPtrMap["Vectors"] = propertystore::PropertySetPtr( new VectorPlanePropertySet() );
    m_featureTypeToSetPtrMap["Streamlines"] = propertystore::PropertySetPtr( new StreamlinePropertySet() );
    m_featureTypeToSetPtrMap["Isosurfaces"] = propertystore::PropertySetPtr( new IsosurfacePropertySet() );
    m_featureTypeToSetPtrMap["Polydata"] = propertystore::PropertySetPtr( new PolydataPropertySet() );
    m_featureTypeToSetPtrMap["Texture-based"] = propertystore::PropertySetPtr( new VolumeVisPropertySet() );
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureManager::~VisFeatureManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
propertystore::PropertySetPtr VisFeatureManager::CreateNewFeature( const std::string& featureType )
{
    using namespace ves::xplorer::data;

    // Attempt to find featureType in the map that was set up in the ctor.
    // If found, call the factory method of the associated ProertySet to get a
    // new set of that type.
    std::map< std::string, propertystore::PropertySetPtr >::const_iterator iter =
        m_featureTypeToSetPtrMap.find( featureType );
    if( iter != m_featureTypeToSetPtrMap.end() )
    {
        return iter->second->CreateNew();
    }
    else
    {
        return propertystore::PropertySetPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureManager::ResyncFromDatabase()
{
    using namespace ves::xplorer::data;
    std::vector<std::string> ids;
    propertystore::PropertySetPtr propSet;

    std::map< std::string, propertystore::PropertySetPtr >::const_iterator iter =
        m_featureTypeToSetPtrMap.begin();
    while( iter != m_featureTypeToSetPtrMap.end() )
    {
        propSet = iter->second->CreateNew();
        if( propSet.get() )
        {
            ids = DatabaseManager::instance()->GetStringVector( propSet->GetTypeName(), "uuid" );
            for( size_t index = 0; index < ids.size(); ++index )
            {
                propSet->SetUUID( ids.at( index ) );
                propSet->Load();
                // The write operation causes the feature to be added to the scene
                propSet->Save();
            }
        }
        ++iter;
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > >
VisFeatureManager::GetNameIDPairsForFeature( const std::string& featureType )
{
    std::vector< std::pair< std::string, std::string > > nameIDPairs;
    std::vector<std::string> ids;
    std::vector<std::string> names;
    using namespace ves::xplorer::data;

    std::map< std::string, propertystore::PropertySetPtr >::const_iterator iter =
        m_featureTypeToSetPtrMap.find( featureType );
    if( iter != m_featureTypeToSetPtrMap.end() )
    {
        ids = DatabaseManager::instance()->GetStringVector( iter->second->GetTypeName(), "uuid" );
        names = DatabaseManager::instance()->GetStringVector( iter->second->GetTypeName(), "NameTag" );

        // Pair up the names and IDs
        std::pair< std::string, std::string > tempPair;
        assert( names.size() == ids.size() );
        for( size_t index = 0; index < names.size(); ++index )
        {
            tempPair.first = names.at( index );
            tempPair.second = ids.at( index );
            nameIDPairs.push_back( tempPair );
        }

        LOG_DEBUG( "GetNameIDPairsForFeature: For " + featureType );
        return nameIDPairs;
    }

    LOG_WARNING( "GetNameIDPairsForFeature: We do not have a " +
                 featureType + " vis feature registered yet." );

    return nameIDPairs;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
