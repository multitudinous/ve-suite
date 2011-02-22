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
#include <ves/conductor/qt/VisFeatureManager.h>
#include <ves/xplorer/data/DatabaseManager.h>

namespace ves
{
namespace conductor
{

vprSingletonImp( VisFeatureManager );
////////////////////////////////////////////////////////////////////////////////
VisFeatureManager::VisFeatureManager()
    :
    m_logger( Poco::Logger::get("conductor.VisFeatureManager") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
VisFeatureManager::~VisFeatureManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::data::PropertySetPtr VisFeatureManager::CreateNewFeature( const std::string& featureName )
{
    using namespace ves::xplorer::data;

    PropertySetPtr set;

    if( featureName == "Contours" )
    {
        LOG_INFO( "CreateNewFeature: Creating new ContourPlanePropertySet" );
        set = PropertySetPtr( new ContourPlanePropertySet() );
    }
    else if( featureName == "Vectors" )
    {
        LOG_INFO( "CreateNewFeature: Creating new VectorPlanePropertySet" );
        set = PropertySetPtr( new VectorPlanePropertySet() );
    }
    else if( featureName == "Streamlines" )
    {
        LOG_INFO( "CreateNewFeature: Creating new StreamlinesPropertySet" );
        set = PropertySetPtr( new StreamlinePropertySet() );
    }
    else if( featureName == "Isosurfaces" )
    {
        LOG_INFO( "CreateNewFeature: Creating new IsoSurfacesPropertySet" );
        set = PropertySetPtr( new IsosurfacePropertySet() );
    }
    else if( featureName == "Texture-based" )
    {
        LOG_INFO( "CreateNewFeature: Creating new TextureBasedPropertySet" );
    }
    else if( featureName == "Polydata" )
    {
        LOG_INFO( "CreateNewFeature: Creating new PolydataPropertySet" );
        set = PropertySetPtr( new PolydataPropertySet() );
    }

    m_featureNameToTableName[ featureName ] = set->GetTableName();

    return set;
}
////////////////////////////////////////////////////////////////////////////////
/*void VisFeatureManager::UpdateFeature( const std::string& featureName, const std::string& UUID )
{
    using namespace ves::conductor;
    VisFeatureMakerBasePtr feature;

    if( featureName == "Contours" )
    {
        LOG_INFO( "UpdateFeature: Updating ContourFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new ContourFeatureMaker() );
    }
    else if( featureName == "Vectors" )
    {
        LOG_INFO( "UpdateFeature: Updating VectorFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new VectorFeatureMaker() );
    }
    else if( featureName == "Streamlines" )
    {
        LOG_INFO( "UpdateFeature: Updating StreamlineFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new StreamlineFeatureMaker() );
    }
    else if( featureName == "Isosurfaces" )
    {
        LOG_INFO( "UpdateFeature: Updating IsosurfaceFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new IsosurfaceFeatureMaker() );
    }
    else if( featureName == "Texture-based" )
    {
        LOG_INFO( "UpdateFeature: Updating TextureBasedFeatureMaker" );
    }
    else if( featureName == "Polydata" )
    {
        LOG_INFO( "UpdateFeature: Updating PolydataFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new PolydataFeatureMaker() );
    }

    feature->Update( UUID );
}*/
////////////////////////////////////////////////////////////////////////////////
std::vector< std::pair< std::string, std::string > >
VisFeatureManager::GetNameIDPairsForFeature( const std::string& featureName )
{
    std::vector< std::pair< std::string, std::string > > nameIDPairs;
    std::vector<std::string> ids;
    std::vector<std::string> names;
    using namespace ves::xplorer::data;

    std::map<std::string, std::string>::const_iterator iter = 
        m_featureNameToTableName.find( featureName );
    if( iter != m_featureNameToTableName.end() )
    {
        ids = DatabaseManager::instance()->GetStringVector( iter->second, "uuid" );
        names = DatabaseManager::instance()->GetStringVector( iter->second, "NameTag" );

        // Pair up the names and IDs
        std::pair< std::string, std::string > tempPair;
        assert( names.size() == ids.size() );
        for( size_t index = 0; index < names.size(); ++index )
        {
            tempPair.first = names.at( index );
            tempPair.second = ids.at( index );
            nameIDPairs.push_back( tempPair );
        }

        LOG_DEBUG( "GetNameIDPairsForFeature: For " + featureName );
        return nameIDPairs;
    }

    LOG_WARNING( "GetNameIDPairsForFeature: We do not have a " + 
                featureName + " vis feature registered yet." );

    return nameIDPairs;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
