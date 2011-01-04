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

#include <ves/conductor/qt/VisFeatureMakerBase.h>
///Contours
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/conductor/qt/ContourFeatureMaker.h>
///Vectors
#include <ves/xplorer/data/VectorPlanePropertySet.h>
#include <ves/conductor/qt/VectorFeatureMaker.h>
///Streamlines
#include <ves/xplorer/data/StreamlinePropertySet.h>
#include <ves/conductor/qt/StreamlineFeatureMaker.h>
///Isosurfaces
#include <ves/xplorer/data/IsosurfacePropertySet.h>
#include <ves/conductor/qt/IsosurfaceFeatureMaker.h>
///Polydata
#include <ves/xplorer/data/PolydataPropertySet.h>
#include <ves/conductor/qt/PolydataFeatureMaker.h>

namespace ves
{
namespace conductor
{

#define _print( text ) std::cout << text << std::endl << std::flush

vprSingletonImp( VisFeatureManager );
////////////////////////////////////////////////////////////////////////////////
VisFeatureManager::VisFeatureManager()
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
        _print( "|\tCreating new ContourPlanePropertySet" );
        set = PropertySetPtr( new ContourPlanePropertySet() );
    }
    else if( featureName == "Vectors" )
    {
        _print( "|\tCreating new VectorPlanePropertySet" );
        set = PropertySetPtr( new VectorPlanePropertySet() );
    }
    else if( featureName == "Streamlines" )
    {
        _print( "|\tCreating new StreamlinesPropertySet" );
        set = PropertySetPtr( new StreamlinePropertySet() );
    }
    else if( featureName == "Isosurfaces" )
    {
        _print( "|\tCreating new IsoSurfacesPropertySet" );
        set = PropertySetPtr( new IsosurfacePropertySet() );
    }
    else if( featureName == "Texture-based" )
    {
        _print( "Would be creating new TextureBasedPropertySet..." );
    }
    else if( featureName == "Polydata" )
    {
        _print( "|\tCreating new PolydataPropertySet" );
        set = PropertySetPtr( new PolydataPropertySet() );
    }

    m_featureNameToTableName[ featureName ] = set->GetTableName();

    return set;
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureManager::UpdateFeature( const std::string& featureName, const std::string& UUID )
{
    using namespace ves::conductor;
    VisFeatureMakerBasePtr feature;

    if( featureName == "Contours" )
    {
        _print( "|\tUpdating ContourFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new ContourFeatureMaker() );
    }
    else if( featureName == "Vectors" )
    {
        _print( "|\tUpdating VectorFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new VectorFeatureMaker() );
    }
    else if( featureName == "Streamlines" )
    {
        _print( "|\tUpdating StreamlineFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new StreamlineFeatureMaker() );
    }
    else if( featureName == "Isosurfaces" )
    {
        _print( "|\tUpdating IsosurfaceFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new IsosurfaceFeatureMaker() );
    }
    else if( featureName == "Texture-based" )
    {
        _print( "Would be updating TextureBasedFeatureMaker..." );
    }
    else if( featureName == "Polydata" )
    {
        _print( "|\tUpdating PolydataFeatureMaker" );
        feature = VisFeatureMakerBasePtr( new PolydataFeatureMaker() );
    }

    feature->Update( UUID );
}
////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> VisFeatureManager::GetIDsForFeature( const std::string& featureName )
{
    std::vector<std::string> ids;
    using namespace ves::xplorer::data;

    std::map<std::string, std::string>::const_iterator iter = 
        m_featureNameToTableName.find( featureName );
    if( iter != m_featureNameToTableName.end() )
    {
        ids = DatabaseManager::instance()->GetStringVector( iter->second, "uuid" );
        return ids;
    }

    std::cout << "We do not have a " << featureName 
        << " vis feature registered yet." << std::endl;
    return ids;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
