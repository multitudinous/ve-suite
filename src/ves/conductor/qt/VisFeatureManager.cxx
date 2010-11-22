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
#include <ves/conductor/qt/VisFeatureManager.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <ves/conductor/qt/VisFeatureMakerBase.h>
///Contours
#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/conductor/qt/ContourFeatureMaker.h>
///Vectors
#include <ves/xplorer/data/VectorPlanePropertySet.h>
#include <ves/conductor/qt/VectorFeatureMaker.h>

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
ves::xplorer::data::PropertySet* VisFeatureManager::CreateNewFeature( const std::string& featureName )
{
    using namespace ves::xplorer::data;

    PropertySet* set = 0;

    if( featureName == "Contours" )
    {
        _print( "Creating new ContourPlanePropertySet" );
        set = new ContourPlanePropertySet();
    }
    else if( featureName == "Vectors" )
    {
        _print( "Creating new VectorPlanePropertySet" );
        set = new VectorPlanePropertySet();
    }
    else if( featureName == "Streamlines" )
    {
        _print( "Would be creating new StreamlinesPropertySet..." );
    }
    else if( featureName == "Isosurfaces" )
    {
        _print( "Would be creating new IsoSurfacesPropertySet..." );
    }
    else if( featureName == "Texture-based" )
    {
        _print( "Would be creating new TextureBasedPropertySet..." );
    }
    else if( featureName == "Polydata" )
    {
        _print( "Would be creating new PolydataPropertySet..." );
    }

    return set;
}
////////////////////////////////////////////////////////////////////////////////
void VisFeatureManager::UpdateFeature( const std::string& featureName, unsigned int ID )
{
    VisFeatureMakerBase* feature = 0;

    if( featureName == "Contours" )
    {
        _print( "Updating ContourFeatureMaker" );
        feature = new ContourFeatureMaker();
    }
    else if( featureName == "Vectors" )
    {
        _print( "Updating VectorFeatureMaker" );
        feature = new VectorFeatureMaker();
    }
    else if( featureName == "Streamlines" )
    {
        _print( "Would be updating StreamlinesFeatureMaker..." );
    }
    else if( featureName == "Isosurfaces" )
    {
        _print( "Would be updating IsoSurfacesFeatureMaker..." );
    }
    else if( featureName == "Texture-based" )
    {
        _print( "Would be updating TextureBasedFeatureMaker..." );
    }
    else if( featureName == "Polydata" )
    {
        _print( "Would be updating PolydataFeatureMaker..." );
    }

    feature->Update( ID );
    delete feature;
}
////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> VisFeatureManager::GetIDsForFeature( const std::string& featureName )
{
    std::vector<std::string> ids;
    using namespace ves::xplorer::data;
    PropertySet* set = CreateNewFeature( featureName );
    if( set )
    {
        ids = DatabaseManager::instance()->GetStringVector( set->GetTableName(), "id" );
    }
    delete set;
    return ids;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace conductor
} // namespace ves
