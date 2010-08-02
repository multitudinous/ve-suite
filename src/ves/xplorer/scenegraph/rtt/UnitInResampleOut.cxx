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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/rtt/UnitInResampleOut.h>

using namespace ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
UnitInResampleOut::UnitInResampleOut()
    :
    UnitInOut(),
    mWidthFactor( 1.0 ),
    mHeightFactor( 1.0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitInResampleOut::UnitInResampleOut(
    const UnitInResampleOut& unitInResampleOut,
    const osg::CopyOp& copyop )
    :
    UnitInOut( unitInResampleOut, copyop ),
    mWidthFactor( unitInResampleOut.mWidthFactor ),
    mHeightFactor( unitInResampleOut.mHeightFactor )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitInResampleOut::~UnitInResampleOut()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInResampleOut::Initialize()
{
    //Do initialize as usual
    UnitInOut::Initialize();

    //If we have to reset the resampling factor
    float width = static_cast< float >( mViewport->width() );
    float height = static_cast< float >( mViewport->height() );

    mViewport->width() =
        static_cast< osg::Viewport::value_type >( width * mWidthFactor );
    mViewport->height() =
        static_cast< osg::Viewport::value_type >( height * mHeightFactor );

    //Notice that we changed the viewport
    NoticeChangeViewport();
}
////////////////////////////////////////////////////////////////////////////////
float UnitInResampleOut::GetFactorX() const
{
    return mWidthFactor;
}
////////////////////////////////////////////////////////////////////////////////
float UnitInResampleOut::GetFactorY() const
{
    return mHeightFactor;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInResampleOut::SetFactorX( float xFactor )
{
    mWidthFactor = xFactor;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInResampleOut::SetFactorY( float yFactor )
{
    mHeightFactor = yFactor;
}
////////////////////////////////////////////////////////////////////////////////
void UnitInResampleOut::SetInputTexturesFromParents()
{
     //Scan all parents and look for units
     for( unsigned int i = 0; i < getNumParents(); ++i )
     {
         rtt::Unit* unit = dynamic_cast< rtt::Unit* >( getParent( i ) );
         if( unit )
         {
             //Add each found texture as input
             const Unit::TextureMap& textureMap =
                unit->GetOutputTextureMap();
             Unit::TextureMap::const_iterator itr = textureMap.begin();
             for( itr; itr != textureMap.end(); ++itr )
             {
                 osg::Texture* texture = itr->get();
                 if( texture )
                 {
                     mInputTextures.push_back( texture );
                 }
                 else
                 {
                     osg::notify( osg::WARN )
                        << "rtt::UnitInResampleOut::SetInputTexturesFromParents(): "
                        << unit->getName()
                        << " has invalid output texture!"
                        << std::endl;
                 }
             }
         }
     }
}
////////////////////////////////////////////////////////////////////////////////
