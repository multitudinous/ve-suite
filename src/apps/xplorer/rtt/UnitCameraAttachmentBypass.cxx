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
#include <apps/xplorer/rtt/UnitCameraAttachmentBypass.h>
#include <apps/xplorer/rtt/Processor.h>

// --- OSG Includes --- //

// --- C/C++ Includes --- //

using namespace ves::xplorer::rtt;

////////////////////////////////////////////////////////////////////////////////
UnitCameraAttachmentBypass::UnitCameraAttachmentBypass()
    :
    Unit(),
    mBufferComponent( osg::Camera::COLOR_BUFFER )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitCameraAttachmentBypass::UnitCameraAttachmentBypass(
    const UnitCameraAttachmentBypass& unitCameraAttachmentBypass,
    const osg::CopyOp& copyop )
    :
    Unit( unitCameraAttachmentBypass, copyop ),
    mBufferComponent( unitCameraAttachmentBypass.mBufferComponent )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
UnitCameraAttachmentBypass::~UnitCameraAttachmentBypass()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void UnitCameraAttachmentBypass::SetBufferComponent(
    osg::Camera::BufferComponent bufferComponent )
{
    mBufferComponent = bufferComponent;
}
////////////////////////////////////////////////////////////////////////////////
void UnitCameraAttachmentBypass::Initialize()
{
    Unit::Initialize();
}
////////////////////////////////////////////////////////////////////////////////
void UnitCameraAttachmentBypass::SetInputTexturesFromParents()
{
    //Scan all parents and look for the processor
    rtt::Processor* processor( NULL );
    for( unsigned int i = 0; i < getNumParents(); ++i )
    {
        processor = dynamic_cast< rtt::Processor* >( getParent( i ) );
        if( processor )
        {
            //Get buffer attachment
            osg::Camera::BufferAttachmentMap& bufferAttachmentMap =
                processor->GetCamera()->getBufferAttachmentMap();
            osg::Texture* input =
                bufferAttachmentMap[ mBufferComponent ]._texture.get();

            //If no attachment then warning
            if( !input )
            {
                osg::notify( osg::WARN )
                    << "rtt::UnitCameraAttachmentBypass::"
                    << "SetInputTexturesFromParents(): "
                    << "Processor's camera has no specified buffer attachment!"
                    << std::endl;
            }
            else
            {
                //Set the input textures
                mInputTextures.clear();
                mInputTextures[ 0 ] = input;
                mOutputTextures = mInputTextures;
            }
        }
        else
        {
            osg::notify( osg::WARN )
                << "rtt::UnitCameraAttachmentBypass::"
                << "SetInputTexturesFromParents(): "
                << "unit is not a direct child of processor!"
                << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
